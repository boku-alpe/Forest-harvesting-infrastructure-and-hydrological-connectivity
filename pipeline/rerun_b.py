# -*- coding: utf-8 -*-
"""
rerun_b.py  -  Re-run Skill B for specific keys (synchronous) and MERGE the result
into connectivity.csv (existing rows for those keys are replaced, the rest untouched).
Prints stop_reason + output length so you can see WHY a request came back empty.

Edit KEYS below.  Run:  python rerun_b.py
"""
import csv, json, os, re, sys

try:
    import anthropic
except ImportError:
    sys.exit("Missing dependency: pip install anthropic")

from prompts import SKILL_B_SYSTEM

KEYS = ["48EQ32MR"]                 # keys to re-run
MODEL_B = "claude-sonnet-4-6"
MAX_TOKENS_B = 6000
TEXT_DIR = "text"
CONN_CSV = "connectivity.csv"

CONN_COLS = ["conn_id", "zotero_key", "review_flag", "metric_name", "metric_family",
    "connectivity_type", "operationalization_basis", "direction_of_use", "software_tool",
    "evidence_quote", "locator", "confidence", "extraction_flag", "extractor"]


def strip_fences(s):
    s = (s or "").strip()
    s = re.sub(r"^```(?:json)?\s*", "", s)
    s = re.sub(r"\s*```$", "", s)
    return s.strip()


def _objects_in_array(arr_text):
    out, depth, start, in_str, esc = [], 0, None, False, False
    for i, ch in enumerate(arr_text):
        if in_str:
            if esc: esc = False
            elif ch == "\\": esc = True
            elif ch == '"': in_str = False
            continue
        if ch == '"': in_str = True
        elif ch == '{':
            if depth == 0: start = i
            depth += 1
        elif ch == '}':
            depth -= 1
            if depth == 0 and start is not None:
                out.append(arr_text[start:i + 1]); start = None
        elif ch == ']' and depth == 0:
            break
    return out


def parse_b(raw):
    raw = strip_fences(raw)
    try:
        d = json.loads(raw)
        return d.get("review_flag", ""), d.get("connectivity", [])
    except Exception:
        flag_m = re.search(r'"review_flag"\s*:\s*"([^"]*)"', raw)
        flag = flag_m.group(1) if flag_m else ""
        elems, idx = [], raw.find('"connectivity"')
        if idx != -1:
            lb = raw.find('[', idx)
            if lb != -1:
                for block in _objects_in_array(raw[lb + 1:]):
                    try: elems.append(json.loads(block))
                    except Exception: pass
        return flag, elems


def main():
    client = anthropic.Anthropic()
    new_rows = []
    for key in KEYS:
        tp = os.path.join(TEXT_DIR, f"{key}.txt")
        if not os.path.isfile(tp):
            print(f"{key}: NO text file ({tp}) -- run 0_prepare first."); continue
        text = open(tp, encoding="utf-8").read()
        print(f"{key}: input text chars = {len(text)}")
        msg = client.messages.create(
            model=MODEL_B, max_tokens=MAX_TOKENS_B, system=SKILL_B_SYSTEM,
            messages=[{"role": "user", "content": f"Zotero key: {key}\n\nFULL TEXT:\n{text}"}],
        )
        raw = "".join(b.text for b in msg.content if getattr(b, "type", None) == "text")
        print(f"{key}: stop_reason={msg.stop_reason}, output chars={len(raw)}")
        if not raw.strip():
            print(f"  STILL EMPTY -> the model returned no text. Inspect text/{key}.txt "
                  f"(garbled/OCR?) or the paper itself."); continue
        flag, elems = parse_b(raw)
        if flag:
            print(f"  review_flag={flag}")
        for n, el in enumerate(elems, 1):
            row = {c: el.get(c, "") for c in CONN_COLS}
            row["conn_id"] = f"{key}_C{n:02d}"
            row["zotero_key"] = key
            row["review_flag"] = flag
            new_rows.append(row)
        print(f"  recovered {len(elems)} metrics")

    # merge: drop existing rows for KEYS, keep the rest, append new
    existing = []
    if os.path.isfile(CONN_CSV):
        with open(CONN_CSV, encoding="utf-8") as f:
            existing = [r for r in csv.DictReader(f) if r["zotero_key"] not in KEYS]
    allrows = existing + new_rows
    with open(CONN_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CONN_COLS)
        w.writeheader()
        for r in allrows:
            w.writerow({c: r.get(c, "") for c in CONN_COLS})
    print(f"\nconnectivity.csv now {len(allrows)} rows, "
          f"{len({r['zotero_key'] for r in allrows})} papers.")


if __name__ == "__main__":
    main()
