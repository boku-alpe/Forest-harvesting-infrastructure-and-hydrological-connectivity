# -*- coding: utf-8 -*-
"""
rerun_c.py  -  Re-run Skill C for specific keys (synchronous) and MERGE into
effects.csv (rows for those keys replaced, rest untouched). Defaults to Opus for the
interpretively hard borderline cases. Prints stop_reason + #effects.

Edit KEYS below.  Run:  python rerun_c.py
"""
import csv, json, os, re, sys

try:
    import anthropic
except ImportError:
    sys.exit("Missing dependency: pip install anthropic")

from prompts import SKILL_C_SYSTEM

KEYS = ["V42YRN35"]                 # keys to re-run
MODEL = "claude-opus-4-8"           # Opus for borderline; or "claude-sonnet-4-6"
MAX_TOKENS = 8000
TEXT_DIR = "text"
MANIFEST = "manifest.json"
EFFECTS_CSV = "effects.csv"

EFFECTS_COLS = ["effect_id", "zotero_key", "infrastructure_type", "spatial_scale",
    "outcome_variable", "connectivity_evidence_basis", "connectivity_metric_name",
    "direction", "magnitude_qual", "magnitude_value", "magnitude_unit_or_basis",
    "moderator_class", "moderator_detail", "evidence_quote", "locator", "confidence",
    "extraction_flag", "synthesis_role", "extractor"]


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


def parse_c(raw):
    raw = strip_fences(raw)
    try:
        return json.loads(raw).get("effects", [])
    except Exception:
        elems, idx = [], raw.find('"effects"')
        if idx != -1:
            lb = raw.find('[', idx)
            if lb != -1:
                for block in _objects_in_array(raw[lb + 1:]):
                    try: elems.append(json.loads(block))
                    except Exception: pass
        return elems


def main():
    man = {x["key"]: x for x in json.load(open(MANIFEST, encoding="utf-8"))}
    client = anthropic.Anthropic()
    new_rows = []
    for key in KEYS:
        rec = man.get(key, {"tags002": [], "citation": key})
        tp = os.path.join(TEXT_DIR, f"{key}.txt")
        if not os.path.isfile(tp):
            print(f"{key}: NO text file."); continue
        text = open(tp, encoding="utf-8").read()
        tags = ", ".join(rec.get("tags002", [])) or "none"
        um = (f"002 tags for this record (each effect's infrastructure_type must be one "
              f"of these, or Network-aggregate): {tags}\n"
              f"Zotero key: {key}\nCitation: {rec.get('citation','')}\n\n"
              f"FULL TEXT:\n{text}")
        msg = client.messages.create(model=MODEL, max_tokens=MAX_TOKENS,
            system=SKILL_C_SYSTEM, messages=[{"role": "user", "content": um}])
        raw = "".join(b.text for b in msg.content if getattr(b, "type", None) == "text")
        print(f"{key}: model={MODEL}, stop_reason={msg.stop_reason}, output chars={len(raw)}")
        elems = parse_c(raw)
        print(f"  effects found: {len(elems)}")
        for n, el in enumerate(elems, 1):
            row = {c: el.get(c, "") for c in EFFECTS_COLS}
            row["effect_id"] = f"{key}_E{n:02d}"
            row["zotero_key"] = key
            new_rows.append(row)

    existing = []
    if os.path.isfile(EFFECTS_CSV):
        with open(EFFECTS_CSV, encoding="utf-8") as f:
            existing = [r for r in csv.DictReader(f) if r["zotero_key"] not in KEYS]
    allrows = existing + new_rows
    with open(EFFECTS_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=EFFECTS_COLS)
        w.writeheader()
        for r in allrows:
            w.writerow({c: r.get(c, "") for c in EFFECTS_COLS})
    print(f"\neffects.csv now {len(allrows)} rows, "
          f"{len({r['zotero_key'] for r in allrows})} papers.")


if __name__ == "__main__":
    main()
