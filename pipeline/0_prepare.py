# -*- coding: utf-8 -*-
"""
Stage 0 - PREPARE
Reads aquasilva.csv, selects all 006-Include-Final records, extracts each PDF's full
text, and writes:
  - manifest.json      : list of {key, doi, citation, kind, tags002, pdf_path,
                         text_chars, status}   (kind = "primary" | "review")
  - text/<key>.txt     : extracted full text per record (primary + reviews)
  - prepare_report.txt : what worked / what needs OCR or a path fix

Run:  python 0_prepare.py
Prereq:  pip install pypdf
"""
import csv, json, os, re, sys

CSV_PATH = "aquasilva.csv"
OUT_DIR = "."
TEXT_DIR = os.path.join(OUT_DIR, "text")
MAX_CHARS = 280_000

try:
    from pypdf import PdfReader
except ImportError:
    sys.exit("Missing dependency: pip install pypdf")

# Some PDFs lack a usable ToUnicode map and pypdf emits glyph codes like
# "/C84/C104/C101" instead of characters. Each "/Cnnn" decodes to chr(nnn).
_GLYPH = re.compile(r"/C(\d+)")


def decode_glyphs(text):
    if len(_GLYPH.findall(text)) > 50:
        def _sub(m):
            n = int(m.group(1))
            return chr(n) if 0 <= n < 0x110000 else ""
        return _GLYPH.sub(_sub, text)
    return text


def tag_list(row, prefix=None):
    raw = (row.get("Manual Tags") or "")
    tags = [t.strip() for t in raw.split(";") if t.strip()]
    return [t for t in tags if (prefix is None or t.startswith(prefix))]


def first_author_year(row):
    author = (row.get("Author") or "").split(";")[0]
    sur = author.split(",")[0].strip() or "Unknown"
    yr = (row.get("Publication Year") or "").split(".")[0]
    return f"{sur} et al. {yr}".strip()


def pick_pdf_path(row):
    parts = [p.strip() for p in (row.get("File Attachments") or "").split(";") if p.strip()]
    pdfs = [p for p in parts if p.lower().endswith(".pdf")]
    if pdfs:
        return pdfs[0]
    return parts[0] if parts else ""


def extract_text(pdf_path):
    if not pdf_path or not os.path.isfile(pdf_path):
        return "", "pdf-not-found"
    try:
        reader = PdfReader(pdf_path)
        text = "\n".join((page.extract_text() or "") for page in reader.pages).strip()
    except Exception as e:
        return "", f"pdf-error:{type(e).__name__}"
    text = decode_glyphs(text)
    if len(text) < 200:
        return text, "insufficient-text-maybe-needs-OCR"
    truncated = ""
    if len(text) > MAX_CHARS:
        text = text[:MAX_CHARS]
        truncated = "-truncated"
    return text, "ok" + truncated


def main():
    os.makedirs(TEXT_DIR, exist_ok=True)
    with open(CSV_PATH, encoding="utf-8-sig") as f:
        rows = list(csv.DictReader(f))

    final = [r for r in rows if "006-Include-Final" in tag_list(r, "006-")]
    tagged = [(r, "review" if "003-Review" in tag_list(r, "003-") else "primary") for r in final]
    n_rev = sum(1 for _, k in tagged if k == "review")
    report = [f"Final-Include: {len(final)} | Primary: {len(final) - n_rev} | Reviews: {n_rev}"]

    manifest = []
    for r, kind in tagged:
        key = r["Key"]
        pdf_path = pick_pdf_path(r)
        try:
            text, status = extract_text(pdf_path)
        except Exception as e:
            text, status = "", f"prepare-error:{type(e).__name__}"
        if text:
            with open(os.path.join(TEXT_DIR, f"{key}.txt"), "w", encoding="utf-8") as t:
                t.write(text)
        manifest.append({
            "key": key,
            "doi": (r.get("DOI") or "").strip(),
            "citation": first_author_year(r),
            "kind": kind,
            "tags002": tag_list(r, "002-"),
            "pdf_path": pdf_path,
            "text_chars": len(text),
            "status": status,
        })
        if status != "ok":
            report.append(f"  [{status}] ({kind}) {key}  {first_author_year(r)}  <- {pdf_path}")

    with open(os.path.join(OUT_DIR, "manifest.json"), "w", encoding="utf-8") as m:
        json.dump(manifest, m, indent=2, ensure_ascii=False)

    ok = sum(1 for x in manifest if x["status"].startswith("ok"))
    report.insert(1, f"Text extracted OK: {ok}/{len(manifest)}")
    with open(os.path.join(OUT_DIR, "prepare_report.txt"), "w", encoding="utf-8") as rep:
        rep.write("\n".join(report))
    print("\n".join(report))
    print(f"\nWrote manifest.json ({len(manifest)} records: primary + reviews) and text/.")
    print("Review prepare_report.txt for any PDFs needing OCR or a path fix BEFORE submitting.")


if __name__ == "__main__":
    main()
