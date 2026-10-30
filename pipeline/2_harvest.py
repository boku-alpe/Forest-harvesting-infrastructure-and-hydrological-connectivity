# -*- coding: utf-8 -*-
"""
Stage 2 - HARVEST (robust)
Polls the batch, retrieves results, parses each JSON output, and writes R-ready CSVs
that join to aquasilva.csv on the Zotero key:
  - studies.csv / infrastructure.csv / connectivity.csv
  - harvest_errors.txt   (errored / expired / salvaged / hard-fail / review-flag)
  - failures/<cid>.txt    (full raw output of any request that failed to parse)

Robust to truncated/malformed JSON: recovers every well-formed connectivity element
even if the array was cut off or one element is broken. Re-running on the SAME
batch_id costs nothing (results persist server-side ~29 days).

Run:  python 2_harvest.py
"""
import csv, json, os, re, sys, time

try:
    import anthropic
except ImportError:
    sys.exit("Missing dependency: pip install anthropic")

BATCH_ID_FILE = "batch_id.txt"
FAIL_DIR = "failures"
POLL_SECONDS = 60

STUDIES_COLS = ["zotero_key", "citation_short", "study_design", "measurement_method",
    "temporal_scale", "study_duration_value", "study_duration_unit", "catchment_area_ha",
    "catchment_area_detail", "climate_zone", "land_use_context", "locator_methods",
    "confidence", "extraction_flag", "extractor"]
INFRA_COLS = ["infra_id", "zotero_key", "infrastructure_type", "density_value",
    "density_unit", "age_or_since", "design_descriptor", "condition", "network_topology",
    "locator", "confidence", "extraction_flag"]
CONN_COLS = ["conn_id", "zotero_key", "review_flag", "metric_name", "metric_family",
    "connectivity_type", "operationalization_basis", "direction_of_use", "software_tool",
    "evidence_quote", "locator", "confidence", "extraction_flag", "extractor"]
EFFECTS_COLS = ["effect_id", "zotero_key", "infrastructure_type", "spatial_scale",
    "outcome_variable", "connectivity_evidence_basis", "connectivity_metric_name",
    "direction", "magnitude_qual", "magnitude_value", "magnitude_unit_or_basis",
    "moderator_class", "moderator_detail", "evidence_quote", "locator", "confidence",
    "extraction_flag", "synthesis_role", "extractor"]
MIT_COLS = ["mit_id", "zotero_key", "measure_type", "target_problem", "effectiveness",
    "effectiveness_evidence", "effectiveness_value", "effectiveness_unit_or_basis",
    "design_spec_reported", "design_spec_detail", "context_conditions", "evidence_quote",
    "locator", "confidence", "extraction_flag", "extractor"]
REVIEWS_COLS = ["zotero_key", "citation_short", "review_scope", "synthesis_statement",
    "identified_gaps", "consensus_claim", "disagreement_claim", "classification_scheme",
    "evidence_quote", "locator", "confidence", "extractor"]


def strip_fences(s):
    s = (s or "").strip()
    s = re.sub(r"^```(?:json)?\s*", "", s)
    s = re.sub(r"\s*```$", "", s)
    return s.strip()


def get_text(result):
    for block in result.result.message.content:
        if getattr(block, "type", None) == "text":
            return block.text
    return ""


def _objects_in_array(arr_text):
    """Return complete top-level {...} object substrings from an array body."""
    out, depth, start, in_str, esc = [], 0, None, False, False
    for i, ch in enumerate(arr_text):
        if in_str:
            if esc: esc = False
            elif ch == "\\": esc = True
            elif ch == '"': in_str = False
            continue
        if ch == '"':
            in_str = True
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


def salvage_connectivity(raw):
    """Recover (key, review_flag, [elements]) from broken/truncated Skill-B JSON."""
    key_m = re.search(r'"zotero_key"\s*:\s*"([^"]+)"', raw)
    flag_m = re.search(r'"review_flag"\s*:\s*"([^"]*)"', raw)
    key = key_m.group(1) if key_m else None
    flag = flag_m.group(1) if flag_m else ""
    elems, idx = [], raw.find('"connectivity"')
    if idx != -1:
        lb = raw.find('[', idx)
        if lb != -1:
            for block in _objects_in_array(raw[lb + 1:]):
                try:
                    elems.append(json.loads(block))
                except Exception:
                    pass
    return key, flag, elems


def salvage_effects(raw):
    """Recover (key, [elements]) from broken/truncated Skill-C JSON."""
    key_m = re.search(r'"zotero_key"\s*:\s*"([^"]+)"', raw)
    key = key_m.group(1) if key_m else None
    elems, idx = [], raw.find('"effects"')
    if idx != -1:
        lb = raw.find('[', idx)
        if lb != -1:
            for block in _objects_in_array(raw[lb + 1:]):
                try:
                    elems.append(json.loads(block))
                except Exception:
                    pass
    return key, elems


def salvage_mitigation(raw):
    """Recover (key, [elements]) from broken/truncated Skill-D JSON."""
    key_m = re.search(r'"zotero_key"\s*:\s*"([^"]+)"', raw)
    key = key_m.group(1) if key_m else None
    elems, idx = [], raw.find('"mitigation"')
    if idx != -1:
        lb = raw.find('[', idx)
        if lb != -1:
            for block in _objects_in_array(raw[lb + 1:]):
                try:
                    elems.append(json.loads(block))
                except Exception:
                    pass
    return key, elems


def salvage_object(raw):
    """Recover a single JSON object (Skill E) from surrounding noise/fences."""
    a, b = raw.find("{"), raw.rfind("}")
    if a != -1 and b != -1 and b > a:
        try:
            return json.loads(raw[a:b + 1])
        except Exception:
            return None
    return None


def main():
    if not os.path.isfile(BATCH_ID_FILE):
        sys.exit("No batch_id.txt found. Run 1_submit.py first.")
    batch_id = open(BATCH_ID_FILE).read().strip()
    client = anthropic.Anthropic()
    os.makedirs(FAIL_DIR, exist_ok=True)

    while True:
        b = client.messages.batches.retrieve(batch_id)
        print(f"status={b.processing_status}  counts={b.request_counts}")
        if b.processing_status == "ended":
            break
        time.sleep(POLL_SECONDS)

    studies_rows, infra_rows, conn_rows, errors = [], [], [], []
    effect_rows, mit_rows, review_rows = [], [], []
    infra_counter, conn_counter, effect_counter, mit_counter = {}, {}, {}, {}

    def add_conn(k, rflag, elems):
        for el in elems:
            n = conn_counter.get(k, 0) + 1
            conn_counter[k] = n
            row = {c: el.get(c, "") for c in CONN_COLS}
            row["conn_id"] = f"{k}_C{n:02d}"
            row["zotero_key"] = k
            row["review_flag"] = rflag
            conn_rows.append(row)

    def add_eff(k, elems):
        for el in elems:
            n = effect_counter.get(k, 0) + 1
            effect_counter[k] = n
            row = {c: el.get(c, "") for c in EFFECTS_COLS}
            row["effect_id"] = f"{k}_E{n:02d}"
            row["zotero_key"] = k
            effect_rows.append(row)

    def add_mit(k, elems):
        for el in elems:
            n = mit_counter.get(k, 0) + 1
            mit_counter[k] = n
            row = {c: el.get(c, "") for c in MIT_COLS}
            row["mit_id"] = f"{k}_M{n:02d}"
            row["zotero_key"] = k
            mit_rows.append(row)

    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id
        key, _, skill = cid.partition("__")
        if result.result.type != "succeeded":
            errors.append(f"{cid}\t{result.result.type}\t{getattr(result.result, 'error', '')}")
            continue
        raw = strip_fences(get_text(result))

        try:
            data = json.loads(raw)
        except Exception as e:
            with open(os.path.join(FAIL_DIR, f"{cid}.txt"), "w", encoding="utf-8") as ff:
                ff.write(raw)
            if skill == "B":
                k, flag, elems = salvage_connectivity(raw)
                if elems:
                    add_conn(k or key, flag, elems)
                    errors.append(f"{cid}\tSALVAGED\t{len(elems)} metrics recovered (failures/{cid}.txt)")
                else:
                    errors.append(f"{cid}\tHARD-FAIL\t{type(e).__name__}\traw in failures/{cid}.txt")
            elif skill == "C":
                k, elems = salvage_effects(raw)
                if elems:
                    add_eff(k or key, elems)
                    errors.append(f"{cid}\tSALVAGED\t{len(elems)} effects recovered (failures/{cid}.txt)")
                else:
                    errors.append(f"{cid}\tHARD-FAIL\t{type(e).__name__}\traw in failures/{cid}.txt")
            elif skill == "D":
                k, elems = salvage_mitigation(raw)
                if elems:
                    add_mit(k or key, elems)
                    errors.append(f"{cid}\tSALVAGED\t{len(elems)} measures recovered (failures/{cid}.txt)")
                else:
                    errors.append(f"{cid}\tHARD-FAIL\t{type(e).__name__}\traw in failures/{cid}.txt")
            elif skill == "E":
                obj = salvage_object(raw)
                if obj:
                    obj.setdefault("zotero_key", key)
                    review_rows.append({c: obj.get(c, "") for c in REVIEWS_COLS})
                    errors.append(f"{cid}\tSALVAGED\treview object recovered (failures/{cid}.txt)")
                else:
                    errors.append(f"{cid}\tHARD-FAIL\t{type(e).__name__}\traw in failures/{cid}.txt")
            else:
                errors.append(f"{cid}\tJSON-PARSE-FAIL\t{type(e).__name__}\traw in failures/{cid}.txt")
            continue

        if skill == "A":
            st = data.get("studies", {})
            st.setdefault("zotero_key", key)
            studies_rows.append({c: st.get(c, "") for c in STUDIES_COLS})
            for el in data.get("infrastructure", []):
                n = infra_counter.get(key, 0) + 1
                infra_counter[key] = n
                row = {c: el.get(c, "") for c in INFRA_COLS}
                row["infra_id"] = f"{key}_I{n:02d}"
                row["zotero_key"] = key
                infra_rows.append(row)
        elif skill == "B":
            rflag = data.get("review_flag", "")
            add_conn(key, rflag, data.get("connectivity", []))
            if rflag:
                errors.append(f"{cid}\tREVIEW-FLAG\t{rflag}")
        elif skill == "C":
            add_eff(key, data.get("effects", []))
            if not data.get("effects"):
                errors.append(f"{cid}\tNO-EFFECTS\tempty effects array")
        elif skill == "D":
            add_mit(key, data.get("mitigation", []))
            if not data.get("mitigation"):
                errors.append(f"{cid}\tNO-MITIGATION\tempty mitigation array")
        elif skill == "E":
            data.setdefault("zotero_key", key)
            review_rows.append({c: data.get(c, "") for c in REVIEWS_COLS})

    def write_csv(path, cols, rows):
        with open(path, "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=cols)
            w.writeheader()
            for r in rows:
                w.writerow({c: r.get(c, "") for c in cols})

    def maybe_write(path, cols, rows):
        if rows:
            write_csv(path, cols, rows)
            print(f"wrote {path}: {len(rows)} rows")
        else:
            print(f"skipped {path} (no rows this run; existing file left untouched)")

    maybe_write("studies.csv", STUDIES_COLS, studies_rows)
    maybe_write("infrastructure.csv", INFRA_COLS, infra_rows)
    maybe_write("connectivity.csv", CONN_COLS, conn_rows)
    maybe_write("effects.csv", EFFECTS_COLS, effect_rows)
    maybe_write("mitigation.csv", MIT_COLS, mit_rows)
    maybe_write("reviews.csv", REVIEWS_COLS, review_rows)
    with open("harvest_errors.txt", "w", encoding="utf-8") as f:
        f.write("\n".join(errors) if errors else "none")

    if conn_rows:
        print(f"connectivity papers covered: {len({r['zotero_key'] for r in conn_rows})}")
    if effect_rows:
        print(f"effects: {len(effect_rows)} rows across "
              f"{len({r['zotero_key'] for r in effect_rows})} papers")
    if mit_rows:
        print(f"mitigation: {len(mit_rows)} measures across "
              f"{len({r['zotero_key'] for r in mit_rows})} papers")
    if review_rows:
        print(f"reviews: {len(review_rows)} rows")
    print(f"errors/flags: {len(errors)} (see harvest_errors.txt; raw of fails in failures/)")
    print("Join to aquasilva.csv on zotero_key (column 'Key'). RQ5c stays an R computation.")


if __name__ == "__main__":
    main()
