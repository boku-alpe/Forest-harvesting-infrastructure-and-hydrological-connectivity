# -*- coding: utf-8 -*-
"""
Stage 1 - SUBMIT
Builds one Skill-A request (Haiku) and one Skill-B request (Sonnet) per primary
study, submits them as a single Message Batch (50% off), and saves the batch id.

custom_id format: "<zotero_key>__A" / "<zotero_key>__B"

Run:  python 1_submit.py
Prereq:  pip install anthropic ; set ANTHROPIC_API_KEY in your environment.
Skill C (effects) and D (mitigation) are added later as their own batches.
"""
import json, os, sys

try:
    import anthropic
except ImportError:
    sys.exit("Missing dependency: pip install anthropic")

from prompts import (SKILL_A_SYSTEM, SKILL_B_SYSTEM, SKILL_C_SYSTEM,
                     SKILL_D_SYSTEM, SKILL_E_SYSTEM)

SKILLS = ["E"]   # reviews catch-up. A/infra already done. Set ["A","E"] to redo both.

MODEL_A = "claude-sonnet-4-6"           # was Haiku; Sonnet for uniformity across sheets
MODEL_B = "claude-sonnet-4-6"           # connectivity judgment + verbatim
MODEL_C = "claude-sonnet-4-6"           # effects: interpretive
MODEL_D = "claude-sonnet-4-6"           # mitigation effectiveness + design specs
MODEL_E = "claude-sonnet-4-6"           # review synthesis (6 papers)
MAX_TOKENS_A = 3000
MAX_TOKENS_B = 6000
MAX_TOKENS_C = 8000
MAX_TOKENS_D = 6000
MAX_TOKENS_E = 2000

CSV_PATH = "aquasilva.csv"
MITIGATION_TAG = "030-Cany-MitigationControls"


def mitigation_keys():
    """Keys of the 030 mitigation subset among final-include records (Skill D scope)."""
    import csv
    keys = set()
    with open(CSV_PATH, encoding="utf-8-sig") as f:
        for r in csv.DictReader(f):
            tags = r.get("Manual Tags") or ""
            if "006-Include-Final" in tags and MITIGATION_TAG in tags:
                keys.add(r["Key"])
    return keys

MANIFEST = "manifest.json"
TEXT_DIR = "text"
BATCH_ID_FILE = "batch_id.txt"


def user_msg_A(rec, text):
    tags = ", ".join(rec["tags002"]) or "none"
    return (f"002 tags for this record: {tags}\n"
            f"Zotero key: {rec['key']}\n"
            f"Citation: {rec['citation']}\n\n"
            f"FULL TEXT:\n{text}")


def user_msg_B(rec, text):
    return (f"Zotero key: {rec['key']}\n"
            f"Citation: {rec['citation']}\n\n"
            f"FULL TEXT:\n{text}")


def user_msg_C(rec, text):
    tags = ", ".join(rec["tags002"]) or "none"
    return (f"002 tags for this record (each effect's infrastructure_type must be one "
            f"of these, or Network-aggregate): {tags}\n"
            f"Zotero key: {rec['key']}\n"
            f"Citation: {rec['citation']}\n\n"
            f"FULL TEXT:\n{text}")


def user_msg_D(rec, text):
    return (f"Zotero key: {rec['key']}\n"
            f"Citation: {rec['citation']}\n\n"
            f"FULL TEXT:\n{text}")


def user_msg_E(rec, text):
    return (f"Zotero key: {rec['key']}\n"
            f"Citation: {rec['citation']}\n\n"
            f"REVIEW FULL TEXT:\n{text}")


def main():
    with open(MANIFEST, encoding="utf-8") as f:
        manifest = json.load(f)

    requests = []
    skipped = []
    mit_keys = mitigation_keys() if "D" in SKILLS else set()
    if "D" in SKILLS:
        print(f"Skill D mitigation subset: {len(mit_keys)} records (030 tag).")
    for rec in manifest:
        tp = os.path.join(TEXT_DIR, f"{rec['key']}.txt")
        if not rec["status"].startswith("ok") or not os.path.isfile(tp):
            skipped.append(rec["key"])
            continue
        with open(tp, encoding="utf-8") as t:
            text = t.read()
        kind = rec.get("kind", "primary")

        if kind == "primary" and "A" in SKILLS:
            requests.append({
                "custom_id": f"{rec['key']}__A",
                "params": {
                    "model": MODEL_A,
                    "max_tokens": MAX_TOKENS_A,
                    "system": SKILL_A_SYSTEM,
                    "messages": [{"role": "user", "content": user_msg_A(rec, text)}],
                },
            })
        if kind == "primary" and "B" in SKILLS:
            requests.append({
                "custom_id": f"{rec['key']}__B",
                "params": {
                    "model": MODEL_B,
                    "max_tokens": MAX_TOKENS_B,
                    "system": SKILL_B_SYSTEM,
                    "messages": [{"role": "user", "content": user_msg_B(rec, text)}],
                },
            })
        if kind == "primary" and "C" in SKILLS:
            requests.append({
                "custom_id": f"{rec['key']}__C",
                "params": {
                    "model": MODEL_C,
                    "max_tokens": MAX_TOKENS_C,
                    "system": SKILL_C_SYSTEM,
                    "messages": [{"role": "user", "content": user_msg_C(rec, text)}],
                },
            })
        if kind == "primary" and "D" in SKILLS and rec["key"] in mit_keys:
            requests.append({
                "custom_id": f"{rec['key']}__D",
                "params": {
                    "model": MODEL_D,
                    "max_tokens": MAX_TOKENS_D,
                    "system": SKILL_D_SYSTEM,
                    "messages": [{"role": "user", "content": user_msg_D(rec, text)}],
                },
            })
        if kind == "review" and "E" in SKILLS:
            requests.append({
                "custom_id": f"{rec['key']}__E",
                "params": {
                    "model": MODEL_E,
                    "max_tokens": MAX_TOKENS_E,
                    "system": SKILL_E_SYSTEM,
                    "messages": [{"role": "user", "content": user_msg_E(rec, text)}],
                },
            })

    if skipped:
        print(f"Skipped {len(skipped)} studies without usable text (fix in 0_prepare): {skipped}")
    if not requests:
        sys.exit("No requests to submit. Check manifest.json / text extraction.")

    client = anthropic.Anthropic()  # reads ANTHROPIC_API_KEY
    batch = client.messages.batches.create(requests=requests)
    with open(BATCH_ID_FILE, "w") as f:
        f.write(batch.id)
    print(f"Submitted batch {batch.id} with {len(requests)} requests for skills {SKILLS}.")
    print(f"Saved id to {BATCH_ID_FILE}. Run 2_harvest.py to poll and collect results.")


if __name__ == "__main__":
    main()
