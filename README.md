# Data and code for the systematic review "How forest roads, trails, and drainage structures alter hydrological connectivity"

Amerhauser, E., Holzleitner, F., Wemple, B., Behringer, M., Lechner, V., Scheidl, C.

This repository holds the data and code behind the review. Running one script
reproduces every figure and table of the article and of its Supplementary
Material.

## Contents

```
analysis/                     data and R code, run from this folder
  run_all.R                   reproduces all outputs
  aquasilva.csv               reference-library export, 272 entries; selected
                              columns and the screening tags of every record
                              (254 screened, 18 background references)
  AquaSilva.bib               bibliography (file paths and abstracts removed)
  cite_map.csv                record key -> BibTeX key (rebuilt by build_cite_map.R)
  search/search_strings.csv   the 54 searches (18 queries x 3 databases),
                              complete Boolean strings as run
  studies.csv                 extraction, one row per primary study (55)
  infrastructure.csv          one row per infrastructure element (95)
  connectivity.csv            one row per connectivity metric (225)
  effects.csv                 one row per reported effect (229)
  mitigation.csv              one row per mitigation measure (50)
  reviews.csv                 one row per included review (6)
  effects_corrections.csv     five documented corrections of effect direction,
                              applied at load time; the raw sheets are unchanged
  *.R                         analysis scripts, see below
outputs/
  screening_decisions.csv     decision, stage, and reason for each of the 254
                              screened records, with database and query provenance
  search_queries.csv          unique records per query, split by database
pipeline/                     Python scripts of the language-model extraction
screening/
  fulltext_reports/           111 gate-by-gate assessments of the full texts by
                              the language model, each verified by the reviewer
  fulltext_crosscheck.csv     model recommendation and final decision per record
protocol/
  stage1_calibration_cases.csv  calibration cases of title and abstract screening
                              (the protocol itself is given in Supplementary Material S1)
```

## Reproducing the results

Requirements: R (4.3 or later) with the packages tidyverse, here, patchwork,
scales, and stringr.

```
cd analysis
Rscript run_all.R
```

The script writes the figures to `analysis/figures/`, the companion tables to
`analysis/tables/`, and the LaTeX tables of the Supplementary Material to
`analysis/writeup/`.

| Script | Produces |
|---|---|
| `build_cite_map.R` | `cite_map.csv` |
| `extraction_load.R` | validates the closed vocabularies, applies `effects_corrections.csv`, joins the sheets to the library |
| `aquasilva_slr_visualization.R` | tag-based counts, PRISMA flow diagram |
| `rq_extraction_figures.R`, `rq5_synthesis.R` | extraction-based figures and vote counts |
| `DR_AS_SLR_manuscript_figures.R` | Figs. 2 to 8 of the article |
| `writeup_export.R`, `review_synthesis.R` | Tables S5.1 and S6.1 to S6.4 |
| `screening_export.R` | Tables S3.4 and S3.5, `screening_decisions.csv` |
| `search_export.R` | Tables S2.2 and S2.3, Fig. S1 |

## Tag system

Each record in `aquasilva.csv` carries its provenance and decisions in the
column `Manual Tags`: `001-` database, `090-` query, `091-ManualAdd` citation
tracking, `006-` screening decision and reason, `002-` to `005-` and `010-` to
`030-` content codes. The tag families are defined in Supplementary Material S1.

## Extraction pipeline

`pipeline/` holds the Python scripts that produced the extraction tables
(Supplementary Material S4). They need Python 3 with `pypdf` and `anthropic`
and an API key in `ANTHROPIC_API_KEY`.

| Script | Step |
|---|---|
| `0_prepare.py` | selects the included records, extracts the PDF full texts, writes `manifest.json` |
| `fix_glyphs.py` | repairs full texts with glyph codes from PDFs without a ToUnicode map |
| `1_submit.py` | builds and submits one batch per skill (switch `SKILLS`) |
| `2_harvest.py` | collects the batch results and writes the extraction CSVs |
| `rerun_b.py`, `rerun_c.py` | re-run Skill B or C for single records and merge the result |
| `prompts.py` | system prompts of Skills A to E, as run |

The scripts are archived in the state of their last use (for example
`SKILLS = ["E"]` in `1_submit.py`). `0_prepare.py` reads the PDF paths from the
full reference-manager export, which is not part of this repository; the full
texts are not shared for copyright reasons. Language-model output is not
deterministic, so a re-run reproduces the procedure, not every value. The
extraction tables in `analysis/` are the data the article is based on.

## Licence

**[To be decided, for example CC BY 4.0 for data and MIT for code.]**

## Citation

**[Article reference and DOI to be added.]**
