# =============================================================================
# review_synthesis.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# Turns the (now populated) reviews.csv into write-up material:
#   writeup/review_synthesis.md   - per-review digest (all qualitative fields)
#                                    for drafting the review-synthesis prose
#   writeup/review_synthesis.tex  - a citable LaTeX longtable
#                                    (Review | Classification | Identified gaps)
#   writeup/review_synthesis.csv  - the same, machine-readable
#
# Citation token: uses citation_short for display and zotero_key as the \cite
# placeholder. Set CITE_KEY once you know your .bib convention (see note below).
# =============================================================================

suppressMessages({ library(tidyverse); library(here) })

if (!exists("ex_reviews")) source(here("extraction_load.R"))

writeup_dir <- here("writeup")
dir.create(writeup_dir, showWarnings = FALSE, recursive = TRUE)

# Citation tokens come from cite_of() (cite_map.csv, via extraction_load.R);
# falls back to zotero_key if cite_map.csv is absent.
CITE_KEY <- "cite_of"

# --- LaTeX escaping for free-text cells --------------------------------------
latex_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}
blank_dash <- function(x) if_else(is.na(x) | str_trim(x) == "", "-", x)

rev_tbl <- ex_reviews |>
  mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |>
  arrange(citation_short)

n_rev  <- nrow(rev_tbl)
n_full <- sum(rev_tbl$synthesis_statement != "" | rev_tbl$identified_gaps != "" |
              rev_tbl$consensus_claim != "" | rev_tbl$classification_scheme != "")

# -----------------------------------------------------------------------------
# 1  Markdown digest (for drafting prose)
# -----------------------------------------------------------------------------
md <- c(
  "# Review synthesis digest",
  "",
  sprintf("_%d review-type records; %d carry qualitative synthesis fields._",
          n_rev, n_full),
  "",
  "Use this to draft the review-synthesis paragraph(s). Each block is one review;",
  "cite with the key noted in brackets.",
  ""
)
for (i in seq_len(n_rev)) {
  r <- rev_tbl[i, ]
  md <- c(md,
    sprintf("### %s  [`%s`]", blank_dash(r$citation_short), cite_of(r$zotero_key)),
    if (r$classification_scheme != "") sprintf("- **Classification:** %s", r$classification_scheme),
    if (r$synthesis_statement != "")   sprintf("- **Synthesis:** %s", r$synthesis_statement),
    if (r$consensus_claim != "")       sprintf("- **Consensus:** %s", r$consensus_claim),
    if (r$identified_gaps != "")       sprintf("- **Identified gaps:** %s", r$identified_gaps),
    if (r$locator != "")               sprintf("- _locator:_ %s", r$locator),
    ""
  )
}
writeLines(md, file.path(writeup_dir, "review_synthesis.md"))

# -----------------------------------------------------------------------------
# 2  LaTeX longtable (Review | Classification | Identified gaps)
# -----------------------------------------------------------------------------
body <- rev_tbl |>
  transmute(line = sprintf(
    "\\cite{%s} & %s & %s \\\\",
    cite_of(zotero_key),
    latex_escape(blank_dash(classification_scheme)),
    latex_escape(blank_dash(identified_gaps))))

tex <- c(
  "% Auto-generated from reviews.csv - review-synthesis overview.",
  "% Requires \\usepackage{longtable} and \\usepackage{booktabs}.",
  "% \\cite keys resolved via cite_map.csv (AquaSilva.bib).",
  "\\begin{longtable}{@{}p{3.0cm} p{3.6cm} p{7.4cm}@{}}",
  "\\caption{Synthesis of the included review articles: classification and the principal research gaps each identifies.}",
  "\\label{tab:review-synthesis}\\\\",
  "\\toprule",
  "Review & Classification & Identified gaps \\\\",
  "\\midrule \\endfirsthead",
  "\\toprule Review & Classification & Identified gaps \\\\ \\midrule \\endhead",
  body$line,
  "\\bottomrule",
  "\\end{longtable}"
)
writeLines(tex, file.path(writeup_dir, "review_synthesis.tex"))

# -----------------------------------------------------------------------------
# 3  Machine-readable copy
# -----------------------------------------------------------------------------
write_csv(
  rev_tbl |> select(any_of(c("zotero_key", "citation_short", "doi",
                             "classification_scheme", "consensus_claim",
                             "identified_gaps", "synthesis_statement", "locator"))),
  file.path(writeup_dir, "review_synthesis.csv"))

message(sprintf(
  "Review synthesis written to writeup/: review_synthesis.{md,tex,csv}\n  %d reviews, %d with qualitative fields. \\cite keys via cite_map.",
  n_rev, n_full))

