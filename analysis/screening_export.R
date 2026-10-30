# =============================================================================
# screening_export.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# Rebuilds the screening record of the review from the Zotero export
# (aquasilva.csv, Manual Tags) so that every number in the PRISMA flow diagram
# and in the supplementary screening tables traces back to one tag per record.
#
# Tag families used
#   001-*                database of retrieval (Scopus, WoS, GoogleScholar)
#   090-*                search string(s) that retrieved the record (H01-H09, M01-M09)
#   091-ManualAdd        added by backward citation tracking
#   006-Include-Stage1   passed title/abstract screening
#   006-Include-Stage2   passed full-text screening
#   006-Include-Final    included in the synthesis
#   006-Exclude-*        exclusion reason (OffTopic-* = title/abstract,
#                        Quality-* and NoFullText = full text)
#
# Records without any 006 tag are background references held in the same
# Zotero library; they were never part of the screened set and are dropped.
#
# Outputs (writeup/)
#   screening_decisions.csv              one row per screened record (254)
#   screening_overview.tex               stage x reason counts (booktabs table)
#   screening_fulltext_exclusions.tex    longtable of the full-text exclusions
# =============================================================================

suppressMessages({ library(tidyverse); library(here) })

raw_csv     <- here("aquasilva.csv")
writeup_dir <- here("writeup")
dir.create(writeup_dir, showWarnings = FALSE, recursive = TRUE)

latex_escape <- function(x) {
  x |>
    str_replace_all("\\\\", "\\\\textbackslash{}") |>
    str_replace_all("([&%$#_{}])", "\\\\\\1") |>
    str_replace_all("~", "\\\\textasciitilde{}") |>
    str_replace_all("\\^", "\\\\textasciicircum{}")
}

short_author <- function(a) {
  au <- str_split(a, ";\\s*")
  map_chr(au, function(v) {
    v <- str_trim(v[v != ""])
    sn <- str_trim(str_remove(v, ",.*$"))
    if (length(sn) == 0) "Anon."
    else if (length(sn) == 1) sn
    else if (length(sn) == 2) paste(sn[1], "and", sn[2])
    else paste(sn[1], "et al.")
  })
}

# --- tags per record -----------------------------------------------------------
aq <- read_csv(raw_csv, col_types = cols(.default = col_character()), na = character()) |>
  rename_with(~ str_remove(.x, "^﻿"))

tags <- aq |>
  select(zotero_key = Key, mt = `Manual Tags`) |>
  separate_rows(mt, sep = ";\\s*") |>
  mutate(tag = str_trim(mt)) |>
  filter(tag != "")

collapse_tags <- function(prefix, strip) {
  tags |>
    filter(str_starts(tag, prefix)) |>
    mutate(v = str_remove(tag, strip)) |>
    group_by(zotero_key) |>
    summarise(v = paste(sort(unique(v)), collapse = ";"), .groups = "drop")
}

db  <- collapse_tags("001-", "^001-") |> rename(databases = v)
qry <- collapse_tags("090-", "^090-") |> rename(search_strings = v)

dec <- tags |>
  filter(str_starts(tag, "006-") | tag == "091-ManualAdd") |>
  group_by(zotero_key) |>
  summarise(
    snowball  = any(tag == "091-ManualAdd"),
    stage1    = any(tag == "006-Include-Stage1"),
    stage2    = any(tag == "006-Include-Stage2"),
    final     = any(tag == "006-Include-Final"),
    excl_tag  = paste(tag[str_starts(tag, "006-Exclude")], collapse = ";"),
    n006      = sum(str_starts(tag, "006-")),
    .groups   = "drop") |>
  filter(n006 > 0)

# consistency: each screened record carries exactly one terminal decision
bad <- dec |> filter((final & excl_tag != "") | (!final & excl_tag == "") |
                     str_detect(excl_tag, ";"))
if (nrow(bad)) stop("Inconsistent screening tags: ", paste(bad$zotero_key, collapse = ", "))
bad2 <- dec |> filter(str_detect(excl_tag, "Quality|NoFullText") & !stage1)
if (nrow(bad2)) stop("Full-text reason without Stage1 tag: ", paste(bad2$zotero_key, collapse = ", "))

reason_label <- c(
  "006-Exclude-OffTopic-not peer-reviewed"          = "Not peer-reviewed",
  "006-Exclude-OffTopic-no Block-B infrastructure"  = "No forest infrastructure (Block B)",
  "006-Exclude-OffTopic-wrong publication type"     = "Wrong publication type",
  "006-Exclude-OffTopic-non-forest context"         = "Non-forest context",
  "006-Exclude-OffTopic-not accessable"             = "Neither abstract nor full text accessible",
  "006-Exclude-OffTopic-language"                   = "Language",
  "006-Exclude-OffTopic-no connectivity framing"    = "No connectivity framing",
  "006-Exclude-Quality-connectivity only narrative, not operationalized" =
    "Connectivity only narrative, not operationalised",
  "006-Exclude-Quality-infrastructure not analyzed as variable" =
    "Infrastructure not analysed as a variable",
  "006-Exclude-Quality-no Block-B infrastructure as variable" =
    "No Block-B infrastructure as a variable",
  "006-Exclude-Quality-infrastructure mentioned as context only" =
    "Infrastructure mentioned as context only",
  "006-Exclude-Quality-connectivity not operationalized as outcome" =
    "Connectivity not operationalised as an outcome",
  "006-Exclude-NoFullText"                          = "Full text not retrievable")

unknown <- setdiff(unique(dec$excl_tag[dec$excl_tag != ""]), names(reason_label))
if (length(unknown)) stop("Unmapped exclusion tag(s): ", paste(unknown, collapse = " | "))

screen <- dec |>
  left_join(aq |> transmute(zotero_key = Key, item_type = `Item Type`,
                            year = `Publication Year`, author = Author,
                            title = Title, source = `Publication Title`, doi = DOI),
            by = "zotero_key") |>
  left_join(db, by = "zotero_key") |>
  left_join(qry, by = "zotero_key") |>
  mutate(
    origin   = if_else(snowball, "Citation tracking", "Database search"),
    stage_reached = case_when(final ~ "Included",
                              stage1 ~ "Full text",
                              TRUE ~ "Title and abstract"),
    decision = if_else(final, "Include", "Exclude"),
    exclusion_stage = case_when(final ~ NA_character_,
                                stage1 ~ "Full text",
                                TRUE ~ "Title and abstract"),
    exclusion_reason = unname(reason_label[excl_tag]),
    author_short = short_author(author))

# review vs primary for included records (003-Review tag)
rev_keys <- tags |> filter(tag == "003-Review") |> pull(zotero_key)
screen <- screen |>
  mutate(included_as = case_when(final & zotero_key %in% rev_keys ~ "Review",
                                 final ~ "Primary study",
                                 TRUE ~ NA_character_))

# --- CSV (one row per screened record) ----------------------------------------
out <- screen |>
  arrange(factor(stage_reached, c("Included", "Full text", "Title and abstract")),
          exclusion_reason, author_short, year) |>
  select(zotero_key, author = author_short, year, title, source, doi, item_type,
         origin, databases, search_strings, stage_reached, decision,
         exclusion_stage, exclusion_reason, included_as)
write_csv(out, file.path(writeup_dir, "screening_decisions.csv"), na = "")

# --- counts ------------------------------------------------------------------
n_screened <- nrow(screen)
n_ta_ex    <- sum(screen$exclusion_stage == "Title and abstract", na.rm = TRUE)
n_ft       <- sum(screen$stage1)
n_ft_ex    <- sum(screen$exclusion_stage == "Full text", na.rm = TRUE)
n_inc      <- sum(screen$final)
n_rev      <- sum(screen$included_as == "Review", na.rm = TRUE)
n_snow     <- sum(screen$snowball)
n_notretr  <- sum(screen$exclusion_reason == "Full text not retrievable", na.rm = TRUE)
n_assessed <- n_ft - n_notretr
n_elig_ex  <- n_ft_ex - n_notretr
stopifnot(n_screened == n_ta_ex + n_ft, n_ft == n_ft_ex + n_inc)

reason_rows <- function(stage) {
  screen |> filter(exclusion_stage == stage,
                   is.na(exclusion_reason) | exclusion_reason != "Full text not retrievable") |>
    count(exclusion_reason, sort = TRUE) |>
    arrange(desc(n), exclusion_reason) |>
    transmute(line = sprintf("\\quad %s & %d \\\\", latex_escape(exclusion_reason), n))
}

overview <- c(
  "% Auto-generated by screening_export.R from the Zotero screening tags.",
  "% Requires \\usepackage{booktabs}.",
  "\\begin{table}[htbp]",
  "\\centering",
  sprintf(paste0("\\caption{Screening outcome by stage and reason. %d records were ",
                 "screened, %d of them retrieved by the search strings and %d added by ",
                 "citation tracking. Every record carries exactly one terminal decision, ",
                 "and the per-record decisions are provided in ",
                 "\\texttt{screening\\_decisions.csv} in the data repository.}"),
          n_screened, n_screened - n_snow, n_snow),
  "\\label{tab:screening-overview}",
  "\\small",
  "\\begin{tabular}{@{}lr@{}}",
  "\\toprule",
  "Stage and exclusion reason & Records \\\\",
  "\\midrule",
  "\\multicolumn{2}{@{}l}{\\textit{Title and abstract screening, excluded}} \\\\",
  reason_rows("Title and abstract")$line,
  sprintf("\\quad\\textit{Subtotal} & \\textit{%d} \\\\", n_ta_ex),
  "\\addlinespace",
  sprintf("\\multicolumn{2}{@{}l}{\\textit{Reports sought for retrieval, %d}} \\\\", n_ft),
  sprintf("\\quad Full text not retrievable & %d \\\\", n_notretr),
  "\\addlinespace",
  sprintf("\\multicolumn{2}{@{}l}{\\textit{Full-text assessment of %d reports, excluded}} \\\\", n_assessed),
  reason_rows("Full text")$line,
  sprintf("\\quad\\textit{Subtotal} & \\textit{%d} \\\\", n_elig_ex),
  "\\addlinespace",
  "\\multicolumn{2}{@{}l}{\\textit{Included}} \\\\",
  sprintf("\\quad Primary studies & %d \\\\", n_inc - n_rev),
  sprintf("\\quad Reviews & %d \\\\", n_rev),
  sprintf("\\quad\\textbf{Total included} & \\textbf{%d} \\\\", n_inc),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}")
writeLines(overview, file.path(writeup_dir, "screening_overview.tex"))

# --- full-text exclusions (PRISMA item 16b) ----------------------------------
ft <- screen |> filter(exclusion_stage == "Full text") |>
  mutate(reason_n = ave(seq_along(exclusion_reason), exclusion_reason, FUN = length)) |>
  arrange(desc(reason_n), exclusion_reason, author_short, year)

ft_lines <- ft |>
  mutate(title = str_trunc(str_squish(title), 140, ellipsis = "…"),
         doi_tex = if_else(doi == "", "", sprintf("\\newline\\footnotesize\\url{%s}",
                                                  str_replace_all(doi, "([%#])", "\\\\\\1"))),
         line = sprintf("%s %s & %s%s & %s \\\\",
                        latex_escape(author_short), year,
                        latex_escape(title), doi_tex,
                        latex_escape(exclusion_reason))) |>
  pull(line)

ft_tex <- c(
  sprintf("%% Auto-generated by screening_export.R (%d records).", nrow(ft)),
  "% Requires \\usepackage{longtable,booktabs}.",
  "{\\small",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{0.20\\textwidth} >{\\raggedright\\arraybackslash}p{0.50\\textwidth} >{\\raggedright\\arraybackslash}p{0.24\\textwidth}@{}}",
  sprintf(paste0("\\caption{Records excluded at the full-text stage (n = %d), ",
                 "grouped by the first failed gate. %d of them could not be retrieved, ",
                 "%d were excluded after full-text assessment.}\\label{tab:fulltext-exclusions}\\\\"),
          nrow(ft), n_notretr, n_elig_ex),
  "\\toprule Record & Title & Reason for exclusion \\\\ \\midrule \\endfirsthead",
  "\\toprule Record & Title & Reason for exclusion \\\\ \\midrule \\endhead",
  "\\bottomrule \\endfoot",
  ft_lines,
  "\\end{longtable}}")
writeLines(ft_tex, file.path(writeup_dir, "screening_fulltext_exclusions.tex"))

message(sprintf(paste0(
  "Screening export: %d screened (%d database, %d citation tracking); ",
  "%d excluded at title/abstract; %d reports sought, %d not retrieved, %d assessed, %d excluded; ",
  "%d included (%d primary, %d reviews). %d library entries without screening tags ignored."),
  n_screened, n_screened - n_snow, n_snow, n_ta_ex, n_ft, n_notretr, n_assessed, n_elig_ex,
  n_inc, n_inc - n_rev, n_rev, nrow(aq) - n_screened))
