# =============================================================================
# writeup_export.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# Turns the four core extraction sheets into write-up / citation material:
#   writeup/evidence_<sheet>.tex   - per-record LaTeX longtable (Study | ... |
#                                    locator | trimmed evidence quote)
#   writeup/clusters_*.tex / .csv  - findings grouped into \cite{} clusters
#   writeup/writeup_digest.md      - readable clusters for drafting prose
#
# Evidence tables show citation_short (always renders). Clusters emit
# \cite{<CITE_KEY>} groups - a scaffold that resolves once a matching .bib is
# supplied. Set CITE_KEY to your .bib key column when known.
#
# NOTE: longtable does not work inside a two-column layout - place the evidence
# tables in a single-column / \textwidth context (supplement or \onecolumn).
# =============================================================================

suppressMessages({ library(tidyverse); library(here) })

if (!exists("ex_effects")) source(here("extraction_load.R"))

writeup_dir <- here("writeup")
dir.create(writeup_dir, showWarnings = FALSE, recursive = TRUE)

CITE_KEY <- "cite_of"     # \cite{} tokens come from cite_of() (cite_map.csv)

# --- helpers -----------------------------------------------------------------
latex_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}
blank_dash <- function(x) if_else(is.na(x) | str_trim(x) == "", "-", as.character(x))
trim_quote <- function(x, n = 200) {
  x <- str_squish(replace_na(as.character(x), ""))
  if_else(nchar(x) > n, paste0(str_sub(x, 1, n - 1), "\u2026"), x)
}

cite_lookup <- ex_studies |> distinct(zotero_key, citation_short)
attach_cite <- function(df) {
  if (!"citation_short" %in% names(df))
    df <- df |> left_join(cite_lookup, by = "zotero_key")
  df |> mutate(citation_short = coalesce(na_if(citation_short, ""), zotero_key))
}

# per-record LaTeX longtable ---------------------------------------------------
evidence_table <- function(df, cols, headers, widths, caption, label, file,
                           quote_cols = character()) {
  d <- df
  for (qc in quote_cols) d[[qc]] <- trim_quote(d[[qc]])
  # closed-vocabulary values: split camel case so narrow columns can wrap
  for (vc in setdiff(cols, c("citation_short", "locator", quote_cols)))
    d[[vc]] <- gsub("([a-z])([A-Z])", "\\1 \\2", d[[vc]])
  for (cc in cols)       d[[cc]] <- latex_escape(blank_dash(d[[cc]]))
  # study column as a citation, so every row resolves to a full reference
  if ("citation_short" %in% cols && "zotero_key" %in% names(d))
    d$citation_short <- sprintf("\\citet{%s}", cite_of(d$zotero_key))
  body <- d |> select(all_of(cols)) |>
    unite("line", everything(), sep = " & ") |>
    transmute(line = paste0(line, " \\\\"))
  colspec <- paste(sprintf(">{\\raggedright\\arraybackslash}p{%s}", widths), collapse = " ")
  hdr <- paste(headers, collapse = " & ")
  tex <- c(
    sprintf("%% Auto-generated evidence table (%d rows). Single-column / \\textwidth context.", nrow(d)),
    "% Requires \\usepackage{longtable,booktabs,array}.",
    "\\setlength{\\tabcolsep}{3pt}",
    sprintf("\\begin{longtable}{@{}%s@{}}", colspec),
    sprintf("\\caption{%s}\\label{%s}\\\\", caption, label),
    "\\toprule", paste0(hdr, " \\\\"), "\\midrule \\endfirsthead",
    paste0("\\toprule ", hdr, " \\\\ \\midrule \\endhead"),
    body$line,
    "\\bottomrule", "\\end{longtable}"
  )
  writeLines(tex, file.path(writeup_dir, file))
  invisible(nrow(d))
}

# citation clusters ------------------------------------------------------------
cluster <- function(df, group_cols, caption, file_stub) {
  g <- df |> attach_cite() |>
    filter(if_all(all_of(group_cols), ~ .x != "")) |>
    group_by(across(all_of(group_cols))) |>
    summarise(n_rows    = n(),
              n_studies = n_distinct(zotero_key),
              keys      = paste(sort(unique(cite_of(zotero_key))), collapse = ","),
              shorts    = paste(sort(unique(citation_short)), collapse = "; "),
              .groups = "drop") |>
    unite("label_txt", all_of(group_cols), sep = " / ", remove = FALSE) |>
    arrange(desc(n_studies))
  write_csv(g |> select(all_of(group_cols), n_rows, n_studies, keys, shorts),
            file.path(writeup_dir, paste0(file_stub, ".csv")))
  items <- sprintf("\\item \\textbf{%s} (%d studies, %d rows): \\cite{%s}",
                   latex_escape(g$label_txt), g$n_studies, g$n_rows, g$keys)
  tex <- c(sprintf("%% Citation clusters: %s. \\cite keys resolved via cite_map.csv (AquaSilva.bib).",
                   caption),
           "\\begin{itemize}", items, "\\end{itemize}")
  writeLines(tex, file.path(writeup_dir, paste0(file_stub, ".tex")))
  g |> mutate(cluster = caption)
}

# =============================================================================
# effects
# =============================================================================
eff <- ex_effects |> attach_cite() |>
  mutate(effect_lab = sprintf("%s (%s)", direction, magnitude_qual))
evidence_table(
  eff,
  cols    = c("citation_short", "infrastructure_type", "outcome_variable",
              "effect_lab", "locator", "evidence_quote"),
  headers = c("Study", "Infrastructure", "Outcome", "Effect", "Loc.", "Evidence"),
  widths  = c("2.0cm", "2.1cm", "2.2cm", "2.2cm", "2.4cm", "4.4cm"),
  caption = "Extracted effects: reported direction and magnitude by infrastructure and outcome, with supporting evidence.",
  label   = "tab:evidence-effects", file = "evidence_effects.tex",
  quote_cols = "evidence_quote")
cl_eff_out <- cluster(ex_effects, c("outcome_variable", "direction"),
                      "Effects by outcome and direction", "clusters_effects_outcome")
cl_eff_con <- cluster(ex_effects |> filter(outcome_family == "Connectivity"),
                      c("infrastructure_type", "direction"),
                      "Effect on connectivity by infrastructure and direction",
                      "clusters_effects_connectivity")

# =============================================================================
# connectivity
# =============================================================================
evidence_table(
  ex_connectivity |> attach_cite(),
  cols    = c("citation_short", "metric_family", "connectivity_type",
              "operationalization_basis", "locator", "evidence_quote"),
  headers = c("Study", "Metric family", "Type", "Basis", "Loc.", "Evidence"),
  widths  = c("2.0cm", "2.3cm", "1.7cm", "2.6cm", "2.4cm", "4.3cm"),
  caption = "Connectivity operationalisation: metric family, type and basis per reported metric instance.",
  label   = "tab:evidence-connectivity", file = "evidence_connectivity.tex",
  quote_cols = "evidence_quote")
cl_con <- cluster(ex_connectivity, c("metric_family"),
                  "Studies by connectivity metric family", "clusters_connectivity_metric")

# =============================================================================
# infrastructure  (design_descriptor is its evidence/prose field)
# =============================================================================
evidence_table(
  ex_infrastructure |> attach_cite(),
  cols    = c("citation_short", "infrastructure_type", "condition",
              "network_topology", "locator", "design_descriptor"),
  headers = c("Study", "Type", "Condition", "Topology", "Loc.", "Design descriptor"),
  widths  = c("2.0cm", "2.2cm", "1.9cm", "2.3cm", "2.4cm", "4.5cm"),
  caption = "Infrastructure characteristics: type, condition, network topology and design descriptor per record.",
  label   = "tab:evidence-infrastructure", file = "evidence_infrastructure.tex",
  quote_cols = "design_descriptor")
cl_infra <- cluster(ex_infrastructure, c("infrastructure_type"),
                    "Studies by infrastructure type", "clusters_infrastructure_type")

# =============================================================================
# mitigation
# =============================================================================
evidence_table(
  ex_mitigation |> attach_cite(),
  cols    = c("citation_short", "measure_type", "target_problem",
              "effectiveness", "locator", "evidence_quote"),
  headers = c("Study", "Measure", "Target", "Effectiveness", "Loc.", "Evidence"),
  widths  = c("2.0cm", "2.3cm", "2.2cm", "2.2cm", "2.4cm", "4.2cm"),
  caption = "Mitigation measures: type, target problem and reported effectiveness with supporting evidence.",
  label   = "tab:evidence-mitigation", file = "evidence_mitigation.tex",
  quote_cols = "evidence_quote")
cl_mit <- cluster(ex_mitigation, c("measure_type", "effectiveness"),
                  "Mitigation measures by type and effectiveness", "clusters_mitigation")

# =============================================================================
# Readable digest (all clusters) for drafting prose
# =============================================================================
digest <- bind_rows(cl_eff_out, cl_eff_con, cl_con, cl_infra, cl_mit)
md <- c("# Write-up digest - citation clusters", "",
        "Readable groupings for drafting the Results text (each line: finding - studies).",
        "The parallel `.tex` files carry the same groups as `\\cite{}` clusters.", "")
for (cap in unique(digest$cluster)) {
  md  <- c(md, sprintf("## %s", cap), "")
  sub <- digest |> filter(cluster == cap)
  for (i in seq_len(nrow(sub)))
    md <- c(md, sprintf("- **%s** (%d studies): %s",
                        sub$label_txt[i], sub$n_studies[i], sub$shorts[i]))
  md <- c(md, "")
}
writeLines(md, file.path(writeup_dir, "writeup_digest.md"))

# =============================================================================
# included records with full references (PRISMA item 17)
# =============================================================================
inc_tags <- aq |>
  transmute(zotero_key = Key, year = `Publication Year`, tags = `Manual Tags`) |>
  tidyr::separate_rows(tags, sep = ";\\s*") |>
  mutate(tags = str_trim(tags)) |>
  group_by(zotero_key, year) |>
  filter(any(tags == "006-Include-Final")) |>
  summarise(
    review  = any(tags == "003-Review"),
    tracked = any(tags == "091-ManualAdd"),
    design  = paste(sort(unique(str_remove(tags[str_starts(tags, "003-") & tags != "003-Review"], "^003-"))), collapse = ", "),
    region  = paste(sort(unique(str_remove(tags[str_starts(tags, "005-")], "^005-"))), collapse = ", "),
    infra   = paste(sort(unique(str_remove(tags[str_starts(tags, "002-")], "^002-"))), collapse = ", "),
    .groups = "drop") |>
  mutate(key = cite_of(zotero_key),
         type = if_else(review, "Review", "Primary"),
         across(c(design, region, infra), ~ gsub("([a-z])([A-Z])", "\\1 \\2", .x)),
         design = if_else(review, "Review", design)) |>
  arrange(as.integer(year), key)
stopifnot(nrow(inc_tags) == 61)

inc_lines <- sprintf("\\citet{%s}%s & %s & %s & %s \\\\",
                     inc_tags$key, if_else(inc_tags$tracked, "$^{\\dagger}$", ""),
                     latex_escape(blank_dash(inc_tags$design)),
                     latex_escape(blank_dash(inc_tags$infra)),
                     latex_escape(blank_dash(inc_tags$region)))
inc_tex <- c(
  "% Auto-generated by writeup_export.R: the 61 included records (PRISMA item 17).",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{longtable}{@{}>{\\raggedright\\arraybackslash}p{4.6cm} >{\\raggedright\\arraybackslash}p{3.6cm} >{\\raggedright\\arraybackslash}p{4.8cm} >{\\raggedright\\arraybackslash}p{2.6cm}@{}}",
  sprintf(paste0("\\caption{The %d records included in the synthesis, %d primary studies and %d reviews, ",
                 "in order of publication. Full references are given at the end of this document. ",
                 "$^{\\dagger}$ Added by citation tracking.}\\label{tab:included}\\\\"),
          nrow(inc_tags), sum(!inc_tags$review), sum(inc_tags$review)),
  "\\toprule Record & Study type & Infrastructure & Region \\\\ \\midrule \\endfirsthead",
  "\\toprule Record & Study type & Infrastructure & Region \\\\ \\midrule \\endhead",
  "\\bottomrule \\endfoot",
  inc_lines,
  "\\end{longtable}")
writeLines(inc_tex, file.path(writeup_dir, "included_studies.tex"))


message(sprintf(
  "writeup/ written: 4 evidence tables (effects %d, connectivity %d, infrastructure %d, mitigation %d rows)\n  + 5 citation-cluster sets (.tex/.csv) + writeup_digest.md. \\cite keys via cite_map (%d mapped).",
  nrow(ex_effects), nrow(ex_connectivity), nrow(ex_infrastructure), nrow(ex_mitigation), nrow(cite_map)))
