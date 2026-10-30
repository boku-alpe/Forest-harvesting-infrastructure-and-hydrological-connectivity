# =============================================================================
# AquaSilva SLR  -  Visualisation pipeline
# Author:        Elias Amerhauser
# Project:       AquaSilva (BMLUK, Q1/2026–Q3/2029)
# Workflow:      PSALSAR connectivity-anchored SLR, Stage 1 + Stage 2 complete
# Input:         aquasilva.csv  (Zotero export from Include-Final + pipeline)
# Output:        ./figures/*.pdf + *.png
#
# Run order:     execute sections in numeric order on a fresh R session.
# Each section is delimited by a `# NN  TITLE -----` header so RStudio's
# document outline can fold them.
# =============================================================================


# 00  SETUP  ===================================================================

# 0.1  Packages ----------------------------------------------------------------
# One-time installation: uncomment and run the install.packages() block once.

# install.packages(c("tidyverse", "patchwork", "scales", "here"))
#
# v2 note: the dependency stack was trimmed. UpSet plots (ComplexUpset) were
# replaced by Coverage-bar + companion-table pairs (clearer at cas-dc column
# width and theme-consistent); the choropleth map (sf / rnaturalearth) was
# replaced by a plain region bar; viridis/RColorBrewer are no longer needed
# because the gap matrix uses an explicit two-regime fill (zero cells loud).

library(tidyverse)      # readr, dplyr, tidyr, stringr, purrr, ggplot2
library(patchwork)      # multi-panel composition
library(scales)         # axis formatting helpers
library(here)           # robust, OS-agnostic path handling


# 0.2  Paths -------------------------------------------------------------------
# Self-contained results/ layout (user decision 2026-06-22): the script, the
# Zotero export, and all outputs live together under
#   .../13_Review_Protokoll/results/
# `here::i_am()` anchors all relative paths to this file's location.
# Portable: only switch directory where the original Windows path exists.
# Run from this folder (the analysis/ directory of the data repository).
here::i_am("aquasilva_slr_visualization.R")

raw_csv     <- here("aquasilva.csv")
figures_dir <- here("figures")
tables_dir  <- here("tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)


# 0.3  ggplot2 theme -----------------------------------------------------------
# theme_minimal base with consistent text sizes tuned for 8 cm column width.
# Major y-grid only; no minor grid; left-aligned caption for source notes.

theme_aquasilva <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title         = element_text(face = "plain"),
      plot.title         = element_text(face = "bold", size = base_size + 1),
      plot.subtitle      = element_text(colour = "grey30", size = base_size - 1),
      plot.caption       = element_text(colour = "grey40", size = base_size - 2,
                                        hjust = 0),
      legend.title       = element_text(face = "plain", size = base_size - 1),
      legend.text        = element_text(size = base_size - 1)
    )
}
theme_set(theme_aquasilva())


# 0.4  Colour palettes ---------------------------------------------------------
# Categorical: Okabe-Ito 8-colour palette (Wong 2011, Nature Methods).
#              Colourblind-safe and print-robust; de-facto standard for
#              qualitative scientific data.
# Note:        sequential/diverging palettes (viridis, RColorBrewer) were
#              dropped in v2 - no choropleth/UpSet remain, and the gap matrix
#              uses an explicit two-regime fill defined inline in section 06.

okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

pal_studytype <- c(
  "FieldMonitoring" = okabe_ito[1],
  "Modeling"        = okabe_ito[2],
  "Experimental"    = okabe_ito[3],
  "PairedCatchment" = okabe_ito[4],
  "Review"          = okabe_ito[8]   # grey - reviews are visually set apart
)

pal_region <- c(
  "Europe"       = okabe_ito[1],
  "NorthAmerica" = okabe_ito[2],
  "Asia"         = okabe_ito[3],
  "Oceania"     = okabe_ito[4],
  "SouthAmerica" = okabe_ito[5],
  "Africa"       = okabe_ito[6],
  "Global"       = okabe_ito[8]
)

pal_infrastructure <- c(
  "Roads"              = okabe_ito[2],
  "Structures"         = okabe_ito[1],
  "ExtractionTrails"   = okabe_ito[3],
  "RoadDrainageBMP"    = okabe_ito[6],
  "RetentionDetention" = okabe_ito[5]
)

pal_outcome <- c(
  "PeakFlow"           = okabe_ito[5],
  "RunoffResponse"     = okabe_ito[2],
  "SedimentDelivery"   = okabe_ito[6],
  "FlowPathInitiation" = okabe_ito[3]
)


# 0.5  Permitted-tag vocabulary ------------------------------------------------
# Canonical reference for the conformance check in section 01.
# Source of truth = the tags actually used in the Zotero library after the
# 002-Mixed disaggregation (user decision 2026-06-18).
# Any tag value not listed here is reported in section 1.7 for manual review.
#
# Note on 030: '030-Cany-MitigationControls' is a deliberate roll-up of the
# original C1/C2/C3 sub-tags (rationale: pilot search returned unsatisfactory
# results for the disaggregated C-tags). RQ6a is therefore answered at the
# Mitigation-yes/no level only, by design.

permitted_tags <- list(
  `001` = c("Scopus", "WoS", "GoogleScholar"),
  `002` = c("Roads", "Structures", "ExtractionTrails",
            "RoadDrainageBMP", "RetentionDetention"),
  `003` = c("FieldMonitoring", "Modeling", "Experimental",
            "PairedCatchment", "Review"),
  `004` = c("PeakFlow", "RunoffResponse",
            "SedimentDelivery", "FlowPathInitiation"),
  `005` = c("Europe", "NorthAmerica", "Asia",
            "Oceania", "SouthAmerica", "Africa", "Global"),
  `010` = c("A1-ConnectivityConcepts", "A2-FlowPathDescriptors",
            "A3-ConnectivityMetrics"),
  `020` = c("B1-Roads", "B2-SkidSnigTrails", "B3-Structures"),
  `030` = c("Cany-MitigationControls", "empty"),
  `091` = c("ManualAdd")
)


# 0.6  Save helper -------------------------------------------------------------
# Primary output is PDF (vector, embeds fonts + UTF-8 glyphs via cairo, no
# rescaling loss in Overleaf). PNG@300 is kept as an optional secondary output
# for slides / the WLV article (toggle with png = FALSE).
#
# cas-dc is a TWO-COLUMN class. A figure must be rendered at the physical width
# of the slot it will occupy and then placed at scale 1.0 (width=\linewidth) in
# LaTeX - never down-scaled by \includegraphics, which is what shrinks text to
# illegibility. Two legitimate target widths:
W_COL  <- 8.4    # single column  (figure)   - confirm via \the\columnwidth
W_FULL <- 17.4   # full text width (figure*)  - confirm via \the\textwidth

save_plot <- function(plot, name, width_cm = W_FULL, height_cm = 10,
                      png = TRUE) {
  ggsave(file.path(figures_dir, paste0(name, ".pdf")),
         plot, width = width_cm, height = height_cm, units = "cm",
         device = cairo_pdf)
  if (png) {
    ggsave(file.path(figures_dir, paste0(name, ".png")),
           plot, width = width_cm, height = height_cm, units = "cm", dpi = 300)
  }
  invisible(plot)
}

# Companion-table helper: within-family tag combinations + counts, written to
# CSV so that detail dropped from a Coverage bar stays reproducible (Test 6).
write_combo_table <- function(corpus_df, tag_col, file_name) {
  out <- corpus_df |>
    transmute(Key, combo = map_chr(.data[[tag_col]],
      ~ if (length(.x) == 0 || all(is.na(.x))) NA_character_
        else paste(sort(unique(.x[.x != "empty"])), collapse = " + "))) |>
    filter(!is.na(combo), combo != "") |>
    count(combo, name = "n", sort = TRUE) |>
    mutate(pct = round(100 * n / sum(n), 1)) |>
    rename(`Tag combination` = combo, Records = n, `% of tagged` = pct)
  write_csv(out, file.path(tables_dir, file_name))
  out
}

message("Setup complete.\n  raw_csv     = ", raw_csv,
        "\n  figures_dir = ", figures_dir)



# 01  IMPORT  ==================================================================

# 1.1  Read raw Zotero export --------------------------------------------------

zotero_raw <- read_csv(raw_csv, show_col_types = FALSE) |>
  select(where(~ !all(is.na(.))))    # drop fully empty Zotero default columns

cat("Loaded ", nrow(zotero_raw), " records and ",
    ncol(zotero_raw), " non-empty columns.\n", sep = "")


# 1.2  Long-form tag table -----------------------------------------------------
# One row per (Key, tag). All downstream analyses derive from this table.
# Zotero stores tags semicolon-delimited in a single cell.

tags_long <- zotero_raw |>
  select(Key, `Manual Tags`) |>
  filter(!is.na(`Manual Tags`)) |>
  separate_longer_delim(`Manual Tags`, delim = ";") |>
  rename(tag = `Manual Tags`) |>
  mutate(tag = str_trim(tag)) |>
  filter(tag != "")

# Tag family = leading 3-digit prefix (001, 002, ..., 091)
# Tag value  = portion after the prefix dash (e.g. "Roads", "Include-Final")
tags_long <- tags_long |>
  mutate(
    family = str_extract(tag, "^[0-9]{3}"),
    value  = str_remove(tag, "^[0-9]{3}-")
  )

tags_struct <- tags_long |> filter(!is.na(family))   # structured analysis tags
tags_free   <- tags_long |> filter(is.na(family))    # auto-imported keywords


# 1.3  Selection-status detection (006) ----------------------------------------
# In the live library, exclusion reasons are appended to the tag string with
# a trailing dash (e.g. "006-Exclude-OffTopic-not peer-reviewed").
# We parse decision and reason into separate columns here.

status_006 <- tags_struct |>
  filter(family == "006") |>
  mutate(
    decision = case_when(
      value == "Include-Stage1"             ~ "Include-Stage1",
      value == "Include-Stage2"             ~ "Include-Stage2",
      value == "Include-Final"              ~ "Include-Final",
      str_starts(value, "Exclude-OffTopic") ~ "Exclude-OffTopic",
      str_starts(value, "Exclude-Quality")  ~ "Exclude-Quality",
      str_starts(value, "Exclude-NoFullText") ~ "Exclude-NoFullText",
      TRUE                                  ~ "Other"
    ),
    reason = case_when(
      str_starts(value, "Exclude-OffTopic-") ~ str_remove(value, "^Exclude-OffTopic-"),
      str_starts(value, "Exclude-Quality-")  ~ str_remove(value, "^Exclude-Quality-"),
      decision == "Exclude-NoFullText"       ~ "full text not retrievable",
      TRUE                                   ~ NA_character_
    )
  )


# 1.4  Build the cohort table --------------------------------------------------
# One row per record, with bibliographic basics + selection-stage flags.

cohort <- zotero_raw |>
  select(Key, Author, Title, `Publication Year`, `Publication Title`,
         `Item Type`, DOI) |>
  rename(
    pub_year  = `Publication Year`,
    journal   = `Publication Title`,
    item_type = `Item Type`
  ) |>
  left_join(
    status_006 |>
      group_by(Key) |>
      summarise(
        in_pipeline    = TRUE,
        stage1_in      = any(decision == "Include-Stage1"),
        stage2_in      = any(decision == "Include-Stage2"),
        final_in       = any(decision == "Include-Final"),
        exclude_at     = case_when(
          any(decision == "Include-Final")        ~ NA_character_,
          any(decision %in% c("Exclude-Quality",
                              "Exclude-NoFullText")) ~ "Stage-2",
          any(decision == "Exclude-OffTopic") &
            !any(decision == "Include-Stage1")  ~ "Stage-1",
          TRUE                                    ~ NA_character_
        ),
        exclude_reason = first(reason[!is.na(reason)], default = NA_character_),
        .groups = "drop"
      ),
    by = "Key"
  ) |>
  mutate(across(c(in_pipeline, stage1_in, stage2_in, final_in),
                ~ coalesce(.x, FALSE)))


# 1.5  Wide-form content-coding ------------------------------------------------
# One list-column per tag family (tag_002, tag_003, ...).

content_wide <- tags_struct |>
  filter(family %in% c("002", "003", "004", "005",
                       "010", "020", "030", "090", "091")) |>
  group_by(Key, family) |>
  summarise(values = list(value), .groups = "drop") |>
  pivot_wider(names_from = family, values_from = values,
              names_prefix = "tag_")

corpus       <- cohort |> left_join(content_wide, by = "Key")
final_corpus <- corpus |> filter(final_in)


# 1.6  002-family derived columns (Multi-Type handling) ------------------------
# Per user decision (2026-06-18): 002 is treated as multi-value. We expose
# both views - Coverage (one count per tag) and Combination (UpSet-ready).

final_corpus <- final_corpus |>
  mutate(
    infra_n_tags     = lengths(tag_002),
    infra_complexity = case_when(
      infra_n_tags == 0 ~ "None",
      infra_n_tags == 1 ~ "Single-Type",
      infra_n_tags  > 1 ~ "Multi-Type"
    ) |> factor(levels = c("Single-Type", "Multi-Type", "None")),
    infra_combo = map_chr(tag_002,
                          ~ if (length(.x) == 0) NA_character_
                          else paste(sort(.x), collapse = " + "))
  )


# 1.7  Tag-vocabulary conformance check ----------------------------------------
# Compares every observed tag value against permitted_tags from section 0.5.
# Drift is logged but does not abort execution - manual review at end.

conformance <- tags_struct |>
  filter(family %in% names(permitted_tags)) |>
  distinct(family, value) |>
  rowwise() |>
  mutate(permitted = value %in% permitted_tags[[family]]) |>
  ungroup() |>
  arrange(family, !permitted, value)

drift <- conformance |> filter(!permitted)

if (nrow(drift) > 0) {
  message("\n[!] Tag conformance drift: ", nrow(drift),
          " unexpected value(s) found")
  print(drift, n = Inf)
} else {
  message("\n[ok] All structured tag values conform to the permitted vocabulary.")
}


# 1.8  Pipeline summary --------------------------------------------------------

n_total       <- nrow(corpus)
n_no_pipeline <- sum(!corpus$in_pipeline)
n_stage1_in   <- sum(corpus$stage1_in)
n_stage1_out  <- sum(corpus$exclude_at == "Stage-1", na.rm = TRUE)
n_stage2_in   <- sum(corpus$stage2_in)
n_stage2_out  <- sum(corpus$exclude_at == "Stage-2", na.rm = TRUE)
n_final       <- sum(corpus$final_in)
n_manual_add  <- tags_struct |>
  filter(family == "091", value == "ManualAdd") |>
  semi_join(filter(corpus, final_in), by = "Key") |>
  nrow()

cat("\n========================================\n")
cat("Pipeline summary\n")
cat("========================================\n")
cat(sprintf("  Total records imported       %4d\n", n_total))
cat(sprintf("  Records without 006- tag     %4d  (outside pipeline)\n", n_no_pipeline))
cat(sprintf("  Stage-1 Include              %4d\n", n_stage1_in))
cat(sprintf("  Stage-1 Exclude              %4d\n", n_stage1_out))
cat(sprintf("  Stage-2 Include / Final      %4d\n", n_final))
cat(sprintf("  Stage-2 Exclude              %4d\n", n_stage2_out))
cat(sprintf("  Manual-add records (Final)   %4d\n", n_manual_add))


# 1.9  Anchor-literature check -------------------------------------------------
# Sanity check: every anchor study from Briefing §10 should be in the Final
# corpus. Missing anchors typically indicate a filter bug, not a scope issue.

anchors <- c("Wemple", "La Marche", "Soulis", "Croke",
             "Hairsine", "Kastridis", "Bracken", "Borselli")

anchor_check <- map_dfr(anchors, function(a) {
  hits <- final_corpus |>
    filter(str_detect(Author, fixed(a, ignore_case = TRUE)))
  tibble(
    anchor      = a,
    n_in_final  = nrow(hits),
    sample_keys = if (nrow(hits) == 0) NA_character_
    else paste(head(hits$Key, 3), collapse = ", ")
  )
})

cat("\nAnchor-literature check (Final corpus):\n")
print(anchor_check)


# 1.10  Optional: persist processed tables -------------------------------------
# Uncomment if you want a CSV copy of the cleaned cohort for inspection.

# write_csv(corpus,       file.path(tables_dir, "corpus_all.csv"))
# write_csv(final_corpus, file.path(tables_dir, "corpus_final.csv"))
# write_csv(conformance,  file.path(tables_dir, "tag_conformance.csv"))

message("\nImport complete. Final corpus N = ", n_final,
        ". Ready for section 02 (PRISMA).")


# 02  PRISMA FLOW DIAGRAM  =====================================================

# 2.1  Aggregate PRISMA counts -------------------------------------------------
# Logic of the count chain (matches the real Zotero workflow):
#   (a) Search strings executed in three databases; hits saved into Zotero
#       with one 001- tag per database. Records that surfaced in multiple
#       databases received multiple 001- tags (intra-library deduplication
#       happens implicitly because the same Zotero entry holds all source
#       tags rather than being re-imported).
#   (b) PRISMA "Records identified from databases" is therefore the SUM of
#       all 001- tag occurrences (= Coverage-logic total, with overlaps).
#   (c) PRISMA "Records after deduplication" is the count of UNIQUE records
#       that carry at least one 001- tag. The difference equals the
#       cross-database overlap (records found by >1 database).
# All numbers are recomputed from `corpus`, so the diagram updates if
# records are re-screened or re-tagged.

# Per-database hit counts (one row per 001- tag, Coverage logic)
db_counts <- tags_struct |>
  filter(family == "001") |>
  semi_join(filter(corpus, in_pipeline), by = "Key") |>
  count(value, name = "n") |>
  arrange(desc(n))

n_scopus  <- db_counts |> filter(value == "Scopus")        |> pull(n)
n_wos     <- db_counts |> filter(value == "WoS")           |> pull(n)
n_scholar <- db_counts |> filter(value == "GoogleScholar") |> pull(n)

# Total database hits before deduplication (sum across databases)
n_db_hits_total <- sum(n_scopus, n_wos, n_scholar)

# Unique records identified via databases
n_db_unique <- tags_struct |>
  filter(family == "001") |>
  semi_join(filter(corpus, in_pipeline), by = "Key") |>
  distinct(Key) |>
  nrow()

# Cross-database duplicates removed (Coverage overlap)
n_dedup_removed <- n_db_hits_total - n_db_unique

# Snowballing path: records added through citation searching
n_snowball <- tags_struct |>
  filter(family == "091", value == "ManualAdd") |>
  semi_join(filter(corpus, final_in), by = "Key") |>
  nrow()

# Screening / eligibility / included counts
n_screened   <- sum(corpus$in_pipeline)
n_s1_exclude <- sum(corpus$exclude_at == "Stage-1", na.rm = TRUE)
n_s1_include <- sum(corpus$stage1_in)
n_s2_exclude <- sum(corpus$exclude_at == "Stage-2", na.rm = TRUE)
# PRISMA 2020: reports sought for retrieval, reports not retrieved, reports assessed
n_not_retr   <- sum(corpus$exclude_at == "Stage-2" &
                    corpus$exclude_reason == "full text not retrievable", na.rm = TRUE)
n_assessed   <- n_s1_include - n_not_retr
n_s2_elig_ex <- n_s2_exclude - n_not_retr
n_final      <- sum(corpus$final_in)
n_final_db   <- n_final - n_snowball

cat(sprintf("PRISMA counts:  DB hits %d  -> unique %d  (%d cross-DB dups)\n",
            n_db_hits_total, n_db_unique, n_dedup_removed))
cat(sprintf("                + %d snowballing  ->  screened %d\n",
            n_snowball, n_screened))

# Tidy a few reason strings before display (fix a typo, shorten long phrases)
clean_reason <- function(x) {
  x |>
    str_replace_all("not accessable", "neither abstract nor full text accessible") |>
    str_replace_all("connectivity only narrative, not operationalized",
                    "narrative connectivity only") |>
    str_replace_all("connectivity not operationalized as outcome",
                    "outcome not operationalized") |>
    str_replace_all("infrastructure mentioned as context only",
                    "infrastructure as context only") |>
    str_replace_all("no Block-B infrastructure as variable",
                    "Block-B infrastructure not a variable")
}

# Reason counts per stage, ordered by frequency
s1_reasons <- corpus |>
  filter(exclude_at == "Stage-1") |>
  mutate(reason = clean_reason(coalesce(exclude_reason, "(no reason)"))) |>
  count(reason, name = "n", sort = TRUE)

s2_reasons <- corpus |>
  filter(exclude_at == "Stage-2", exclude_reason != "full text not retrievable") |>
  mutate(reason = clean_reason(coalesce(exclude_reason,
                                        "no full text retrievable"))) |>
  count(reason, name = "n", sort = TRUE)

# Format reasons as multi-line text blocks for the side boxes
format_excludes <- function(df, header) {
  body <- sprintf("  -  %s  (n = %d)", df$reason, df$n)
  paste(c(header, "", body), collapse = "\n")
}
s1_box_text <- format_excludes(
  s1_reasons,
  sprintf("Records excluded (n = %d)", n_s1_exclude)
)
s2_box_text <- format_excludes(
  s2_reasons,
  sprintf("Reports excluded (n = %d)", n_s2_elig_ex)
)


# 2.2  Box and arrow layout ----------------------------------------------------
# Three-column layout that mirrors the real workflow:
#   Main column (x ~ 6) - database identification → screening → eligibility
#   Side column (x ~ 14.5) - exclude side-boxes (cross-DB dups, S1, S2)
#   Other column (x ~ 18) - snowballing path that re-joins at the Included box
# Coordinate system: x in [0, 20], y in [1, 21]. coord_fixed() preserves aspect.

# Fill colours (PRISMA stage conventions, low saturation for print legibility)
fill_id     <- "#E1F0F4"   # Identification (light blue)
fill_screen <- "#FFF3CD"   # Screening (light yellow)
fill_elig   <- "#D1ECF1"   # Eligibility (light cyan)
fill_incl   <- "#C3E6CB"   # Included (light green)
fill_excl   <- "#F5F5F5"   # Side / excluded boxes (neutral grey)
fill_other  <- "#F2E2D9"   # Other methods / snowballing (warm beige)

boxes <- tribble(
  ~id,         ~cx,  ~cy,   ~w,   ~h,  ~label,                                                                                                   ~fill,
  "db_scop",    3.0, 19.9,  2.8, 1.3, sprintf("Scopus\nn = %d", n_scopus),                                                                      fill_id,
  "db_wos",     6.0, 19.9,  2.8, 1.3, sprintf("Web of Science\nn = %d", n_wos),                                                                 fill_id,
  "db_sch",     9.0, 19.9,  2.8, 1.3, sprintf("Google Scholar\nn = %d", n_scholar),                                                             fill_id,
  "other",     19.0, 19.9,  3.6, 1.7, sprintf("Other methods\n(citation tracking)\nn = %d", n_snowball),                                      fill_other,
  "ident_tot",  6.0, 17.6,  8.0, 1.7, sprintf("Records identified from databases\n(total hits, incl. cross-DB duplicates)\nn = %d", n_db_hits_total), fill_id,
  "dedup",     14.7, 17.6,  7.2, 1.6, sprintf("Records removed before screening:\nCross-database duplicates\nn = %d", n_dedup_removed),        fill_excl,
  "ident_uni",  6.0, 15.6,  8.0, 1.3, sprintf("Records identified (unique)\nn = %d", n_db_unique),                                              fill_id,
  "screen",     6.0, 12.8,  8.0, 1.8, sprintf("Records screened (title / abstract)\nn = %d\n(%d from databases, %d via citation tracking)",
                                              n_screened, n_screened - n_snowball, n_snowball),                                                 fill_screen,
  "s1_excl",   14.7, 11.95, 7.2, 4.4, s1_box_text,                                                                                              fill_excl,
  "sought",     6.0,  8.7,  8.0, 1.3, sprintf("Reports sought for retrieval\nn = %d", n_s1_include),                                           fill_elig,
  "notretr",   14.7,  8.7,  7.2, 1.3, sprintf("Reports not retrieved\nn = %d", n_not_retr),                                                    fill_excl,
  "fulltxt",    6.0,  5.9,  8.0, 1.3, sprintf("Reports assessed for eligibility\nn = %d", n_assessed),                                          fill_elig,
  "s2_excl",   14.7,  5.4,  7.2, 3.6, s2_box_text,                                                                                              fill_excl,
  "incl",       6.0,  2.1,  8.0, 1.9, sprintf("Studies included in synthesis\nn = %d\n(%d from databases, %d via citation tracking)",
                                              n_final, n_final_db, n_snowball),                                                                 fill_incl
)

# Arrows WITH arrow head (pipeline directionality)
arrows <- tribble(
  ~x1,   ~y1,    ~x2,    ~y2,
  3.0,   19.25,   4.0,   18.45,    # Scopus  -> ident_tot
  6.0,   19.25,   6.0,   18.45,    # WoS     -> ident_tot
  9.0,   19.25,   8.0,   18.45,    # Scholar -> ident_tot
  10.0,  17.6,   11.1,   17.6,     # ident_tot -> dedup
  6.0,   16.75,   6.0,   16.25,    # ident_tot -> ident_uni
  6.0,   14.95,   6.0,   13.70,    # ident_uni -> screen
  19.0,  14.45,   6.08,  14.45,    # citation tracking joins before screening
  10.0,  12.8,   11.1,   12.8,     # screen -> s1_excl
  6.0,   11.90,   6.0,    9.35,    # screen -> sought
  10.0,   8.7,   11.1,    8.7,     # sought -> not retrieved
  6.0,    8.05,   6.0,    6.55,    # sought -> assessed
  10.0,   5.9,   11.1,    5.9,     # assessed -> s2_excl
  6.0,    5.25,   6.0,    3.05     # assessed -> incl
)

arrows_plain <- tribble(
  ~x1,   ~y1,    ~x2,    ~y2,
  19.0,  19.05,  19.0,   14.45
)


# 2.3  Render the diagram ------------------------------------------------------

p_prisma <- ggplot() +
  # Box rectangles
  geom_rect(
    data = boxes,
    aes(xmin = cx - w/2, xmax = cx + w/2,
        ymin = cy - h/2, ymax = cy + h/2,
        fill = fill),
    colour = "grey25", linewidth = 0.4
  ) +
  # Main-flow box labels (centred)
  geom_text(
    data = boxes |> filter(!str_detect(id, "excl")),
    aes(x = cx, y = cy, label = label),
    size = 2.7, lineheight = 1.10
  ) +
  # Side-box labels (top-left anchored so the bullet list reads cleanly)
  geom_text(
    data = boxes |> filter(str_detect(id, "excl")),
    aes(x = cx - w/2 + 0.25, y = cy + h/2 - 0.30, label = label),
    size = 2.2, lineheight = 1.20, hjust = 0, vjust = 1
  ) +
  # Snowballing vertical drop (plain connector, no arrow head)
  geom_segment(
    data = arrows_plain,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    linewidth = 0.4, colour = "grey25"
  ) +
  # Pipeline arrows (with arrow head)
  geom_segment(
    data = arrows,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.4, colour = "grey25"
  ) +
  scale_fill_identity() +
  coord_fixed(xlim = c(0.5, 21), ylim = c(0.9, 21)) +
  labs(
    title    = "PRISMA",
    subtitle = "AquaSilva connectivity-anchored systematic literature review"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(colour = "grey30", size = 9, hjust = 0),
    plot.caption  = element_text(colour = "grey45", size = 7, hjust = 0,
                                 lineheight = 1.25,
                                 margin = margin(t = 10)),
    plot.margin   = margin(8, 8, 18, 8)
  )

save_plot(p_prisma, "02_prisma_flow", width_cm = W_FULL, height_cm = 18)

message("\nPRISMA flow rendered. ",
        "File: figures/02_prisma_flow.{pdf,png}")



# 02b  SEARCH STRATEGY PERFORMANCE  ============================================
#
# Coverage of the search architecture as preserved in the Zotero library.
# Each record carries:
#   - one 001-tag per database that retrieved it
#   - one 090-Hxx (Baseline) or 090-Mxx (Mitigation) tag per query that hit it
# These provenance tags were assigned mechanically at import (Briefing 3.1).
# The library is therefore the single source of truth — no external file
# (e.g. PSALSAR_SearchStringsOverview_final.csv) is consulted here.
#
# WHAT THE LIBRARY CAN SHOW (post-deduplication):
#   - Unique records each search string contributed (= 090-tag occurrences)
#   - Database-by-database coverage of each search string (intersection counts)
#   - Block-A × Block-B (× Block-C for M) decomposition via the query ID map
#
# WHAT THE LIBRARY CANNOT SHOW (lost at import-time deduplication):
#   - Raw hit counts per search-run before dedup
#   - Within-database cross-string overlap (a record retrieved by H01-Scopus
#     AND H04-Scopus appears once in the library with both 090- tags)
#
# Coverage logic: a record retrieved by multiple databases appears under each
# database tag, so stack heights count (record × database) pairs; the bar
# total can exceed the unique-record count. The companion table reports both.


# 2b.1  Fixed mapping: query ID -> block combination --------------------------
# This mapping is set by the PSALSAR protocol (Briefing_Methodik §5.3) and
# only changes if the protocol itself is revised.

query_block_map <- tribble(
  ~search_id, ~a_block, ~b_block, ~c_block,
  "H01",      "A1",      "B1",     NA_character_,
  "H02",      "A1",      "B2",     NA_character_,
  "H03",      "A1",      "B3",     NA_character_,
  "H04",      "A2",      "B1",     NA_character_,
  "H05",      "A2",      "B2",     NA_character_,
  "H06",      "A2",      "B3",     NA_character_,
  "H07",      "A3",      "B1",     NA_character_,
  "H08",      "A3",      "B2",     NA_character_,
  "H09",      "A3",      "B3",     NA_character_,
  "M01",      "A1",      "B1",     "C",
  "M02",      "A1",      "B2",     "C",
  "M03",      "A1",      "B3",     "C",
  "M04",      "A2",      "B1",     "C",
  "M05",      "A2",      "B2",     "C",
  "M06",      "A2",      "B3",     "C",
  "M07",      "A3",      "B1",     "C",
  "M08",      "A3",      "B2",     "C",
  "M09",      "A3",      "B3",     "C"
) |>
  mutate(
    query_type = str_sub(search_id, 1, 1),
    combo      = if_else(is.na(c_block),
                         paste0(a_block, "\u00d7", b_block),
                         paste0(a_block, "\u00d7", b_block, "\u00d7C"))
  )


# 2b.2  Aggregate (search-string \u00d7 database) coverage from the tags ----------

# Records that carry any 090-Hxx or 090-Mxx tag
search_tagged <- tags_struct |>
  filter(family == "090",
         str_detect(value, "^[HM]\\d{2}$")) |>
  rename(search_id = value) |>
  select(Key, search_id) |>
  distinct()

# Database tags per record
db_tagged <- tags_struct |>
  filter(family == "001") |>
  rename(db = value) |>
  select(Key, db) |>
  distinct()

# Intersection: distinct records sharing a (search-string, database) tag pair
search_runs <- search_tagged |>
  inner_join(db_tagged, by = "Key", relationship = "many-to-many") |>
  count(search_id, db, name = "n_records")

# Unique records per search string (= 090-tag occurrence count)
search_unique <- search_tagged |>
  count(search_id, name = "n_unique")

# Stacked bar height per search string (can exceed n_unique when cross-DB
# coverage is present; that overlap is the methodological message we surface)
search_bar_height <- search_runs |>
  group_by(search_id) |>
  summarise(n_stack = sum(n_records), .groups = "drop")

# Per-string summary (block decomposition + coverage metrics)
search_summary <- query_block_map |>
  left_join(search_unique,     by = "search_id") |>
  left_join(search_bar_height, by = "search_id") |>
  mutate(
    n_unique    = coalesce(n_unique, 0L),
    n_stack     = coalesce(n_stack,  0L),
    overlap_pct = if_else(n_unique > 0,
                          round(100 * (n_stack - n_unique) / n_unique, 0),
                          NA_real_)
  )

# Console sanity print
n_records_total <- n_distinct(search_tagged$Key)
n_pairs_total   <- sum(search_runs$n_records)
n_records_h     <- search_tagged |>
  filter(str_starts(search_id, "H")) |> pull(Key) |> n_distinct()
n_records_m     <- search_tagged |>
  filter(str_starts(search_id, "M")) |> pull(Key) |> n_distinct()

cat(sprintf("Search coverage (from library tags):\n"))
cat(sprintf("  %d unique records carry at least one search-string tag\n",
            n_records_total))
cat(sprintf("  %d (record \u00d7 database) pairs across all 18 queries\n",
            n_pairs_total))
cat(sprintf("  H-queries cover %d unique records, M-queries cover %d\n",
            n_records_h, n_records_m))


# 2b.3  Stacked bar plot ------------------------------------------------------

id_levels <- query_block_map$search_id
n_h <- 9L
n_m <- 9L

search_runs <- search_runs |>
  mutate(
    search_id = factor(search_id, levels = id_levels),
    db        = factor(db, levels = c("Scopus", "WoS", "GoogleScholar"))
  )
search_summary <- search_summary |>
  mutate(search_id = factor(search_id, levels = id_levels))

# Database palette (Okabe-Ito triad, consistent across the report)
pal_db <- c(
  "Scopus"        = okabe_ito[2],   # blue
  "WoS"           = okabe_ito[3],   # green
  "GoogleScholar" = okabe_ito[1]    # orange
)

# x-axis labels: Search-ID only. The A×B[×C] sub-component enumeration moved to
# the LaTeX caption (the per-bar second line collided at 18 bars / full width).
# v3 (Oct 2026, reviewer C016/C019): two-line labels give the block combination
# under each query ID, so the figure explains itself without the caption.
x_label_vec <- setNames(
  paste0(query_block_map$search_id, "\n",
         query_block_map$a_block, "\u00b7", query_block_map$b_block),
  query_block_map$search_id
)

# Headroom for on-top labels + group brackets
y_max     <- max(search_summary$n_stack)
y_bracket <- y_max * 1.18
y_label   <- y_max * 1.28

p_search <- ggplot() +
  # H / M block banding
  annotate("rect",
           xmin = 0.4, xmax = n_h + 0.5,
           ymin = -Inf, ymax = Inf, fill = "grey96") +
  annotate("rect",
           xmin = n_h + 0.5, xmax = n_h + n_m + 0.6,
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  # Stacked bars by database
  geom_col(
    data = search_runs,
    aes(x = search_id, y = n_records, fill = db),
    width = 0.72, colour = "white", linewidth = 0.25
  ) +
  # On-top label = stack total
  geom_text(
    data = search_summary,
    aes(x = search_id, y = n_stack, label = n_stack),
    vjust = -0.6, size = 2.9, fontface = "bold", colour = "grey20"
  ) +
  # H/M group brackets and headers
  annotate("segment",
           x = 0.6, xend = n_h + 0.4,
           y = y_bracket, yend = y_bracket,
           colour = "grey40", linewidth = 0.3) +
  annotate("segment",
           x = n_h + 0.6, xend = n_h + n_m + 0.4,
           y = y_bracket, yend = y_bracket,
           colour = "grey40", linewidth = 0.3) +
  annotate("text",
           x = (1 + n_h) / 2, y = y_label,
           label = "Baseline queries H (A \u00d7 B \u00d7 D)",
           size = 3.0, fontface = "bold", colour = "grey20") +
  annotate("text",
           x = n_h + (1 + n_m) / 2, y = y_label,
           label = "Mitigation queries M (A \u00d7 B \u00d7 C \u00d7 D)",
           size = 3.0, fontface = "bold", colour = "grey20") +
  scale_fill_manual(
    values = pal_db,
    name   = "Database",
    breaks = c("Scopus", "WoS", "GoogleScholar"),
    labels = c("Scopus", "Web of Science", "Google Scholar")
  ) +
  scale_x_discrete(labels = x_label_vec) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)),
                     breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title    = "Search strategy coverage",
    subtitle = sprintf(
      "%d record\u00d7database pairs \u00b7 18 strings (9 H + 9 M) \u00b7 3 databases \u00b7 %d unique records",
      n_pairs_total, n_records_total
    ),
    x = NULL,
    y = "Records identified per (search string \u00d7 database)"
  ) +
  theme_aquasilva(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    axis.text.x        = element_text(size = 6, lineheight = 0.9),
    plot.subtitle      = element_text(colour = "grey30", size = 8, hjust = 0),
    plot.caption       = element_text(colour = "grey45", size = 7, hjust = 0,
                                      lineheight = 1.3, margin = margin(t = 10)),
    plot.margin        = margin(8, 10, 16, 8)
  )

save_plot(p_search, "02b_search_strategy_bar", width_cm = W_FULL, height_cm = 11)


# 2b.4  Companion table ------------------------------------------------------
# Wide form per search string with per-DB counts, block decomposition,
# unique count, stack total, and cross-database overlap percentage.

search_table_wide <- search_runs |>
  mutate(search_id = as.character(search_id),
         db        = as.character(db)) |>
  pivot_wider(names_from = db, values_from = n_records, values_fill = 0L) |>
  full_join(query_block_map |>
              select(search_id, query_type, a_block, b_block, c_block, combo),
            by = "search_id") |>
  full_join(search_summary |>
              select(search_id, n_unique, n_stack, overlap_pct),
            by = "search_id") |>
  mutate(across(any_of(c("Scopus", "WoS", "GoogleScholar")),
                ~ coalesce(.x, 0L))) |>
  select(`Search-ID`           = search_id,
         `Query type`          = query_type,
         `A-Block`             = a_block,
         `B-Block`             = b_block,
         `C-Block`             = c_block,
         `Block combination`   = combo,
         Scopus,
         `Web of Science`      = WoS,
         `Google Scholar`      = GoogleScholar,
         `Unique records`      = n_unique,
         `Sum across DBs`      = n_stack,
         `Cross-DB overlap (%)` = overlap_pct) |>
  arrange(`Search-ID`)

write_csv(search_table_wide, file.path(tables_dir, "02b_search_strategy_table.csv"))

message("\nSearch strategy coverage rendered.\n",
        "  Plot:  figures/02b_search_strategy_bar.{pdf,png}\n",
        "  Table: tables/02b_search_strategy_table.csv")




# 03  RQ1 — LITERATURE CHARACTERISTICS  ========================================
#
# Four plots from the Final corpus (n = 58):
#   RQ1a  Publication volume over time
#   RQ1b  Geographic distribution by region
#   RQ1c  Dominant journals
#   RQ1d  Study-type distribution
#
# All plots use 005- / 003- content-coding tags from section 01.

# 3.0  Final-corpus extracts ---------------------------------------------------

# Normalise journal name casing (Zotero exports occasionally mix ALL-CAPS and
# Title Case for the same journal — they need to be merged before counting).
prettify_journal <- function(x) {
  x |>
    str_squish() |>
    str_to_title() |>
    str_replace_all("\\bAnd\\b", "and") |>
    str_replace_all("\\bOf\\b",  "of")  |>
    str_replace_all("\\bThe\\b", "the") |>
    str_replace_all("\\bA\\b",   "a")
}

rq_corpus <- final_corpus |>
  mutate(
    pub_year     = as.integer(pub_year),
    journal_norm = prettify_journal(coalesce(journal, "Unknown"))
  )

n_final_disp <- nrow(rq_corpus)


# 3.1  RQ1a  Publication volume over time ------------------------------------
# One statement per plot: the annual publication count. The cumulative line and
# secondary axis were removed in v2 (they layered a second message onto the same
# panel). Methodological anchor events are kept as slim dashed markers with
# year-only labels; the full event descriptions belong in the LaTeX caption.

year_range <- range(rq_corpus$pub_year, na.rm = TRUE)
rq1a_data <- tibble(pub_year = seq(year_range[1], year_range[2])) |>
  left_join(rq_corpus |> count(pub_year, name = "n"), by = "pub_year") |>
  mutate(n = coalesce(n, 0L))

max_n <- max(rq1a_data$n)

# Methodological anchor events (Briefing §5.2). 2007/2008 share one marker.
anchors_v <- tribble(
  ~year, ~label,
  1996,  "1996",
  2008,  "2007/08",
  2013,  "2013"
) |>
  filter(year >= year_range[1], year <= year_range[2])

p_rq1a <- ggplot(rq1a_data, aes(x = pub_year, y = n)) +
  geom_vline(data = anchors_v, aes(xintercept = year),
             linetype = "dashed", colour = "grey60", linewidth = 0.3) +
  geom_col(fill = okabe_ito[2], width = 0.85) +
  geom_text(data = anchors_v,
            aes(x = year, y = max_n * 1.06, label = label),
            inherit.aes = FALSE, vjust = 0, size = 2.2, colour = "grey40") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(name = "Publications per year",
                     breaks = scales::breaks_pretty(n = 5),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "RQ1a - Publication volume over time",
    subtitle = sprintf("Final corpus, n = %d \u00b7 annual count", n_final_disp),
    x        = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(plot.caption = element_text(colour = "grey45", size = 6.5, hjust = 0,
                                    margin = margin(t = 6)))

save_plot(p_rq1a, "03a_rq1a_temporal", width_cm = W_COL, height_cm = 7)


# 3.2  RQ1b  Geographic distribution ------------------------------------------
# v2: the Robinson choropleth was dropped (it hid a 7-row fact behind a full
# page of empty ocean). A plain horizontal bar is read instantly. All permitted
# regions are shown, including those with zero records, so coverage gaps (Africa)
# are visible rather than silently absent. 005- is single-valued per record,
# so bars sum to n and no Coverage caveat is needed.

region_levels <- permitted_tags[["005"]]

rq1b_data <- rq_corpus |>
  select(Key, tag_005) |>
  unnest_longer(tag_005, values_to = "region") |>
  filter(!is.na(region)) |>
  count(region, name = "n") |>
  complete(region = region_levels, fill = list(n = 0L)) |>
  mutate(region = fct_reorder(region, n))

p_rq1b <- ggplot(rq1b_data, aes(x = n, y = region, fill = as.character(region))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.7,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = pal_region, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "RQ1b - Study region",
    subtitle = sprintf("Final corpus, n = %d \u00b7 one region per record", n_final_disp),
    x        = "Records", y = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(colour = "grey45", size = 6.5, hjust = 0,
                                margin = margin(t = 6))
  )


save_plot(p_rq1b, "03b_rq1b_region", width_cm = W_COL, height_cm = 7)


# 3.3  RQ1c  Dominant journals -----------------------------------------------
# Top-10 + pooled "Other" bar (axis names truncated to fit column width); the
# FULL journal list with untruncated names goes to a companion CSV so nothing
# is lost to the "Other" bin (Test 6).

rq1c_raw <- rq_corpus |>
  count(journal_norm, name = "n", sort = TRUE) |>
  mutate(journal_norm = str_replace(
    journal_norm,
    "^Jawra Journal of the American Water Resources Association$",
    "Journal of the American Water Resources Association"
  )) |>
  group_by(journal_norm) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(desc(n))

# Companion table: every journal, full name, count (single source for the bar)
rq1c_raw |>
  rename(Journal = journal_norm, Records = n) |>
  write_csv(file.path(tables_dir, "03c_rq1c_journals_full.csv"))

top_n <- 10L
rq1c_top  <- rq1c_raw |> slice_head(n = top_n)
rq1c_rest <- rq1c_raw |> slice_tail(n = max(0, nrow(rq1c_raw) - top_n))

rq1c_plot_data <- if (nrow(rq1c_rest) > 0) {
  bind_rows(
    rq1c_top,
    tibble(journal_norm = sprintf("Other (%d journals)", nrow(rq1c_rest)),
           n            = sum(rq1c_rest$n))
  )
} else rq1c_top

rq1c_plot_data <- rq1c_plot_data |>
  mutate(
    is_other     = str_starts(journal_norm, "Other"),
    label_short  = if_else(is_other, journal_norm, str_trunc(journal_norm, 32)),
    label_short  = fct_reorder(label_short, n)
  )

p_rq1c <- ggplot(rq1c_plot_data, aes(x = n, y = label_short, fill = is_other)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.6, fontface = "bold",
            colour = "grey25") +
  scale_fill_manual(values = c(`FALSE` = okabe_ito[2], `TRUE` = "grey70"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = sprintf("RQ1c - Top %d journals", top_n),
    subtitle = sprintf("Final corpus, n = %d", n_final_disp),
    x        = "Records", y = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text(size = 7),
    plot.caption = element_text(colour = "grey45", size = 6.5, hjust = 0,
                                margin = margin(t = 6))
  )

save_plot(p_rq1c, "03c_rq1c_journals", width_cm = W_COL, height_cm = 8)


# 3.4  RQ1d  Study-type distribution -----------------------------------------
# v2: the UpSet plot was replaced by a Coverage bar + companion table (decision
# 2026-06-22). 003- is only 21% multi-tag and has 8 distinct combinations, so a
# bar is clearer and stays inside theme_aquasilva. The bar counts each tag
# occurrence (Coverage); because 12/58 records carry two method tags, the bars
# sum to more than n — stated explicitly. Reviews are set apart in grey because
# they are treated differently in the RQ5 synthesis. The exact tag combinations
# are written to a companion CSV.

study_levels <- c("Modeling", "FieldMonitoring", "Experimental",
                  "Review", "PairedCatchment")

rq1d_cov <- rq_corpus |>
  select(Key, tag_003) |>
  unnest_longer(tag_003, values_to = "study") |>
  filter(!is.na(study)) |>
  count(study, name = "n") |>
  complete(study = study_levels, fill = list(n = 0L)) |>
  mutate(study = fct_reorder(study, n))

n_multi_003 <- rq_corpus |>
  mutate(k = lengths(tag_003)) |> filter(k > 1) |> nrow()

write_combo_table(rq_corpus, "tag_003", "03d_rq1d_studytype_combos.csv")

p_rq1d <- ggplot(rq1d_cov, aes(x = n, y = study, fill = as.character(study))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.7,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = pal_studytype, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "RQ1d - Study type (Coverage)",
    subtitle = sprintf("n = %d \u00b7 %d%% use two methods",
                       n_final_disp, round(100 * n_multi_003 / n_final_disp)),
    x        = "Records reporting each type", y = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(colour = "grey45", size = 6.5, hjust = 0,
                                lineheight = 1.3, margin = margin(t = 6))
  )

save_plot(p_rq1d, "03d_rq1d_studytype", width_cm = W_COL, height_cm = 7)

message("\nRQ1 plots rendered:\n",
        "  figures/03a_rq1a_temporal.{pdf,png}\n",
        "  figures/03b_rq1b_region.{pdf,png}\n",
        "  figures/03c_rq1c_journals.{pdf,png}  + tables/03c_rq1c_journals_full.csv\n",
        "  figures/03d_rq1d_studytype.{pdf,png} + tables/03d_rq1d_studytype_combos.csv")




# 04  RQ2 — INFRASTRUCTURE TYPE  ==============================================
#
# v2: UpSet (04a) + linear/structural bar (04b) merged into one figure —
# a Coverage bar + a slim class-membership strip + a combination CSV.
# 002- is 59% multi-tag, so Coverage bars sum to >n (stated). RetentionDetention
# (n = 1) is the AquaSilva-relevant infrastructure class and is left visible as
# a near-empty bar rather than dropped; that under-representation is a finding,
# discussed in the running text, not asserted in the caption.
# RQ2b (density, age, design, condition) needs the extraction Excel — deferred.

infra_levels     <- c("Roads", "Structures", "ExtractionTrails",
                      "RoadDrainageBMP", "RetentionDetention")
linear_types     <- c("Roads", "ExtractionTrails")
structural_types <- c("Structures", "RoadDrainageBMP", "RetentionDetention")

# Coverage counts (one count per tag occurrence)
rq2_cov <- rq_corpus |>
  select(Key, tag_002) |>
  unnest_longer(tag_002, values_to = "infra") |>
  filter(!is.na(infra)) |>
  count(infra, name = "n") |>
  complete(infra = infra_levels, fill = list(n = 0L)) |>
  mutate(infra = fct_reorder(infra, n))

n_multi_002 <- rq_corpus |>
  mutate(k = lengths(tag_002)) |> filter(k > 1) |> nrow()

# Linear vs structural class membership (per record, no double-count)
rq2_class <- rq_corpus |>
  mutate(
    has_lin = map_lgl(tag_002, ~ any(.x %in% linear_types)),
    has_str = map_lgl(tag_002, ~ any(.x %in% structural_types))
  ) |>
  filter(has_lin | has_str) |>
  mutate(class = case_when(
    has_lin & has_str ~ "Both",
    has_lin           ~ "Linear only",
    TRUE              ~ "Structural only"
  )) |>
  count(class, name = "n") |>
  complete(class = c("Linear only", "Both", "Structural only"),
           fill = list(n = 0L)) |>
  mutate(class = factor(class, levels = c("Structural only", "Both", "Linear only")))

write_combo_table(rq_corpus, "tag_002", "04_rq2a_infra_combos.csv")

# Panel A — per-type Coverage
p_infra_cov <- ggplot(rq2_cov, aes(x = n, y = infra, fill = as.character(infra))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.7,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = pal_infrastructure, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "RQ2a - Infrastructure type (Coverage)",
    subtitle = sprintf("n = %d \u00b7 %d%% report multiple types",
                       n_final_disp, round(100 * n_multi_002 / n_final_disp)),
    x = NULL, y = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Panel B — linear/structural class membership
pal_classes <- c("Linear only" = okabe_ito[2], "Both" = okabe_ito[6],
                 "Structural only" = okabe_ito[1])
p_infra_class <- ggplot(rq2_class, aes(x = n, y = class, fill = class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.5,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = pal_classes, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Records", y = NULL) +
  theme_aquasilva(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_rq2a <- p_infra_cov / p_infra_class + plot_layout(heights = c(5, 3))

save_plot(p_rq2a, "04_rq2a_infrastructure", width_cm = W_COL, height_cm = 9.5)

message("\nRQ2 plot rendered:\n",
        "  figures/04_rq2a_infrastructure.{pdf,png} + tables/04_rq2a_infra_combos.csv")




# 05  RQ3b — HYDROLOGICAL OUTCOME VARIABLES  ==================================
#
# Co-reported hydrological outcome variables (004- family). Highest multi-tag
# share in the corpus (79%, max 3 tags per record) — bars are Coverage and do
# NOT sum to n; this is stated on the plot. PeakFlow's low count relative to the
# project's flood-peak focus is a finding for the running text, not the caption.
# RQ3a (connectivity indices/metrics) needs the extraction Excel — deferred.

outcome_levels <- c("FlowPathInitiation", "RunoffResponse",
                    "SedimentDelivery", "PeakFlow")

rq3b_cov <- rq_corpus |>
  select(Key, tag_004) |>
  unnest_longer(tag_004, values_to = "outcome") |>
  filter(!is.na(outcome), outcome != "empty") |>
  count(outcome, name = "n") |>
  complete(outcome = outcome_levels, fill = list(n = 0L)) |>
  mutate(outcome = fct_reorder(outcome, n))

n_multi_004 <- rq_corpus |>
  mutate(k = map_int(tag_004, ~ sum(!is.na(.x) & .x != "empty"))) |>
  filter(k > 1) |> nrow()

write_combo_table(rq_corpus, "tag_004", "05_rq3b_outcome_combos.csv")

p_rq3b <- ggplot(rq3b_cov, aes(x = n, y = outcome, fill = as.character(outcome))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.35, size = 2.7,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = pal_outcome, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "RQ3b - Outcome variables (Coverage)",
    subtitle = sprintf("n = %d \u00b7 %d%% multi-outcome (max 3 tags)",
                       n_final_disp, round(100 * n_multi_004 / n_final_disp)),
    x = "Records reporting each outcome", y = NULL
  ) +
  theme_aquasilva(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.caption = element_text(colour = "grey45", size = 6.5, hjust = 0,
                                    lineheight = 1.3, margin = margin(t = 6)))

save_plot(p_rq3b, "05_rq3b_outcome", width_cm = W_COL, height_cm = 7)

message("\nRQ3b plot rendered:\n",
        "  figures/05_rq3b_outcome.{pdf,png} + tables/05_rq3b_outcome_combos.csv")




# 06  EVIDENCE GAP MATRIX — INFRASTRUCTURE × OUTCOME  =========================
#
# How many studies report each (infrastructure type × outcome variable) pair.
# This is an evidence-mapping cross-tabulation, NOT an effect-direction synthesis
# (that is RQ5a, deferred to the extraction phase). Empty cells are rendered in a
# distinct fill so research gaps are visually loud rather than reading as "dark =
# low". Caption stays descriptive; the gap interpretation belongs in the text.
# Coverage logic: a multi-tagged record contributes to every pair it spans.

gap_long <- rq_corpus |>
  select(Key, tag_002, tag_004) |>
  unnest_longer(tag_002, values_to = "infra") |>
  unnest_longer(tag_004, values_to = "outcome") |>
  filter(!is.na(infra), !is.na(outcome), outcome != "empty") |>
  distinct(Key, infra, outcome) |>
  count(infra, outcome, name = "n") |>
  complete(infra = infra_levels, outcome = outcome_levels, fill = list(n = 0L))

# Marginal totals → baked into axis labels (robust alternative to outer strips)
infra_tot <- gap_long |> group_by(infra)   |> summarise(t = sum(n), .groups = "drop")
out_tot   <- gap_long |> group_by(outcome) |> summarise(t = sum(n), .groups = "drop")
# Short, wrapped outcome display names so the top-axis labels don't collide
# (the long single tokens overlapped at coord_fixed column pitch).
out_short <- c(FlowPathInitiation = "Flow-path\ninitiation",
               RunoffResponse     = "Runoff\nresponse",
               SedimentDelivery   = "Sediment\ndelivery",
               PeakFlow           = "Peak\nflow")
infra_lab <- setNames(sprintf("%s  (%d)", infra_tot$infra, infra_tot$t), infra_tot$infra)
out_lab   <- setNames(sprintf("%s\n(%d)", out_short[out_tot$outcome], out_tot$t), out_tot$outcome)

# Order: highest-total infra at top, highest-total outcome at left
infra_ord <- infra_tot |> arrange(desc(t)) |> pull(infra)
out_ord   <- out_tot   |> arrange(desc(t)) |> pull(outcome)

gap_long <- gap_long |>
  mutate(
    infra   = factor(infra,   levels = rev(infra_ord)),
    outcome = factor(outcome, levels = out_ord),
    is_zero = n == 0
  )
gap_max <- max(gap_long$n)

p_gap <- ggplot(gap_long, aes(x = outcome, y = infra)) +
  geom_tile(data = ~ filter(.x, !is_zero), aes(fill = n),
            colour = "white", linewidth = 0.8) +
  geom_tile(data = ~ filter(.x, is_zero),
            fill = "#F4DEDA", colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n, colour = !is_zero & n >= gap_max * 0.55),
            size = 3.1, fontface = "bold", show.legend = FALSE) +
  scale_fill_gradient(low = "#D7E8F4", high = okabe_ito[5],
                      name = "Records", limits = c(1, gap_max),
                      breaks = scales::breaks_pretty(n = 4)) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey25"),
                      guide = "none") +
  scale_x_discrete(labels = out_lab, position = "top") +
  scale_y_discrete(labels = infra_lab) +
  coord_fixed() +
  labs(
    title    = "Evidence gap matrix",
    subtitle = sprintf("Records reporting each pair (Coverage)", n_final_disp),
    x = NULL, y = NULL
  ) +
  theme_aquasilva(base_size = 10) +
  theme(
    panel.grid   = element_blank(),
    axis.text.x  = element_text(face = "bold", lineheight = 0.9),
    axis.text.y  = element_text(face = "bold"),
    legend.position = "right",
    plot.caption = element_text(colour = "grey45", size = 7, hjust = 0,
                                lineheight = 1.3, margin = margin(t = 10))
  )

save_plot(p_gap, "06_gap_matrix_infra_outcome", width_cm = W_FULL, height_cm = 11)

# Companion table: wide cross-tab + marginal totals
gap_wide <- gap_long |>
  select(infra, outcome, n) |>
  mutate(infra = as.character(infra), outcome = as.character(outcome)) |>
  pivot_wider(names_from = outcome, values_from = n) |>
  arrange(match(infra, infra_ord)) |>
  rowwise() |>
  mutate(`Row total` = sum(c_across(where(is.numeric)))) |>
  ungroup()
write_csv(gap_wide, file.path(tables_dir, "06_gap_matrix_infra_outcome.csv"))

message("\nGap matrix rendered:\n",
        "  figures/06_gap_matrix_infra_outcome.{pdf,png} + tables/06_gap_matrix_infra_outcome.csv")




# 07  RQ6a — MITIGATION MEASURES (presence)  ==================================
#
# 030- is a deliberate roll-up (Cany-MitigationControls); the original C1/C2/C3
# subtypes were collapsed after an unsatisfactory pilot search. RQ6a is therefore
# answerable only as mitigation present/absent, by design. The cross-tab shows
# which infrastructure types co-occur with reported mitigation. This is PRESENCE,
# not effectiveness — RQ6b (reported effectiveness) needs the extraction Excel.

rq_corpus_mit <- rq_corpus |>
  mutate(mit = map_lgl(tag_030, ~ any(.x == "Cany-MitigationControls")))

n_mit <- sum(rq_corpus_mit$mit)

# Panel A — presence yes/no
rq6_yesno <- rq_corpus_mit |>
  count(mit, name = "n") |>
  mutate(status = if_else(mit, "Mitigation\nreported", "No mitigation\nreported"),
         status = fct_reorder(status, n))

p_mit_yesno <- ggplot(rq6_yesno, aes(x = n, y = status, fill = mit)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%d (%d%%)", n, round(100 * n / n_final_disp))),
            hjust = -0.15, size = 2.6, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c(`TRUE` = okabe_ito[6], `FALSE` = "grey75"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "RQ6a - Mitigation (presence)",
       subtitle = sprintf("n = %d", n_final_disp),
       x = NULL, y = NULL) +
  theme_aquasilva(base_size = 8) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Panel B — infrastructure × mitigation cross-tab (Coverage)
rq6_cross <- rq_corpus_mit |>
  select(Key, tag_002, mit) |>
  unnest_longer(tag_002, values_to = "infra") |>
  filter(!is.na(infra)) |>
  count(infra, mit, name = "n") |>
  complete(infra = infra_levels, mit = c(TRUE, FALSE), fill = list(n = 0L)) |>
  mutate(infra  = factor(infra, levels = rev(infra_levels)),
         status = factor(if_else(mit, "Mitigation", "No mitigation"),
                         levels = c("Mitigation", "No mitigation")))

p_mit_cross <- ggplot(rq6_cross, aes(x = n, y = infra, fill = status)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(
    aes(label = if_else(n > 0, as.character(n), "")),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 2.4,
    fontface = "bold",
    colour = "grey25"
  ) +
  scale_fill_manual(values = c("Mitigation" = okabe_ito[6],
                               "No mitigation" = "grey75"),
                    name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_cartesian(clip = "off") +
  labs(x = "Records (Coverage)", y = NULL) +
  theme_aquasilva(base_size = 8) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(5.5, 18, 5.5, 5.5)
  )

p_rq6a <- p_mit_yesno / p_mit_cross + plot_layout(heights = c(1, 3))

save_plot(p_rq6a, "07_rq6a_mitigation", width_cm = W_COL, height_cm = 11)

message("\nRQ6a plot rendered:\n  figures/07_rq6a_mitigation.{pdf,png}")




# 08  TABLE 1 — CORPUS AT A GLANCE  ===========================================
# Descriptive summary table for the Methods/Results section. Depends on counts
# computed across sections 02–07, so it is assembled last.

glance <- tibble::tribble(
  ~Metric,                          ~Value,
  "Final corpus (n)",                as.character(n_final_disp),
  "Publication years",               sprintf("%d\u2013%d", year_range[1], year_range[2]),
  "Added via snowballing",           as.character(n_snowball),
  "Regions with \u22651 record",     as.character(sum(rq1b_data$n > 0)),
  "Multi-type infrastructure",       sprintf("%d (%d%%)", n_multi_002, round(100 * n_multi_002 / n_final_disp)),
  "Multi-method studies",            sprintf("%d (%d%%)", n_multi_003, round(100 * n_multi_003 / n_final_disp)),
  "Multi-outcome studies",           sprintf("%d (%d%%)", n_multi_004, round(100 * n_multi_004 / n_final_disp)),
  "Studies reporting mitigation",    sprintf("%d (%d%%)", n_mit, round(100 * n_mit / n_final_disp))
)
write_csv(glance, file.path(tables_dir, "01_corpus_at_a_glance.csv"))

message("\nTable 1 written: tables/01_corpus_at_a_glance.csv")


# =============================================================================
# End of v2 pipeline. Rendered (figures/): 02 PRISMA, 02b search strategy,
# 03a temporal, 03b region, 03c journals, 03d study type, 04 infrastructure,
# 05 outcome, 06 gap matrix, 07 mitigation. Tables (tables/): corpus glance,
# search strategy, journals, study-type/infra/outcome combinations, gap matrix.
#
# Deferred until the extraction Excel is available: RQ2b (infra characteristics),
# RQ3a (connectivity indices), RQ4a-subtypes / RQ4b (methods, timescales),
# RQ5a effect direction + RQ5b/c, RQ6b (mitigation effectiveness).
# =============================================================================
