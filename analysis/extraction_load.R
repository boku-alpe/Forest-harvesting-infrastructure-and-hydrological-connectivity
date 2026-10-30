# =============================================================================
# extraction_load.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# Load · normalise · validate the six extraction sheets and expose clean tibbles
# for the deferred RQ sections (RQ2b, RQ3a, RQ4b, RQ5a/b, RQ6b).
#
# Place the six CSVs (studies, infrastructure, connectivity, effects,
# mitigation, reviews) in results/ next to aquasilva.csv, then either
#   source("extraction_load.R")
# after the main pipeline's setup, or run it standalone.
#
# Design principle: closed-vocabulary fields are checked against an explicit
# allowed set and the script STOPS on any unknown value, so a re-extraction
# cannot silently introduce a new label that quietly corrupts a group-by.
# Normalisation decisions (locked 2026-06-25):
#   - effects.infrastructure_type: strip leading "002-" prefix
#   - effects.outcome_variable: FlowPathExtension -> FlowPathInitiation
#   - ConnectivityIndex kept as its own outcome family (effect ON connectivity)
#   - FlowTiming kept as a minor outcome; Network-aggregate kept as infra class
#   - outcome "other" retained in data but excluded from the main vote-count
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(here)
})

# Anchor to results/ only when run standalone (the main script already does this).
if (!exists("raw_csv")) {
  here::i_am("extraction_load.R")
  raw_csv    <- here("aquasilva.csv")
  tables_dir <- here("tables")
}
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

read_sheet <- function(name) {
  readr::read_csv(here(paste0(name, ".csv")),
                  col_types = cols(.default = col_character()),
                  na = character()) |>
    rename_with(~ str_remove(.x, "^\uFEFF")) |>            # strip UTF-8 BOM in header
    mutate(across(everything(), ~ str_trim(replace_na(.x, ""))))
}

# -----------------------------------------------------------------------------
# 1  Closed vocabularies  (single source of truth for validation)
# -----------------------------------------------------------------------------
INFRA_CANON <- c("Roads", "Structures", "ExtractionTrails",
                 "RoadDrainageBMP", "RetentionDetention")

VOCAB <- list(
  effects = list(
    direction                   = c("Increase", "Decrease", "Mixed", "NoEffect",
                                    "NotDerivable"),   # admitted by protocol S2
    magnitude_qual              = c("Major", "Moderate", "Minor", "Negligible", "NotStated"),
    spatial_scale               = c("Hillslope", "Catchment", "Element", "Reach"),
    connectivity_evidence_basis = c("FlowPathDelineation", "HydrologicalProxy",
                                    "Metric", "Qualitative"),
    synthesis_role              = c("primary", "secondary"),
    # post-normalisation (prefix stripped); Network-aggregate is a valid class
    infrastructure_type         = c(INFRA_CANON, "Network-aggregate"),
    # post-normalisation (FlowPathExtension folded into FlowPathInitiation)
    outcome_variable            = c("FlowPathInitiation", "RunoffResponse",
                                    "SedimentDelivery", "PeakFlow",
                                    "ConnectivityIndex", "FlowTiming", "other")
  ),
  connectivity = list(
    metric_family            = c("Custom", "DrainageDensityExtension", "DistanceBased",
                                 "TravelTime", "IndexOfConnectivity", "RoadStreamLinkage",
                                 "FlowPathDelineation", "GraphNetwork"),
    connectivity_type        = c("Functional", "Structural", "Both"),
    operationalization_basis = c("ModelDerived", "QuantitativeIndex",
                                 "FieldMeasurementLinkage", "FlowPathDelineation"),
    direction_of_use         = c("DiagnosticOnly", "Both", "OptimizationTarget")
  ),
  infrastructure = list(
    infrastructure_type = INFRA_CANON
  ),
  mitigation = list(
    measure_type         = c("DrainageBMP", "InfiltrationDisconnection", "Revegetation",
                             "RetentionDetention", "RoadDecommissioning", "SedimentTrap",
                             "SurfaceStabilization", "RoadRelocationClosure", "other"),
    effectiveness        = c("Effective", "PartiallyEffective", "Ineffective",
                             "Mixed", "NotAssessed"),
    design_spec_reported = c("Yes", "No")
  )
)
MODERATOR_VOCAB <- c("TopographySlope", "CatchmentCharacteristics", "SoilSubstrate",
                     "LandManagement", "InfrastructureDesign", "StormCharacteristics",
                     "Seasonality", "AntecedentConditions", "none")
CONF_VOCAB <- c("High", "Medium", "Low")

# -----------------------------------------------------------------------------
# 2  Validators  (hard stop on unknown closed-vocab values)
# -----------------------------------------------------------------------------
check_vocab <- function(df, col, allowed, sheet, allow_blank = TRUE) {
  if (!col %in% names(df)) stop(sprintf("[%s] expected column '%s' is missing.", sheet, col),
                                call. = FALSE)
  vals <- df[[col]]
  bad  <- !(vals %in% allowed) & !(allow_blank & vals == "")
  if (any(bad)) {
    bad_vals <- sort(unique(vals[bad]))
    stop(sprintf(
      "[%s] column '%s': %d value(s) outside the allowed vocabulary:\n    %s\n  Allowed: %s",
      sheet, col, length(bad_vals),
      paste(bad_vals, collapse = " | "), paste(allowed, collapse = ", ")),
      call. = FALSE)
  }
  invisible(TRUE)
}

check_vocab_multi <- function(df, col, allowed, sheet, sep = ";") {
  atoms <- df[[col]] |> str_split(sep) |> unlist() |> str_trim()
  atoms <- atoms[atoms != ""]
  bad   <- setdiff(unique(atoms), allowed)
  if (length(bad)) {
    stop(sprintf("[%s] multi-value column '%s' has unknown atom(s):\n    %s\n  Allowed: %s",
                 sheet, col, paste(sort(bad), collapse = " | "),
                 paste(allowed, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 3  Load sheets
# -----------------------------------------------------------------------------
ex_studies        <- read_sheet("studies")
ex_infrastructure <- read_sheet("infrastructure")
ex_connectivity   <- read_sheet("connectivity")
ex_effects        <- read_sheet("effects")
ex_mitigation     <- read_sheet("mitigation")
ex_reviews        <- read_sheet("reviews")

# -----------------------------------------------------------------------------
# 3b  Documented manual corrections (consistency review, October 2026)
# -----------------------------------------------------------------------------
# The raw extraction sheets stay untouched. Corrections found during the
# manuscript revision are listed, with rationale, in effects_corrections.csv and
# applied here, so every change remains traceable and reversible.
n_corrections <- 0L
if (file.exists(here("effects_corrections.csv"))) {
  corr <- readr::read_csv(here("effects_corrections.csv"),
                          col_types = cols(.default = col_character()))
  for (i in seq_len(nrow(corr))) {
    r   <- corr[i, ]
    idx <- which(ex_effects$effect_id == r$effect_id)
    if (length(idx) != 1L)
      stop(sprintf("correction: effect_id %s not found exactly once", r$effect_id), call. = FALSE)
    if (!identical(ex_effects[[r$field]][idx], r$old_value))
      stop(sprintf("correction: %s$%s is '%s', expected '%s'", r$effect_id, r$field,
                   ex_effects[[r$field]][idx], r$old_value), call. = FALSE)
    ex_effects[[r$field]][idx] <- r$new_value
  }
  n_corrections <- nrow(corr)
  message(sprintf("Applied %d documented corrections from effects_corrections.csv.", n_corrections))
}

# -----------------------------------------------------------------------------
# 4  Normalise effects (before validation)
# -----------------------------------------------------------------------------
n_prefix_stripped <- sum(str_detect(ex_effects$infrastructure_type, "^002-"))
n_fpext_renamed   <- sum(ex_effects$outcome_variable == "FlowPathExtension")

ex_effects <- ex_effects |>
  mutate(
    infrastructure_type = str_remove(infrastructure_type, "^002-"),
    outcome_variable    = if_else(outcome_variable == "FlowPathExtension",
                                  "FlowPathInitiation", outcome_variable),
    outcome_family = case_when(
      outcome_variable == "ConnectivityIndex" ~ "Connectivity",
      outcome_variable %in% c("FlowPathInitiation", "RunoffResponse",
                              "SedimentDelivery", "PeakFlow", "FlowTiming") ~ "Hydrological",
      TRUE ~ "Other"
    ),
    in_main_votecount = outcome_variable != "other",
    direction_sign = case_when(
      direction == "Increase" ~ 1L,
      direction == "Decrease" ~ -1L,
      direction == "NoEffect" ~ 0L,
      TRUE ~ NA_integer_          # Mixed (and blanks) carry no single sign
    )
  )

# -----------------------------------------------------------------------------
# 5  Validate closed vocabularies (stops on first offending sheet/column)
# -----------------------------------------------------------------------------
for (sheet in names(VOCAB)) {
  df <- get(paste0("ex_", sheet))
  for (col in names(VOCAB[[sheet]])) {
    check_vocab(df, col, VOCAB[[sheet]][[col]], sheet)
  }
}
check_vocab_multi(ex_effects, "moderator_class", MODERATOR_VOCAB, "effects")
for (sheet in c("studies", "infrastructure", "connectivity", "effects", "mitigation")) {
  df <- get(paste0("ex_", sheet))
  if ("confidence" %in% names(df)) check_vocab(df, "confidence", CONF_VOCAB, sheet)
}
message("Vocabulary validation passed for all closed-vocabulary fields.")

# -----------------------------------------------------------------------------
# 6  Join / corpus integrity
# -----------------------------------------------------------------------------
aq <- readr::read_csv(raw_csv, col_types = cols(.default = col_character()),
                      na = character()) |>
  rename_with(~ str_remove(.x, "^\uFEFF"))
aq_tags <- str_split(aq[["Manual Tags"]], ";")
aq_tags <- map(aq_tags, ~ str_trim(.x))
final_keys <- aq$Key[map_lgl(aq_tags, ~ "006-Include-Final" %in% .x)]
mit030_keys <- aq$Key[map_lgl(aq_tags, ~ any(str_starts(.x, "030-")))]

study_keys  <- ex_studies$zotero_key
review_keys <- ex_reviews$zotero_key
record_keys <- union(study_keys, review_keys)   # every extracted record

# Hard checks: the final corpus is covered by primary studies + reviews together.
miss_corpus  <- setdiff(final_keys, record_keys)    # final records with no sheet row
extra_corpus <- setdiff(record_keys, final_keys)    # sheet records not in the corpus
overlap_sr   <- intersect(study_keys, review_keys)  # a record filed as both
if (length(miss_corpus) || length(extra_corpus)) {
  warning(sprintf("studies+reviews vs final corpus mismatch: %d final not in either sheet, %d sheet records not in corpus.",
                  length(miss_corpus), length(extra_corpus)), call. = FALSE)
}
if (length(overlap_sr)) {
  warning(sprintf("%d record(s) appear in BOTH studies and reviews: %s",
                  length(overlap_sr), paste(overlap_sr, collapse = ", ")), call. = FALSE)
}

# Effect-level sheets must key to a primary study; reviews key to the corpus.
for (sheet in c("infrastructure", "connectivity", "effects", "mitigation")) {
  k <- get(paste0("ex_", sheet))$zotero_key
  orph <- setdiff(unique(k), study_keys)
  if (length(orph)) {
    stop(sprintf("[%s] %d zotero_key(s) not present in studies: %s",
                 sheet, length(orph), paste(orph, collapse = ", ")), call. = FALSE)
  }
}
orph_rev <- setdiff(unique(review_keys), final_keys)
if (length(orph_rev)) {
  stop(sprintf("[reviews] %d zotero_key(s) not in the final corpus: %s",
               length(orph_rev), paste(orph_rev, collapse = ", ")), call. = FALSE)
}

# Soft checks (reported, not fatal)
no_infra   <- setdiff(study_keys, ex_infrastructure$zotero_key)
no_effects <- setdiff(study_keys, ex_effects$zotero_key)
mit_tag_no_row <- setdiff(mit030_keys, ex_mitigation$zotero_key)
reviews_empty <- ex_reviews |>
  summarise(across(any_of(c("synthesis_statement", "identified_gaps",
                            "consensus_claim", "classification_scheme")),
                   ~ all(.x == ""))) |>
  unlist() |> all()

# -----------------------------------------------------------------------------
# 7  Summary + validation log
# -----------------------------------------------------------------------------
cat("\n================ extraction_load summary ================\n")
cat(sprintf("Final corpus (006-Include-Final): %d\n", length(final_keys)))
cat(sprintf("studies %d | infrastructure %d (%d studies) | connectivity %d (%d) | effects %d (%d) | mitigation %d (%d) | reviews %d\n",
            nrow(ex_studies),
            nrow(ex_infrastructure), n_distinct(ex_infrastructure$zotero_key),
            nrow(ex_connectivity),   n_distinct(ex_connectivity$zotero_key),
            nrow(ex_effects),        n_distinct(ex_effects$zotero_key),
            nrow(ex_mitigation),     n_distinct(ex_mitigation$zotero_key),
            nrow(ex_reviews)))
cat(sprintf("corpus coverage: %d studies + %d reviews = %d records (final corpus %d)\n",
            nrow(ex_studies), nrow(ex_reviews), length(record_keys), length(final_keys)))
cat(sprintf("Normalised: stripped '002-' prefix on %d effects rows; renamed FlowPathExtension on %d rows.\n",
            n_prefix_stripped, n_fpext_renamed))
cat(sprintf("effects outcome families: %s\n",
            paste(names(table(ex_effects$outcome_family)),
                  table(ex_effects$outcome_family), sep = "=", collapse = ", ")))

cat("\n--- data gaps (review before synthesis) ---\n")
cat(sprintf("studies without an infrastructure row: %s\n",
            if (length(no_infra)) paste(no_infra, collapse = ", ") else "none"))
cat(sprintf("primary studies without an effects row: %s\n",
            if (length(no_effects)) paste(no_effects, collapse = ", ") else "none"))
cat(sprintf("030-tagged studies without a mitigation row: %s\n",
            if (length(mit_tag_no_row)) paste(mit_tag_no_row, collapse = ", ") else "none"))
cat(sprintf("reviews.csv qualitative fields empty: %s\n", reviews_empty))

# flagged-row inventory (the human-review queue)
flag_inv <- imap_dfr(
  list(studies = ex_studies, infrastructure = ex_infrastructure,
       connectivity = ex_connectivity, effects = ex_effects, mitigation = ex_mitigation),
  function(df, nm) {
    if (!"extraction_flag" %in% names(df)) return(tibble())
    df |> filter(extraction_flag != "") |>
      count(extraction_flag, name = "n") |>
      mutate(sheet = nm, .before = 1)
  }
)
cat(sprintf("\nFlagged rows (extraction_flag set): %d across all sheets.\n", sum(flag_inv$n)))

# Persist a machine-readable validation log
log_tbl <- bind_rows(
  tibble(check = "final_corpus_n",            value = as.character(length(final_keys))),
  tibble(check = "studies_n",                 value = as.character(nrow(ex_studies))),
  tibble(check = "corpus_coverage_mismatch",  value = as.character(length(miss_corpus) + length(extra_corpus))),
  tibble(check = "effects_prefix_stripped",   value = as.character(n_prefix_stripped)),
  tibble(check = "effects_flowpath_renamed",  value = as.character(n_fpext_renamed)),
  tibble(check = "studies_without_infra",     value = paste(no_infra, collapse = "; ")),
  tibble(check = "studies_without_effects",   value = paste(no_effects, collapse = "; ")),
  tibble(check = "mit030_without_row",        value = paste(mit_tag_no_row, collapse = "; ")),
  tibble(check = "reviews_fields_empty",      value = as.character(reviews_empty)),
  tibble(check = "flagged_rows_total",        value = as.character(sum(flag_inv$n))),
  tibble(check = "manual_corrections_applied", value = as.character(n_corrections))
)
write_csv(log_tbl,  file.path(tables_dir, "extraction_validation_log.csv"))
write_csv(flag_inv, file.path(tables_dir, "extraction_flag_inventory.csv"))
cat("\nWrote tables/extraction_validation_log.csv and tables/extraction_flag_inventory.csv\n")
cat("=========================================================\n")

# Exposed for the RQ sections:
#   ex_studies, ex_infrastructure, ex_connectivity, ex_effects, ex_mitigation, ex_reviews
#   ex_effects extras: infrastructure_type (normalised), outcome_variable (normalised),
#                      outcome_family {Hydrological|Connectivity|Other},
#                      in_main_votecount (lgl), direction_sign {-1,0,1,NA}
#   final_keys, mit030_keys

# -----------------------------------------------------------------------------
# 8  Citation-key map (optional; regenerate with build_cite_map.R -> cite_map.csv)
# -----------------------------------------------------------------------------
cite_map_path <- here("cite_map.csv")
cite_map <- if (file.exists(cite_map_path)) {
  suppressMessages(readr::read_csv(cite_map_path, show_col_types = FALSE)) |>
    select(zotero_key, cite_key)
} else tibble(zotero_key = character(), cite_key = character())

# cite_of(): zotero_key -> BibTeX cite key; falls back to zotero_key if unmapped.
cite_of <- function(keys) {
  m <- cite_map$cite_key[match(keys, cite_map$zotero_key)]
  dplyr::coalesce(m, as.character(keys))
}
if (nrow(cite_map))
  message(sprintf("cite_map loaded: %d zotero_key -> cite_key mappings.", nrow(cite_map)))
