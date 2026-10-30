# =============================================================================
# build_cite_map.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# Regenerates cite_map.csv (zotero_key -> BibTeX cite key) from AquaSilva.bib
# and aquasilva.csv. Anchor = DOI (unique), fallback = normalised title.
# Run whenever the library / .bib changes. extraction_load.R reads cite_map.csv
# and exposes cite_of() for the write-up scripts.
#
# Place AquaSilva.bib and aquasilva.csv in results/ (next to this script).
# =============================================================================

suppressMessages({ library(tidyverse); library(here) })

if (!exists("raw_csv")) { here::i_am("build_cite_map.R"); raw_csv <- here("aquasilva.csv") }
bib_path <- here("AquaSilva.bib")

# --- parse .bib (Better BibTeX exports one field per line) -------------------
lines  <- readLines(bib_path, encoding = "UTF-8", warn = FALSE)
starts <- which(str_detect(lines, "^@\\w+\\{"))
ends   <- c(starts[-1] - 1L, length(lines))

parse_entry <- function(s, e) {
  block <- lines[s:e]
  ck    <- str_match(block[1], "^@\\w+\\{([^,]+),")[1, 2]
  gf <- function(field) {
    hit <- block[str_detect(block, paste0("(?i)^\\s*", field, "\\s*="))]
    if (!length(hit)) return(NA_character_)
    str_match(hit[1], "=\\s*\\{*(.+?)\\}*,?\\s*$")[1, 2]
  }
  tibble(cite_key = str_trim(ck), doi = gf("doi"), title = gf("title"))
}

B <- map2_dfr(starts, ends, parse_entry) |>
  mutate(doi   = str_to_lower(str_remove(replace_na(doi, ""), "https?://doi\\.org/")),
         tnorm = str_replace_all(str_to_lower(replace_na(title, "")), "[^a-z0-9]", ""))

# --- corpus keys from aquasilva.csv ------------------------------------------
aq <- readr::read_csv(raw_csv, col_types = cols(.default = col_character()),
                      na = character()) |>
  rename_with(~ str_remove(.x, "^\uFEFF")) |>
  mutate(doi   = str_to_lower(str_remove(DOI, "https?://doi\\.org/")),
         tnorm = str_replace_all(str_to_lower(Title), "[^a-z0-9]", ""),
         final = str_detect(`Manual Tags`, "006-Include-Final"))
corpus <- aq |> filter(final)

# --- map: DOI first (unique), then normalised title --------------------------
bdoi <- B |> filter(doi   != "") |> distinct(doi,   .keep_all = TRUE) |> select(doi,   ck_doi = cite_key)
btit <- B |> filter(tnorm != "") |> distinct(tnorm, .keep_all = TRUE) |> select(tnorm, ck_tit = cite_key)

mapped <- corpus |>
  left_join(bdoi, by = "doi") |>
  left_join(btit, by = "tnorm") |>
  mutate(cite_key = coalesce(ck_doi, ck_tit),
         first_auth = str_trim(str_extract(Author, "^[^,;]+")),
         citation_short = str_c(first_auth,
                                if_else(str_detect(Author, ";"), " et al. ", " "),
                                `Publication Year`))

# --- report + write ----------------------------------------------------------
n_tot <- nrow(mapped); n_map <- sum(!is.na(mapped$cite_key))
dup   <- mapped |> filter(!is.na(cite_key)) |> count(cite_key) |> filter(n > 1)
unres <- mapped |> filter(is.na(cite_key))

cat(sprintf("cite_map: %d of %d corpus records mapped (DOI: %d, title-fallback: %d).\n",
            n_map, n_tot, sum(!is.na(mapped$ck_doi)),
            sum(is.na(mapped$ck_doi) & !is.na(mapped$ck_tit))))
if (nrow(dup))   warning(sprintf("%d cite key(s) map to >1 record: %s",
                                 nrow(dup), paste(dup$cite_key, collapse = ", ")), call. = FALSE)
if (nrow(unres)) warning(sprintf("%d record(s) UNRESOLVED (no DOI/title match): %s\n  Add the cite key by hand in cite_map.csv.",
                                 nrow(unres), paste(unres$Key, collapse = ", ")), call. = FALSE)

out <- mapped |>
  transmute(zotero_key = Key,
            cite_key   = coalesce(cite_key, Key),   # fallback: zotero key
            citation_short, doi) |>
  arrange(citation_short)
write_csv(out, here("cite_map.csv"))
cat(sprintf("Wrote cite_map.csv (%d rows). WOS-style keys with ':' are valid but unusual; rekey in Zotero if desired.\n",
            nrow(out)))

