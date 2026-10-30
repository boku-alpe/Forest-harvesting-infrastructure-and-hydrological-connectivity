# =============================================================================
# rq_extraction_figures.R  —  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# The remaining extraction-based descriptive RQs, built on the normalised sheets
# from extraction_load.R:
#   10  RQ3a  connectivity operationalisation (metric families × type, basis)
#   11  RQ4b  measurement methods + temporal scale
#   12  RQ2b  infrastructure: stream connectivity (topology × type) + condition
#   13  RQ5b  effect moderators (coverage)
#   14  RQ6b  reported mitigation effectiveness × measure type
#
# Free-text fields (design_descriptor, land_use_context, age_or_since) carry the
# qualitative detail and are intentionally NOT charted — they belong in tables /
# running text. Counts are effect-/metric-/study-/measure-level as noted.
# =============================================================================

suppressMessages({ library(tidyverse); library(here); library(patchwork) })

if (!exists("ex_effects")) source(here("extraction_load.R"))

# --- shared visual helpers (reuse the main pipeline's if already in session) --
if (!exists("figures_dir")) figures_dir <- here("figures")
if (!exists("tables_dir"))  tables_dir  <- here("tables")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir,  showWarnings = FALSE, recursive = TRUE)
if (!exists("W_COL"))  W_COL  <- 8.4
if (!exists("W_FULL")) W_FULL <- 17.4
if (!exists("okabe_ito")) okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442",
                                         "#0072B2","#D55E00","#CC79A7","#999999")
if (!exists("theme_aquasilva")) {
  theme_aquasilva <- function(base_size = 9) {
    theme_minimal(base_size = base_size) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(colour = "grey35"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = "grey92"),
            axis.title    = element_text(colour = "grey30"),
            legend.position = "bottom")
  }
}
if (!exists("save_plot")) {
  save_plot <- function(plot, name, width_cm = W_FULL, height_cm = 10, png = TRUE) {
    ggsave(file.path(figures_dir, paste0(name, ".pdf")), plot,
           width = width_cm, height = height_cm, units = "cm", device = cairo_pdf)
    if (png) ggsave(file.path(figures_dir, paste0(name, ".png")), plot,
                    width = width_cm, height = height_cm, units = "cm", dpi = 300)
    invisible(plot)
  }
}

# Insert spaces at camelCase boundaries for readable axis labels.
pretty_lab <- function(x) stringr::str_wrap(gsub("([a-z])([A-Z])", "\\1 \\2", x), width = 20)

# Categorical palettes
pal_conn_type <- c(Structural = okabe_ito[1], Functional = okabe_ito[5], Both = "grey60")
pal_topology  <- c(StreamConnected = okabe_ito[6], Mixed = okabe_ito[5],
                   Disconnected = okabe_ito[3], NotStated = "grey75")
pal_effect    <- c(Effective = "#1A9850", PartiallyEffective = "#A6D96A",
                   Mixed = "#FEE08B", Ineffective = "#D73027", NotAssessed = "grey75")

# -----------------------------------------------------------------------------
# Builders
# -----------------------------------------------------------------------------
# single-category horizontal coverage bar (input: tibble with cols cat, n)
hbar <- function(dat, title = NULL, subtitle = NULL,
                 fill = okabe_ito[2], xlab = "Records") {
  dat <- dat |> mutate(cat = fct_reorder(as.character(cat), n))
  ggplot(dat, aes(n, cat)) +
    geom_col(fill = fill, width = 0.7) +
    geom_text(aes(label = n), hjust = -0.3, size = 2.5,
              fontface = "bold", colour = "grey25") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
    scale_y_discrete(labels = pretty_lab) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_aquasilva(8) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

# stacked horizontal bar (input: tibble; pass bare cat/fill columns + a `n`)
hbar_stack <- function(dat, catcol, fillcol, pal, title = NULL, subtitle = NULL,
                       xlab = "Records", fill_levels = NULL, label = TRUE) {
  d <- dat |> rename(cat = {{ catcol }}, fll = {{ fillcol }})
  ord <- d |> group_by(cat) |> summarise(t = sum(n), .groups = "drop") |>
    arrange(t) |> pull(cat)
  d <- d |> mutate(cat = factor(cat, levels = ord))
  if (!is.null(fill_levels)) d <- d |> mutate(fll = factor(fll, levels = fill_levels))
  # per-fill label colour: white on dark segments, dark on light ones
  contrast <- vapply(pal, function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    lum <- 0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
    if (lum > 0.55) "grey15" else "white"
  }, character(1))
  p <- ggplot(d, aes(n, cat, fill = fll)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = pal, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
    scale_y_discrete(labels = pretty_lab) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_aquasilva(8) +
    theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
  if (label) p <- p +
    geom_text(aes(label = ifelse(n > 0, n, ""), colour = fll),
              position = position_stack(vjust = 0.5),
              size = 2.1, fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = contrast, guide = "none")
  p
}

# multi-value coverage count (";"-separated atoms; input: df + bare column)
atom_count <- function(df, col) {
  df |> transmute(v = {{ col }}) |>
    tidyr::separate_rows(v, sep = ";") |>
    mutate(v = str_trim(v)) |>
    filter(v != "", v != "none") |>
    count(cat = v, name = "n")
}
val_count <- function(df, col) {
  df |> transmute(cat = {{ col }}) |> filter(cat != "") |>
    count(cat, name = "n")
}

# =============================================================================
# 10  RQ3a — Connectivity operationalisation
# =============================================================================
conn_clean <- ex_connectivity |> filter(metric_family != "", connectivity_type != "")
mfam <- conn_clean |> count(metric_family, connectivity_type, name = "n")

pA3 <- hbar_stack(
  mfam, metric_family, connectivity_type, pal_conn_type,
  "RQ3a - Metric families",
  sprintf("n = %d metric instances", nrow(ex_connectivity)),
  xlab = "Metric instances",
  fill_levels = c("Structural", "Functional", "Both"))

pB3 <- hbar(val_count(ex_connectivity, operationalization_basis),
            "Operationalisation basis", NULL,
            fill = okabe_ito[5], xlab = "Metric instances")

p_rq3a <- (pA3 / pB3) + plot_layout(heights = c(8, 4))
save_plot(p_rq3a, "10_rq3a_connectivity_metrics", width_cm = W_FULL, height_cm = 12)
write_csv(mfam, file.path(tables_dir, "10_rq3a_metric_family_by_type.csv"))

# =============================================================================
# 11  RQ4b — Measurement methods + temporal scale
# =============================================================================
mm <- atom_count(ex_studies, measurement_method)
pA4 <- hbar(mm, "RQ4 - Measurement methods",
            sprintf("n = %d studies \u00b7 multi-method", nrow(ex_studies)),
            fill = okabe_ito[3], xlab = "Studies")
pB4 <- hbar(val_count(ex_studies, temporal_scale),
            "Temporal scale", NULL, fill = okabe_ito[5], xlab = "Studies")

p_rq4b <- (pA4 / pB4) + plot_layout(heights = c(9, 6))
save_plot(p_rq4b, "11_rq4b_methods", width_cm = W_COL, height_cm = 11)
write_csv(mm, file.path(tables_dir, "11_rq4b_measurement_methods.csv"))

# =============================================================================
# 12  RQ2b — Infrastructure: stream connectivity + condition
# =============================================================================
infra_clean <- ex_infrastructure |> filter(infrastructure_type != "", network_topology != "")
topo <- infra_clean |> count(infrastructure_type, network_topology, name = "n")

pA2 <- hbar_stack(
  topo, infrastructure_type, network_topology, pal_topology,
  "RQ2b - Network topology",
  sprintf("n = %d records", nrow(ex_infrastructure)),
  xlab = "Records",
  fill_levels = c("StreamConnected", "Mixed", "Disconnected", "NotStated"))

pB2 <- hbar(val_count(ex_infrastructure, condition),
            "Reported condition", NULL, fill = okabe_ito[1], xlab = "Records")

p_rq2b <- (pA2 / pB2) + plot_layout(heights = c(5, 6))
save_plot(p_rq2b, "12_rq2b_infrastructure", width_cm = W_COL, height_cm = 11)
write_csv(topo, file.path(tables_dir, "12_rq2b_topology_by_type.csv"))

# =============================================================================
# 13  RQ5b — Effect moderators (coverage)
# =============================================================================
mod <- atom_count(ex_effects, moderator_class)
p_rq5b <- hbar(mod, "RQ5b - Effect moderators",
               sprintf("n = %d effects \u00b7 multi-moderator", nrow(ex_effects)),
               fill = okabe_ito[6], xlab = "Effects")
save_plot(p_rq5b, "13_rq5b_moderators", width_cm = W_COL, height_cm = 9.5)
write_csv(mod, file.path(tables_dir, "13_rq5b_moderators.csv"))

# =============================================================================
# 14  RQ6b — Reported mitigation effectiveness
# =============================================================================
eff <- ex_mitigation |> filter(measure_type != "", effectiveness != "") |>
  count(measure_type, effectiveness, name = "n")

p_rq6b <- hbar_stack(
  eff, measure_type, effectiveness, pal_effect,
  "RQ 6b - Mitigation effectiveness",
  sprintf("n = %d measures, %d studies \u00b7 presence-limited",
          nrow(ex_mitigation), n_distinct(ex_mitigation$zotero_key)),
  xlab = "Measures",
  fill_levels = c("Effective", "PartiallyEffective", "Mixed", "Ineffective", "NotAssessed"))
save_plot(p_rq6b, "14_rq6b_mitigation_effectiveness", width_cm = W_FULL, height_cm = 10)
write_csv(eff, file.path(tables_dir, "14_rq6b_effectiveness_by_measure.csv"))

message("Extraction RQ figures rendered: 10_rq3a, 11_rq4b, 12_rq2b, 13_rq5b, 14_rq6b ",
        "(+ companion CSVs in tables/).")