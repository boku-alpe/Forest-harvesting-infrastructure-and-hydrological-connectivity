# =============================================================================
# rq5_synthesis.R  -  AquaSilva connectivity-anchored SLR
# -----------------------------------------------------------------------------
# RQ5a - direction of reported effects (vote count), built on the normalised
# extraction sheets. Diverging RdBu bars; two panels with different lenses:
#   A  hydrological outcomes (by outcome, infrastructure pooled)
#   B  effect on connectivity (ConnectivityIndex, by infrastructure type)
#
# Unit of count: EFFECT-LEVEL (each effects.csv row is one vote; matches the
# extraction grain). Study-level counts are a one-line change (see VOTE_UNIT).
# Direction only in v1; magnitude (magnitude_qual) is the natural next layer.
#
# Run after extraction_load.R (it is sourced below if not already loaded), and
# after the main pipeline if you want to reuse its theme - otherwise minimal
# fallbacks are defined here.
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

# RdBu poles (direction) + magnitude encoded as opacity within each bar.
dir_pal    <- c(Decrease = "#2166AC", Increase = "#B2182B")
mag_levels <- c("Major", "Moderate", "Minor", "Negligible", "NotStated")
mag_alpha  <- c(Major = 1.00, Moderate = 0.80, Minor = 0.58,
                Negligible = 0.36, NotStated = 0.18)

# VOTE_UNIT: "effect" = one row per reported effect (needed for the magnitude
# breakdown); "study" = distinct papers (direction only, magnitude less meaningful).
VOTE_UNIT <- "effect"

# -----------------------------------------------------------------------------
# 1  Vote-count tables
# -----------------------------------------------------------------------------
# Directional rows carry magnitude (for the stacked shading); neutral rows
# (NoEffect / Mixed) are counted separately and annotated off-axis.
dir_counts <- function(df, group_col) {
  base <- df |>
    transmute(group = .data[[group_col]], zotero_key, direction,
              magnitude = factor(if_else(magnitude_qual %in% mag_levels,
                                         magnitude_qual, "NotStated"),
                                 levels = mag_levels)) |>
    filter(direction %in% c("Increase", "Decrease"))
  if (VOTE_UNIT == "study")
    base <- base |> distinct(group, direction, magnitude, zotero_key)
  base |>
    count(group, direction, magnitude, name = "n") |>
    complete(group, direction = c("Increase", "Decrease"),
             magnitude, fill = list(n = 0L))
}

hyd <- ex_effects |> filter(outcome_family == "Hydrological", in_main_votecount)
con <- ex_effects |> filter(outcome_family == "Connectivity")

# -----------------------------------------------------------------------------
# 2  Panel builder (diverging bars, magnitude shading + neutral annotation)
# -----------------------------------------------------------------------------
make_panel <- function(df, group_col, title, subtitle, xlab = NULL) {
  dc  <- dir_counts(df, group_col)
  ord <- dc |> group_by(group) |> summarise(t = sum(n), .groups = "drop") |>
    arrange(t) |> pull(group)
  dc  <- dc |> mutate(group = factor(group, levels = ord),
                      signed = if_else(direction == "Decrease", -n, n))
  
  # per-direction totals -> end-of-bar count labels
  tot <- dc |> group_by(group, direction) |> summarise(tt = sum(n), .groups = "drop") |>
    filter(tt > 0) |>
    mutate(x = if_else(direction == "Decrease", -tt, tt),
           h = if_else(direction == "Decrease", 1.25, -0.25))
  
  # neutral (NoEffect / Mixed) counts -> right-margin annotation
  neu <- df |> transmute(group = .data[[group_col]], direction) |>
    filter(direction %in% c("NoEffect", "Mixed", "NotDerivable"), group %in% ord) |>
    count(group, direction, name = "n") |>
    complete(group = ord, direction = c("NoEffect", "Mixed", "NotDerivable"),
             fill = list(n = 0L)) |>
    pivot_wider(names_from = direction, values_from = n, values_fill = 0) |>
    mutate(group = factor(group, levels = ord),
           lab = pmap_chr(list(NoEffect, Mixed, NotDerivable), function(a, b, c) {
             parts <- c(if (a > 0) sprintf("no change %d", a),
                        if (b > 0) sprintf("mixed %d", b),
                        if (c > 0) sprintf("not attributable %d", c))
             paste(parts, collapse = " \u00b7 ")
           }))
  
  xmax <- max(abs(tot$x), 1)
  xann <- xmax * 1.22
  
  ggplot(dc, aes(signed, group, fill = direction, alpha = magnitude)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.15,
             position = position_stack(reverse = TRUE)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_text(data = tot, aes(x = x, y = group, label = tt), hjust = tot$h,
              inherit.aes = FALSE, size = 2.5, colour = "grey20") +
    geom_text(data = neu, aes(x = xann, y = group, label = lab),
              inherit.aes = FALSE, hjust = 0, size = 2.2, colour = "grey55") +
    scale_fill_manual(values = dir_pal, name = "Direction",
                      breaks = c("Decrease", "Increase"),
                      limits = c("Decrease", "Increase")) +
    scale_alpha_manual(values = mag_alpha, name = "Magnitude",
                       breaks = mag_levels, limits = mag_levels) +
    scale_x_continuous(labels = function(x) abs(x),
                       expand = expansion(mult = c(0.10, 0.36))) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_aquasilva(base_size = 8) +
    theme(panel.grid.major.y = element_blank())
}

pA <- make_panel(
  hyd, "outcome_variable", "A - Effect on hydrological outcomes",
  "by outcome variable \u00b7 infrastructure pooled \u00b7 effect-level")
pB <- make_panel(
  con, "infrastructure_type", "B - Effect on connectivity (ConnectivityIndex)",
  "by infrastructure type \u00b7 effect-level",
  xlab = "\u2190 Decrease        reported effects        Increase \u2192")

p_rq5a <- (pA / pB) +
  plot_layout(guides = "collect", heights = c(5, 6)) +
  plot_annotation(
    title = "RQ5a - Direction and magnitude of reported effects (vote count)",
    theme = theme_aquasilva(base_size = 8)
  ) &
  guides(fill  = guide_legend(order = 1, override.aes = list(alpha = 1)),
         alpha = guide_legend(order = 2, override.aes = list(fill = "grey25"))) &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot(p_rq5a, "09_rq5a_effect_direction", width_cm = W_FULL, height_cm = 13.5)

# -----------------------------------------------------------------------------
# 3  Companion tables (direction totals + magnitude breakdown)
# -----------------------------------------------------------------------------
votes_dir <- bind_rows(
  hyd |> transmute(panel = "Hydrological (by outcome)", group = outcome_variable, direction),
  con |> transmute(panel = "Connectivity (by infrastructure)", group = infrastructure_type, direction)
) |>
  count(panel, group, direction, name = "n") |>
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) |>
  mutate(Total = rowSums(across(any_of(c("Increase", "Decrease", "NoEffect", "Mixed", "NotDerivable"))))) |>
  arrange(panel, desc(Total))
write_csv(votes_dir, file.path(tables_dir, "09_rq5a_votecount.csv"))

votes_mag <- bind_rows(
  dir_counts(hyd, "outcome_variable")    |> mutate(panel = "Hydrological (by outcome)"),
  dir_counts(con, "infrastructure_type") |> mutate(panel = "Connectivity (by infrastructure)")
) |>
  select(panel, group, direction, magnitude, n) |>
  arrange(panel, group, direction, magnitude)
write_csv(votes_mag, file.path(tables_dir, "09_rq5a_votecount_magnitude.csv"))

message(sprintf(
  "RQ5a rendered (unit = %s): figures/09_rq5a_effect_direction.{pdf,png}\n  tables: 09_rq5a_votecount.csv + 09_rq5a_votecount_magnitude.csv\n  hydrological effects = %d across %d outcomes; connectivity effects = %d across %d infrastructure types.",
  VOTE_UNIT, nrow(hyd), n_distinct(hyd$outcome_variable),
  nrow(con), n_distinct(con$infrastructure_type)))