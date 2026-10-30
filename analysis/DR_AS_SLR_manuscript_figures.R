# =============================================================================
# DR_AS_SLR_manuscript_figures.R  -  AquaSilva SLR, manuscript v2 figure set
# -----------------------------------------------------------------------------
# Composes the panels produced by the existing pipeline into the eight
# manuscript figures of v2 (October 2026 revision). The pipeline scripts are
# sourced unchanged, so every count still comes from the same code. This file
# only restyles panels (titles, readable category labels, panel order) and
# arranges them with patchwork.
#
#   fig_corpus      Corpus overview, 4 panels      (was Fig. 3-6;   C027)
#   fig_infra       Infrastructure (RQ1), 4 panels (was Fig. 7-8;   C032, C033)
#   fig_measure     Measurement (RQ2), 4 panels    (was Fig. 9, 12; C034, C040)
#   fig_gap         Evidence gap matrix             (was Fig. 11)
#   fig_effects     Effect direction (RQ3)          (was Fig. 13;   C044 swap)
#   fig_moderators  Effect moderators (RQ3)         (was Fig. 14)
#   fig_mitigation  Mitigation (RQ4), 3 panels      (was Fig. 15-16; C051)
#
# Run order: from the results/ folder, source this file on a fresh session.
# Output: figures/fig_*.pdf (+ .png)
# =============================================================================

suppressMessages({ library(tidyverse); library(patchwork); library(here) })

# 1  Run the existing pipeline ---------------------------------------------------
source("aquasilva_slr_visualization.R")   # corpus figures, objects p_rq1a ... p_rq6a
source("extraction_load.R")               # normalised extraction sheets
source("rq_extraction_figures.R")         # pA3, pB3, pA4, pB4, pA2, pB2, p_rq5b, p_rq6b
source("rq5_synthesis.R")                 # pA (processes), pB (connectivity)

# 2  Readable labels -------------------------------------------------------------
# One mapping for every category label in the figures (C031: no CamelCase).
label_map <- c(
  # study type
  FieldMonitoring = "Field monitoring", Modeling = "Modelling",
  PairedCatchment = "Paired catchment", Experimental = "Experimental",
  Review = "Review",
  # regions
  NorthAmerica = "North America", SouthAmerica = "South America",
  # infrastructure
  Roads = "Roads", ExtractionTrails = "Extraction trails",
  Structures = "Crossing structures", RoadDrainageBMP = "Road-drainage measures",
  RetentionDetention = "Retention / detention",
  # processes
  FlowPathInitiation = "Flow-path initiation", FlowPathExtension = "Flow-path initiation",
  RunoffResponse = "Runoff response", SedimentDelivery = "Sediment delivery",
  PeakFlow = "Peak flow", FlowTiming = "Flow timing",
  # topology / condition
  StreamConnected = "Stream-connected", Disconnected = "Disconnected",
  NotStated = "Not stated", Mixed = "Mixed",
  # connectivity metrics
  Custom = "Study-specific", RoadStreamLinkage = "Road-stream linkage",
  DistanceBased = "Distance-based", DrainageDensityExtension = "Drainage density / extension",
  IndexOfConnectivity = "Index of connectivity", TravelTime = "Travel time",
  GraphNetwork = "Graph network", FlowPathDelineation = "Flow-path delineation",
  FieldMeasurementLinkage = "Field-measured link", ModelDerived = "Model-derived",
  QuantitativeIndex = "Quantitative index",
  # methods / temporal scale
  FieldSurvey = "Field survey", `GIS-Terrain` = "GIS / terrain analysis",
  StreamGauging = "Stream gauging", SedimentSampling = "Sediment sampling",
  RemoteSensing = "Remote sensing", RainfallSimulation = "Rainfall simulation",
  TracerStudy = "Tracer study", other = "Other",
  EventBased = "Event-based", MultiYear = "Multi-year", Snapshot = "Snapshot",
  Seasonal = "Seasonal", Annual = "Annual",
  # moderators
  InfrastructureDesign = "Infrastructure design", TopographySlope = "Topography / slope",
  StormCharacteristics = "Storm characteristics", LandManagement = "Land management",
  CatchmentCharacteristics = "Catchment characteristics", SoilSubstrate = "Soil / substrate",
  Seasonality = "Seasonality", AntecedentConditions = "Antecedent conditions",
  # mitigation
  DrainageBMP = "Drainage measures", Revegetation = "Revegetation",
  SedimentTrap = "Sediment traps", InfiltrationDisconnection = "Infiltration / disconnection",
  SurfaceStabilization = "Surface stabilisation", RoadDecommissioning = "Road decommissioning",
  RoadRelocationClosure = "Road relocation / closure",
  Effective = "Effective", PartiallyEffective = "Partially effective",
  Ineffective = "Ineffective", NotAssessed = "Not assessed"
)

nice <- function(x) {
  x <- as.character(x)
  out <- ifelse(x %in% names(label_map), label_map[x], x)
  # fall back: split remaining CamelCase
  gsub("([a-z])([A-Z])", "\\1 \\2", out)
}
nice_wrap <- function(width = 22) function(x) stringr::str_wrap(nice(x), width)

# Panel restyler: short title, no subtitle, readable y labels.
restyle <- function(p, title, y_wrap = 22, subtitle = NULL) {
  suppressMessages(
    p + labs(title = title, subtitle = subtitle) +
      scale_y_discrete(labels = nice_wrap(y_wrap)) +
      theme(plot.title = element_text(face = "bold", size = 8.5),
            plot.title.position = "plot")
  )
}
relegend_fill <- function(p, pal, levels = names(pal)) {
  suppressMessages(p + scale_fill_manual(values = pal, breaks = levels,
                                         labels = nice, name = NULL))
}

# Panel letters are written into the titles ("(a) ...") rather than added as
# patchwork tags, which collide with left-aligned titles.
tag_theme <- theme()
tags <- plot_annotation()

save_fig <- function(p, name, width_cm, height_cm) {
  ggsave(file.path(figures_dir, paste0(name, ".pdf")), p,
         width = width_cm, height = height_cm, units = "cm", device = cairo_pdf)
  ggsave(file.path(figures_dir, paste0(name, ".png")), p,
         width = width_cm, height = height_cm, units = "cm", dpi = 300)
  invisible(p)
}

# 3  fig_corpus  (C027) --------------------------------------------------------------
f_a <- free(p_rq1a + labs(title = "(a) Publications per year", subtitle = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(face = "bold", size = 8.5), plot.title.position = "plot"))
f_b <- restyle(p_rq1b, "(b) Study region")
f_c <- p_rq1c + labs(title = "(c) Journals (ten most frequent)", subtitle = NULL) +
  theme(plot.title = element_text(face = "bold", size = 8.5), plot.title.position = "plot")
f_d <- restyle(p_rq1d, "(d) Study type") + labs(x = "Records")

fig_corpus <- (f_a | f_b) / (f_c | f_d) + tags & tag_theme
save_fig(fig_corpus, "fig_corpus", W_FULL, 14)

# 4  fig_infra  (C032, C033) ------------------------------------------------------
i_a <- restyle(p_infra_cov, "(a) Element type", subtitle = "Records reporting each type")
i_b <- restyle(p_infra_class, "(b) Element classes per record") + labs(x = "Records")
i_c <- relegend_fill(restyle(pA2, "(c) Link to the stream network",
                             subtitle = "Characterised elements (n = 95)"),
                     pal_topology, c("StreamConnected", "Mixed", "Disconnected", "NotStated")) +
  labs(x = "Elements") + guides(fill = guide_legend(nrow = 2))
i_d <- restyle(pB2, "(d) Reported condition") + labs(x = "Elements")

fig_infra <- ((i_a / i_b) + plot_layout(heights = c(5, 3))) |
             ((i_c / i_d) + plot_layout(heights = c(5, 6)))
fig_infra <- fig_infra + tags & tag_theme
save_fig(fig_infra, "fig_infra", W_FULL, 12)

# 5  fig_measure  (C034, C040) ----------------------------------------------------
m_a <- relegend_fill(restyle(pA3, "(a) Metric family", y_wrap = 26,
                             subtitle = "Connectivity-metric instances (n = 225)"),
                     pal_conn_type, c("Structural", "Functional", "Both"))
m_b <- restyle(pB3, "(b) Operationalisation basis", y_wrap = 26)
m_c <- restyle(pA4, "(c) Measurement method", subtitle = "Primary studies (n = 55)")
m_d <- restyle(pB4, "(d) Temporal scale of observation")

fig_measure <- ((m_a / m_b) + plot_layout(heights = c(8, 4))) |
               ((m_c / m_d) + plot_layout(heights = c(9, 6)))
fig_measure <- fig_measure + plot_layout(widths = c(1.15, 1)) + tags & tag_theme
save_fig(fig_measure, "fig_measure", W_FULL, 13)

# 6  fig_gap ----------------------------------------------------------------------------
gap_infra_lab <- setNames(sprintf("%s  (%d)", nice(infra_tot$infra), infra_tot$t), infra_tot$infra)
fig_gap <- suppressMessages(
  p_gap + labs(title = NULL, subtitle = NULL) +
    scale_y_discrete(labels = gap_infra_lab)
)
save_fig(fig_gap, "fig_gap", W_FULL, 10)

# 7  fig_effects  (C044: connectivity first) ------------------------------------------
e_a <- suppressMessages(
  pB + labs(title = "(a) Effect on connectivity, by infrastructure type",
            subtitle = NULL, x = NULL) +
    scale_y_discrete(labels = nice_wrap(24)))
e_b <- suppressMessages(
  pA + labs(title = "(b) Effect on hydrological processes, all infrastructure pooled",
            subtitle = NULL,
            x = "Number of reported effects (decreases left of zero, increases right)") +
    scale_y_discrete(labels = nice_wrap(24)))

fig_effects <- (e_a / e_b) +
  plot_layout(guides = "collect", heights = c(5, 5)) +
  tags &
  guides(fill  = guide_legend(order = 1, override.aes = list(alpha = 1)),
         alpha = guide_legend(order = 2, override.aes = list(fill = "grey25"))) &
  scale_alpha_manual(values = mag_alpha, name = "Magnitude",
                     breaks = mag_levels, limits = mag_levels, labels = nice) &
  theme(legend.position = "bottom", legend.box = "vertical",
        plot.title = element_text(face = "bold", size = 8.5),
        plot.title.position = "plot") &
  tag_theme
save_fig(fig_effects, "fig_effects", W_FULL, 13)

# 8  fig_moderators -------------------------------------------------------------------
fig_moderators <- restyle(p_rq5b, NULL, y_wrap = 24) + labs(x = "Effects")
save_fig(fig_moderators, "fig_moderators", W_COL, 8)

# 9  fig_mitigation  (C051) -------------------------------------------------------
t_a <- restyle(p_mit_yesno, "(a) Records reporting mitigation") +
  scale_y_discrete(labels = function(x) gsub("\n", " ", x))
t_b <- suppressMessages(
  restyle(p_mit_cross, "(b) Element types in records with and without mitigation") +
    scale_y_discrete(labels = nice_wrap(22)) + labs(x = "Records"))
t_c <- relegend_fill(restyle(p_rq6b, "(c) Reported effectiveness by measure type",
                             subtitle = "Measures (n = 50) from 16 studies"),
                     pal_effect,
                     c("Effective", "PartiallyEffective", "Mixed", "Ineffective", "NotAssessed")) +
  guides(fill = guide_legend(nrow = 2))

fig_mitigation <- ((t_a / t_b) + plot_layout(heights = c(1, 3))) | t_c
fig_mitigation <- fig_mitigation + plot_layout(widths = c(1, 1.25)) + tags & tag_theme
save_fig(fig_mitigation, "fig_mitigation", W_FULL, 11)


# 10  fig_S1_search  (supplement) -----------------------------------------------
# Moved to search_export.R (Oct 2026). The earlier version stacked
# (string x database) tag intersections, which overcount records that carry
# several string and database tags. search_export.R plots exact unique-record
# counts per string, split by the databases that returned them.

message("Manuscript v2 figures written to ", figures_dir, ": fig_corpus, fig_infra, ",
        "fig_measure, fig_gap, fig_effects, fig_moderators, fig_mitigation")
