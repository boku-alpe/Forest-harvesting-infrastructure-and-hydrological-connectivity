# =============================================================================
# run_all.R  -  reproduces every figure, table, and supplementary table
# Run from this folder:  Rscript run_all.R
# =============================================================================
if (!file.exists(".here")) file.create(".here")

source("build_cite_map.R")              # cite_map.csv from AquaSilva.bib + aquasilva.csv
rm(list = ls())
source("DR_AS_SLR_manuscript_figures.R")  # main-text figures (sources the pipeline)
rm(list = ls())
source("writeup_export.R")              # evidence tables S6 + citation clusters
source("review_synthesis.R")            # review synthesis S5
rm(list = ls())
source("screening_export.R")            # screening tables S3 + screening_decisions.csv
rm(list = ls())
source("search_export.R")               # search tables S2 + Fig. S1

message("All outputs written to figures/, tables/, and writeup/.")
