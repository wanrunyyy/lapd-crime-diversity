# ============================================================
# run_all.R
# Reproduce the complete thesis analysis
# ============================================================

cat("\n1/3 Cleaning data...\n")
source("R/01_data_cleaning.R")

cat("\n2/3 Running analyses...\n")
source("R/02_analysis.R")

cat("\n3/3 Creating figures...\n")
source("R/03_visualization.R")

cat("\n============================================\n")
cat("Complete thesis analysis reproduced.\n")
cat("============================================\n")