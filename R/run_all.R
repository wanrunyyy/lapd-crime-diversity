# ============================================================
# run_all.R
#
# Master script for reproducing the entire thesis analysis
#
# Run this file to reproduce all processed data,
# statistical analyses, and figures.
# ============================================================

cat("=============================================\n")
cat(" Crime Diversity Thesis Reproducibility\n")
cat("=============================================\n\n")

# ------------------------------------------------------------
# Step 1: Data cleaning
# ------------------------------------------------------------

cat("Running 01_data_cleaning.R ...\n")

source("R/01_data_cleaning.R")

cat("✓ Data cleaning completed.\n\n")


# ------------------------------------------------------------
# Step 2: Statistical analysis
# ------------------------------------------------------------

cat("Running 02_analysis.R ...\n")

source("R/02_analysis.R")

cat("✓ Statistical analysis completed.\n\n")


# ------------------------------------------------------------
# Step 3: Visualization
# ------------------------------------------------------------

cat("Running 03_visualization.R ...\n")

source("R/03_visualization.R")

cat("✓ Figures created.\n\n")


# ------------------------------------------------------------
# Finished
# ------------------------------------------------------------

cat("=============================================\n")
cat(" All analyses completed successfully!\n")
cat("=============================================\n\n")

cat("Generated files:\n")
cat("  • data/processed/\n")
cat("  • output/tables/\n")
cat("  • output/figures/\n\n")