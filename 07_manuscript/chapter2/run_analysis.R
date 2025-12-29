# Master Script: Run Complete Chapter 2 Analysis Pipeline
# =========================================================
# Purpose: Execute all analysis scripts in correct order
# Usage: source("run_analysis.R") or Rscript run_analysis.R
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(here)

# Set working directory to chapter2 directory
chapter2_dir <- here("07_manuscript", "chapter2")
setwd(chapter2_dir)

cat("========================================\n")
cat("Chapter 2 Analysis Pipeline\n")
cat("========================================\n\n")

# Load path configuration
source(file.path(chapter2_dir, "config", "paths_config.R"))

# Script execution order
scripts <- c(
  "01_load_and_validate_data.R",
  "02_compute_pf_parameters.R",
  "03_pupil_quality_tiers.R",
  "04_effort_pupil_manipulation_check.R",
  "05_missingness_diagnostic.R",
  "06_pupil_psychometric_coupling.R",
  "07_pf_pupil_subject_coupling.R",
  "08_generate_figures.R"
)

scripts_dir <- file.path(chapter2_dir, "scripts")

# Run each script
for (i in seq_along(scripts)) {
  script_path <- file.path(scripts_dir, scripts[i])
  
  if (!file.exists(script_path)) {
    cat(sprintf("⚠ Warning: Script %s not found, skipping...\n", scripts[i]))
    next
  }
  
  cat(sprintf("\n[%d/%d] Running: %s\n", i, length(scripts), scripts[i]))
  cat("----------------------------------------\n")
  
  tryCatch({
    source(script_path)
    cat(sprintf("✓ Completed: %s\n", scripts[i]))
  }, error = function(e) {
    cat(sprintf("✗ ERROR in %s:\n", scripts[i]))
    cat(sprintf("  %s\n", conditionMessage(e)))
    cat("  Continuing with next script...\n")
  })
  
  cat("\n")
}

cat("========================================\n")
cat("Analysis pipeline complete!\n")
cat("========================================\n")
cat("\nNext steps:\n")
cat("1. Review output files in:", output_dir, "\n")
cat("2. Render Quarto report: quarto render", report_file, "\n")
cat("3. Check figures in:", figures_dir, "\n")
cat("4. Review tables in:", tables_dir, "\n")

