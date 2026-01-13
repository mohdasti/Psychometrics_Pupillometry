# Master Script: Run Complete Chapter 2 Analysis Pipeline
# =========================================================
# Purpose: Execute all analysis scripts in correct order
# Usage: source("run_analysis.R") or Rscript run_analysis.R
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis
# Updated: Reorganized into numbered folder structure

library(here)

# Set working directory to repository root
base_dir <- here()
setwd(base_dir)

cat("========================================\n")
cat("Chapter 2 Analysis Pipeline\n")
cat("Reorganized Structure\n")
cat("========================================\n\n")

# Load path configuration
source(file.path(base_dir, "config", "paths_config.R"))

# Script execution order (by analysis stage)
scripts <- list(
  # Stage 1: Data Preparation
  list(stage = "01_data_preparation", file = "01_load_and_validate_data.R"),
  list(stage = "01_data_preparation", file = "02_compute_pf_parameters.R"),
  
  # Stage 2: Pupil Quality Control
  list(stage = "02_pupil_quality_control", file = "03_pupil_quality_tiers.R"),
  list(stage = "02_pupil_quality_control", file = "05_missingness_diagnostic.R"),
  
  # Stage 3: Effort Manipulation Check
  list(stage = "03_effort_manipulation_check", file = "04_effort_pupil_manipulation_check.R"),
  
  # Stage 4: Primary Analysis - Pupil-Psychometric Coupling
  list(stage = "04_pupil_psychometric_coupling", file = "06_pupil_psychometric_coupling.R"),
  
  # Stage 5: Subject-Level Analysis
  list(stage = "05_subject_level_analysis", file = "07_pf_pupil_subject_coupling.R"),
  
  # Stage 6: Visualization
  list(stage = "06_visualization", file = "08_generate_figures.R")
)

# Run each script
for (i in seq_along(scripts)) {
  script_info <- scripts[[i]]
  script_path <- file.path(base_dir, script_info$stage, script_info$file)
  
  if (!file.exists(script_path)) {
    cat(sprintf("⚠ Warning: Script %s/%s not found, skipping...\n", 
                script_info$stage, script_info$file))
    next
  }
  
  cat(sprintf("\n[%d/%d] Running: %s/%s\n", 
              i, length(scripts), script_info$stage, script_info$file))
  cat("----------------------------------------\n")
  
  tryCatch({
    source(script_path)
    cat(sprintf("✓ Completed: %s/%s\n", script_info$stage, script_info$file))
  }, error = function(e) {
    cat(sprintf("✗ ERROR in %s/%s:\n", script_info$stage, script_info$file))
    cat(sprintf("  %s\n", conditionMessage(e)))
    cat("  Continuing with next script...\n")
  })
  
  cat("\n")
}

cat("========================================\n")
cat("Analysis pipeline complete!\n")
cat("========================================\n")
cat("\nNext steps:\n")
cat("1. Review output files in:", visualization_output, "\n")
cat("2. Render Quarto report: quarto render", report_file, "\n")
cat("3. Check figures in:", figures_dir, "\n")
cat("4. Review tables in:", tables_dir, "\n")



