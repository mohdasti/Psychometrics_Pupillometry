# Configuration file for Chapter 2 analysis paths
# ================================================
# Purpose: Centralized path configuration for all scripts
# Usage: source this file at the beginning of analysis scripts
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis
# Updated: Reorganized into numbered folder structure

library(here)

# Base directory (should be repository root)
base_dir <- here()

# Analysis stage directories
data_prep_dir <- file.path(base_dir, "01_data_preparation")
pupil_qc_dir <- file.path(base_dir, "02_pupil_quality_control")
effort_check_dir <- file.path(base_dir, "03_effort_manipulation_check")
pupil_coupling_dir <- file.path(base_dir, "04_pupil_psychometric_coupling")
subject_analysis_dir <- file.path(base_dir, "05_subject_level_analysis")
visualization_dir <- file.path(base_dir, "06_visualization")
manuscript_dir <- file.path(base_dir, "07_manuscript", "chapter2")

# Shared data directories (at repository root)
data_dir <- file.path(base_dir, "data")
raw_dir <- file.path(data_dir, "raw")
processed_dir <- file.path(data_dir, "processed")
qc_dir <- file.path(data_dir, "qc")

# Behavioral data files (raw)
beh_trial_file <- file.path(raw_dir, "behavioral", "bap_beh_trialdata_v2.csv")
beh_subjxtask_file <- file.path(raw_dir, "behavioral", "bap_beh_subjxtaskdata_v2.csv")
beh_trial_counts_file <- file.path(raw_dir, "behavioral", "bap_beh_trialdata_v2_trials_per_subject_per_task.csv")

# Master spreadsheet files (LC Aging Subject Data)
master_beh_file <- file.path(raw_dir, "behavioral", "LC Aging Subject Data master spreadsheet - behavioral.csv")
master_beh_dict_file <- file.path(raw_dir, "behavioral", "LC Aging Subject Data master spreadsheet - behavioral data dictionary.csv")
master_demo_file <- file.path(raw_dir, "behavioral", "LC Aging Subject Data master spreadsheet - demographics.csv")
master_neuropsych_file <- file.path(raw_dir, "behavioral", "LC Aging Subject Data master spreadsheet - neuropsych.csv")

# Processed data files
merged_trial_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")
pf_params_file <- file.path(processed_dir, "ch2_pf_parameters.csv")
subject_summary_file <- file.path(processed_dir, "ch2_subject_summary.csv")

# QC files
quality_summary_file <- file.path(qc_dir, "pupil_quality_summary.csv")
missingness_file <- file.path(qc_dir, "missingness_diagnostic.csv")
inclusion_bias_file <- file.path(qc_dir, "inclusion_bias_table.csv")

# Output directories (stage-specific)
data_prep_output <- file.path(data_prep_dir, "output")
pupil_qc_output <- file.path(pupil_qc_dir, "output")
effort_check_output <- file.path(effort_check_dir, "output")
pupil_coupling_output <- file.path(pupil_coupling_dir, "output")
subject_analysis_output <- file.path(subject_analysis_dir, "output")
visualization_output <- file.path(visualization_dir, "output")

# Shared output directories (for convenience, point to visualization for figures)
figures_dir <- file.path(visualization_dir, "output", "figures")
tables_dir <- file.path(visualization_dir, "output", "tables")
models_dir <- file.path(visualization_dir, "output", "models")

# Reports directory
reports_dir <- file.path(manuscript_dir)
report_file <- file.path(reports_dir, "chapter2_dissertation.qmd")
pupil_data_report_file <- file.path(reports_dir, "pupil_data_report_advisor.qmd")

# Documentation directory
docs_dir <- file.path(manuscript_dir, "docs")

# Pupil data files (processed)
pupil_trial_file <- file.path(processed_dir, "ch2_triallevel_pupil.csv")
pupil_data_readme <- file.path(processed_dir, "README_data_source.md")

# Data generation script (for regenerating pupil data if needed)
make_pupil_data_script <- file.path(data_prep_dir, "make_quick_share_v7.R")

# Create output directories if they don't exist
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(qc_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_prep_output, showWarnings = FALSE, recursive = TRUE)
dir.create(pupil_qc_output, showWarnings = FALSE, recursive = TRUE)
dir.create(effort_check_output, showWarnings = FALSE, recursive = TRUE)
dir.create(pupil_coupling_output, showWarnings = FALSE, recursive = TRUE)
dir.create(subject_analysis_output, showWarnings = FALSE, recursive = TRUE)
dir.create(visualization_output, showWarnings = FALSE, recursive = TRUE)

# Print configuration (optional, for debugging)
if (FALSE) {  # Set to TRUE to print paths when sourcing
  cat("Chapter 2 Path Configuration:\n")
  cat("  Base directory:", base_dir, "\n")
  cat("  Data directory:", data_dir, "\n")
  cat("  Processed directory:", processed_dir, "\n")
  cat("  Output directory:", visualization_output, "\n")
}
