# Configuration file for Chapter 2 analysis paths
# ================================================
# Purpose: Centralized path configuration for all scripts
# Usage: source this file at the beginning of analysis scripts
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(here)

# Base directory (should be repository root)
base_dir <- here()

# Chapter 2 specific directories
chapter2_dir <- file.path(base_dir, "07_manuscript", "chapter2")

# Data directories
data_dir <- file.path(chapter2_dir, "data")
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

# Output directories
output_dir <- file.path(chapter2_dir, "output")
figures_dir <- file.path(output_dir, "figures")
tables_dir <- file.path(output_dir, "tables")
models_dir <- file.path(output_dir, "models")

# Scripts directory
scripts_dir <- file.path(chapter2_dir, "scripts")

# Reports directory
reports_dir <- file.path(chapter2_dir, "reports")
report_file <- file.path(reports_dir, "chap2_psychometric_pupil.qmd")

# Create output directories if they don't exist
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(qc_dir, showWarnings = FALSE, recursive = TRUE)

# Print configuration (optional, for debugging)
if (FALSE) {  # Set to TRUE to print paths when sourcing
  cat("Chapter 2 Path Configuration:\n")
  cat("  Base directory:", base_dir, "\n")
  cat("  Chapter 2 directory:", chapter2_dir, "\n")
  cat("  Data directory:", data_dir, "\n")
  cat("  Output directory:", output_dir, "\n")
}

