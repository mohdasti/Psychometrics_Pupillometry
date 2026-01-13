# Script 1: Load and Validate Data for Chapter 2
# ================================================
# Purpose: Load pre-merged trial-level data and validate structure
# Input: ch2_triallevel_merged.csv (pre-merged behavioral + pupil data)
# Output: Validated dataset saved as ch2_triallevel_merged.csv
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

# Load required libraries
library(tidyverse)
library(here)

# Set paths
data_dir <- here("07_manuscript", "chapter2", "data")
processed_dir <- file.path(data_dir, "processed")
raw_dir <- file.path(data_dir, "raw")

# ============================================================================
# PRIMARY APPROACH: Load pre-merged data
# ============================================================================
# The file ch2_triallevel_merged.csv is already merged from quick_share_v7
# This contains both behavioral and pupil data with quality metrics

cat("Loading pre-merged trial-level data...\n")
merged_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")

if (!file.exists(merged_file)) {
  stop("ERROR: Pre-merged data file not found at: ", merged_file, "\n",
       "Please ensure ch2_triallevel.csv has been copied from quick_share_v7/analysis_ready/")
}

dat <- read_csv(merged_file, show_col_types = FALSE)
cat("Loaded", nrow(dat), "trials from", length(unique(dat$sub)), "subjects\n")

# ============================================================================
# ALTERNATIVE APPROACH (if needed): Load and merge separate files
# ============================================================================
# Uncomment if you need to merge from scratch:
# 
# beh_file <- file.path(raw_dir, "behavioral", "bap_beh_trialdata_v2.csv")
# pupil_file <- file.path(processed_dir, "alternative_pupil_source.csv")  # Specify if needed
# 
# if (file.exists(beh_file) && file.exists(pupil_file)) {
#   cat("Loading behavioral data...\n")
#   beh <- read_csv(beh_file, show_col_types = FALSE)
#   
#   cat("Loading pupil data...\n")
#   pupil <- read_csv(pupil_file, show_col_types = FALSE)
#   
#   # Merge on common identifiers (adjust based on actual column names)
#   # Example: dat <- left_join(beh, pupil, by = c("subject_id" = "sub", "trial_num" = "trial_index"))
# }

# ============================================================================
# VALIDATION: Check required columns
# ============================================================================

cat("\n=== Validating data structure ===\n")

# Required behavioral columns
req_beh <- c("sub", "task", "effort", "stimulus_intensity", "choice", "rt", "correct")
missing_beh <- setdiff(req_beh, names(dat))
if (length(missing_beh) > 0) {
  warning("Missing behavioral columns: ", paste(missing_beh, collapse = ", "))
} else {
  cat("✓ All required behavioral columns present\n")
}

# Required pupil columns
req_pupil <- c("baseline_quality", "cog_quality", "overall_quality", 
               "total_auc", "cog_auc", "gate_pupil_primary")
missing_pupil <- setdiff(req_pupil, names(dat))
if (length(missing_pupil) > 0) {
  warning("Missing pupil columns: ", paste(missing_pupil, collapse = ", "))
} else {
  cat("✓ All required pupil columns present\n")
}

# Check for duplicate rows
n_duplicates <- sum(duplicated(dat))
if (n_duplicates > 0) {
  warning("Found ", n_duplicates, " duplicate rows")
} else {
  cat("✓ No duplicate rows found\n")
}

# Check stimulus intensity is continuous (not binned)
if ("stimulus_intensity" %in% names(dat)) {
  n_unique_intensity <- length(unique(dat$stimulus_intensity))
  cat("✓ Stimulus intensity has", n_unique_intensity, "unique values (continuous)\n")
  
  # Verify it's numeric
  if (!is.numeric(dat$stimulus_intensity)) {
    warning("stimulus_intensity is not numeric, converting...")
    dat$stimulus_intensity <- as.numeric(dat$stimulus_intensity)
  }
}

# Check for missing values in key columns
cat("\nMissing values in key columns:\n")
key_cols <- c("sub", "task", "effort", "stimulus_intensity", "choice", "rt")
for (col in key_cols) {
  if (col %in% names(dat)) {
    n_missing <- sum(is.na(dat[[col]]))
    if (n_missing > 0) {
      cat(sprintf("  %s: %d missing (%.2f%%)\n", col, n_missing, 100*n_missing/nrow(dat)))
    } else {
      cat(sprintf("  %s: no missing values\n", col))
    }
  }
}

# ============================================================================
# DATA CLEANING: Standardize column names and values
# ============================================================================

cat("\n=== Standardizing data ===\n")

# Ensure task labels are consistent (ADT/VDT)
if ("task" %in% names(dat)) {
  dat$task <- toupper(dat$task)
  cat("✓ Task labels standardized:", paste(unique(dat$task), collapse = ", "), "\n")
}

# Ensure effort labels are consistent (Low/High)
if ("effort" %in% names(dat)) {
  dat$effort <- str_to_title(dat$effort)
  cat("✓ Effort labels standardized:", paste(unique(dat$effort), collapse = ", "), "\n")
}

# Ensure choice is binary (0/1 or FALSE/TRUE) for modeling
if ("choice" %in% names(dat)) {
  if (is.character(dat$choice) || is.factor(dat$choice)) {
    # Convert "SAME"/"DIFFERENT" or similar to binary
    # Adjust based on actual values in your data
    dat$choice_num <- ifelse(dat$choice == "DIFFERENT" | dat$choice == 1, 1, 0)
  }
}

# ============================================================================
# SAVE VALIDATED DATASET
# ============================================================================

cat("\n=== Saving validated dataset ===\n")
output_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")
write_csv(dat, output_file)
cat("✓ Saved validated dataset to:", output_file, "\n")
cat("  Rows:", nrow(dat), "\n")
cat("  Columns:", ncol(dat), "\n")
cat("  Subjects:", length(unique(dat$sub)), "\n")
cat("  Tasks:", paste(unique(dat$task), collapse = ", "), "\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== Summary Statistics ===\n")
cat("\nSample size by task:\n")
print(table(dat$task))

cat("\nSample size by effort:\n")
print(table(dat$effort))

cat("\nSample size by task × effort:\n")
print(table(dat$task, dat$effort))

cat("\nPupil data availability:\n")
if ("gate_pupil_primary" %in% names(dat)) {
  print(table(dat$gate_pupil_primary, useNA = "always"))
}

cat("\n=== Data loading complete ===\n")



