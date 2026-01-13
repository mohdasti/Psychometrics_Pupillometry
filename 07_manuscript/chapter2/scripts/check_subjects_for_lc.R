# Helper Script: Check Which Subjects Are in Merged Data
# ========================================================
# Purpose: Verify which subjects (especially those awaiting LC integrity scores)
#          are present in the merged behavioral + pupil dataset
# 
# Usage: Run this to check if subjects BAP 191-202 have behavioral/pupil data
#
# Author: Mohammad Dastgheib
# Date: Created for LC integrity status check

library(tidyverse)
library(here)

# Source paths
source(file.path(here(), "config", "paths_config.R"))

cat("=== Checking Subjects in Merged Data ===\n\n")

# Load merged data
merged_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")

if (!file.exists(merged_file)) {
  stop("ERROR: Merged data file not found at: ", merged_file)
}

cat("Loading merged data...\n")
dat <- read_csv(merged_file, show_col_types = FALSE)

# Get all subjects in data
subjects_in_data <- unique(dat$sub)
cat("Total subjects in merged data:", length(subjects_in_data), "\n\n")

# Subjects awaiting LC integrity scores
waiting_subjects <- c("BAP 191", "BAP 192", "BAP 194", "BAP 195", "BAP 196", 
                      "BAP 197", "BAP 199", "BAP 200", "BAP 201", "BAP 202")

# Check different possible formats
waiting_subjects_no_space <- gsub(" ", "", waiting_subjects)
waiting_subjects_upper <- toupper(waiting_subjects_no_space)
waiting_subjects_lower <- tolower(waiting_subjects_no_space)

# Also check if subject IDs in data have different formats
subjects_in_data_upper <- toupper(subjects_in_data)
subjects_in_data_lower <- tolower(subjects_in_data)

cat("=== Subjects Awaiting LC Integrity Scores ===\n")
cat("Checking for:", paste(waiting_subjects, collapse = ", "), "\n\n")

# Try to match in different formats
found_subjects <- c()
missing_subjects <- c()

for (sub in waiting_subjects) {
  # Try multiple formats
  formats_to_try <- c(
    sub,                    # "BAP 191"
    gsub(" ", "", sub),      # "BAP191"
    toupper(gsub(" ", "", sub)),  # "BAP191"
    tolower(gsub(" ", "", sub))   # "bap191"
  )
  
  matched <- FALSE
  matched_format <- NULL
  
  for (fmt in formats_to_try) {
    if (fmt %in% subjects_in_data || 
        toupper(fmt) %in% subjects_in_data_upper ||
        tolower(fmt) %in% subjects_in_data_lower) {
      matched <- TRUE
      matched_format <- fmt
      break
    }
  }
  
  if (matched) {
    found_subjects <- c(found_subjects, sub)
    cat("✓ FOUND:", sub, "(matched as:", matched_format, ")\n")
  } else {
    missing_subjects <- c(missing_subjects, sub)
    cat("✗ MISSING:", sub, "\n")
  }
}

cat("\n=== Summary ===\n")
cat("Subjects with behavioral/pupil data:", length(found_subjects), "/", length(waiting_subjects), "\n")
cat("Subjects missing from data:", length(missing_subjects), "/", length(waiting_subjects), "\n")

if (length(found_subjects) > 0) {
  cat("\n✓ These subjects have behavioral/pupil data and are ready for LC integrity analysis:\n")
  cat("  ", paste(found_subjects, collapse = ", "), "\n")
}

if (length(missing_subjects) > 0) {
  cat("\n✗ These subjects are NOT in the merged data:\n")
  cat("  ", paste(missing_subjects, collapse = ", "), "\n")
  cat("\n  Note: They may need to be added to the merged dataset, or their IDs may be formatted differently.\n")
}

# Also show sample of all subject IDs to help identify format
cat("\n=== Sample of Subject IDs in Data (first 20) ===\n")
cat("  ", paste(head(subjects_in_data, 20), collapse = ", "), "\n")
if (length(subjects_in_data) > 20) {
  cat("  ... and", length(subjects_in_data) - 20, "more\n")
}

cat("\n=== Next Steps ===\n")
if (length(found_subjects) == length(waiting_subjects)) {
  cat("✓ All waiting subjects are in the merged data!\n")
  cat("  → Once LC integrity scores are added to the master spreadsheet:\n")
  cat("    1. Extract LC data to data/processed/ch2_lc_integrity.csv\n")
  cat("    2. Run: source('04_pupil_psychometric_coupling/06b_lc_integrity_extension.R')\n")
  cat("    3. Re-render the QMD to see results\n")
} else {
  cat("⚠ Some subjects are missing from the merged data.\n")
  cat("  → Check if their IDs are formatted differently\n")
  cat("  → Or if they need to be added to the merged dataset\n")
}

cat("\n=== Script Complete ===\n")



