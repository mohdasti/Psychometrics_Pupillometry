# Script 3: Define Pupil Quality Tiers and Compute Within-Subject Centered Metrics
# ================================================================================
# Purpose: Define quality tiers based on window validity thresholds
#          Compute within-subject centered pupil metrics (state vs trait)
#          Updated January 2026: Includes gap-aware QC metrics per FILTERING_GUIDE.md
# Input: ch2_triallevel_merged.csv
# Output: Updated dataset with quality tier flags and centered pupil metrics
#
# Quality Tiers:
#   - Primary: baseline ≥ 0.60 AND cognitive ≥ 0.60 
#             AND cog_auc_max_gap_ms ≤ 250 (if exists)
#             AND cog_window_duration ≥ 0.5 (if exists)
#             AND cog_auc_n_valid ≥ 100 (if exists)
#   - Lenient: baseline ≥ 0.50 AND cognitive ≥ 0.50
#             AND cog_auc_max_gap_ms ≤ 250 (if exists)
#             AND cog_window_duration ≥ 0.3 (if exists)
#   - Strict: baseline ≥ 0.70 AND cognitive ≥ 0.70
#            AND cog_auc_max_gap_ms ≤ 250 (if exists)
#            AND cog_window_duration ≥ 0.5 (if exists)
#            AND cog_auc_n_valid ≥ 100 (if exists)
#
# See chapter2_materials/docs/FILTERING_GUIDE.md for detailed rationale
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis, updated January 2026

library(tidyverse)
library(here)

# Load path configuration
source(file.path(here(), "config", "paths_config.R"))

# Load data
cat("Loading trial-level data...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

# ============================================================================
# DEFINE QUALITY TIERS (WITH GAP-AWARE FILTERING)
# ============================================================================
# Updated January 2026: Includes gap-aware QC metrics per FILTERING_GUIDE.md
# Gap-aware metrics help identify trials with problematic missing data patterns
# even when overall quality looks acceptable (e.g., large gaps during peak response)
# ============================================================================

cat("\n=== Defining Quality Tiers (with Gap-Aware Filtering) ===\n")

# Check if quality columns exist
if (!"baseline_quality" %in% names(dat) || !"cog_quality" %in% names(dat)) {
  stop("ERROR: Required quality columns (baseline_quality, cog_quality) not found")
}

# Check for gap-aware QC metrics (may not exist in all datasets)
has_gap_metrics <- all(c("cog_auc_max_gap_ms", "cog_window_duration", "cog_auc_n_valid") %in% names(dat))
if (has_gap_metrics) {
  cat("✓ Gap-aware QC metrics found - applying gap-aware filtering\n")
} else {
  cat("⚠ Gap-aware QC metrics not found - using percentage-validity only\n")
  cat("  Missing: cog_auc_max_gap_ms, cog_window_duration, cog_auc_n_valid\n")
}

# Base quality thresholds (percentage-validity)
base_primary <- dat$baseline_quality >= 0.60 & dat$cog_quality >= 0.60
base_lenient <- dat$baseline_quality >= 0.50 & dat$cog_quality >= 0.50
base_strict <- dat$baseline_quality >= 0.70 & dat$cog_quality >= 0.70

# Gap-aware filtering (Option A: only apply if metrics exist)
# Primary (Strict): baseline ≥ 0.60 AND cognitive ≥ 0.60 
#                   AND cog_auc_max_gap_ms ≤ 250 (if exists)
#                   AND cog_window_duration ≥ 0.5 (if exists)
#                   AND cog_auc_n_valid ≥ 100 (if exists)
if (has_gap_metrics) {
  gap_primary <- (is.na(dat$cog_auc_max_gap_ms) | dat$cog_auc_max_gap_ms <= 250) &
                 (is.na(dat$cog_window_duration) | dat$cog_window_duration >= 0.5) &
                 (is.na(dat$cog_auc_n_valid) | dat$cog_auc_n_valid >= 100)
  dat$quality_primary <- base_primary & gap_primary
} else {
  dat$quality_primary <- base_primary
}

# Lenient: baseline ≥ 0.50 AND cognitive ≥ 0.50
#          AND cog_auc_max_gap_ms ≤ 250 (if exists)
#          AND cog_window_duration ≥ 0.3 (if exists) - allows faster RTs
if (has_gap_metrics) {
  gap_lenient <- (is.na(dat$cog_auc_max_gap_ms) | dat$cog_auc_max_gap_ms <= 250) &
                 (is.na(dat$cog_window_duration) | dat$cog_window_duration >= 0.3)
  dat$quality_lenient <- base_lenient & gap_lenient
} else {
  dat$quality_lenient <- base_lenient
}

# Strict: baseline ≥ 0.70 AND cognitive ≥ 0.70
#         AND cog_auc_max_gap_ms ≤ 250 (if exists)
#         AND cog_window_duration ≥ 0.5 (if exists)
#         AND cog_auc_n_valid ≥ 100 (if exists)
if (has_gap_metrics) {
  gap_strict <- (is.na(dat$cog_auc_max_gap_ms) | dat$cog_auc_max_gap_ms <= 250) &
               (is.na(dat$cog_window_duration) | dat$cog_window_duration >= 0.5) &
               (is.na(dat$cog_auc_n_valid) | dat$cog_auc_n_valid >= 100)
  dat$quality_strict <- base_strict & gap_strict
} else {
  dat$quality_strict <- base_strict
}

cat("\nQuality tier sample sizes:\n")
cat("  Primary (≥0.60 + gap-aware): ", sum(dat$quality_primary, na.rm = TRUE), " trials\n")
cat("  Lenient (≥0.50 + gap-aware): ", sum(dat$quality_lenient, na.rm = TRUE), " trials\n")
cat("  Strict (≥0.70 + gap-aware):  ", sum(dat$quality_strict, na.rm = TRUE), " trials\n")

if (has_gap_metrics) {
  n_with_gap_metrics <- sum(!is.na(dat$cog_auc_max_gap_ms))
  n_excluded_by_gap <- sum(base_primary, na.rm = TRUE) - sum(dat$quality_primary, na.rm = TRUE)
  cat("\nGap-aware filtering impact:\n")
  cat("  Trials with gap metrics: ", n_with_gap_metrics, "\n")
  cat("  Additional exclusions by gap criteria: ", n_excluded_by_gap, " trials\n")
}

# Check if gate_pupil_primary already exists and compare
if ("gate_pupil_primary" %in% names(dat)) {
  cat("\nComparing with existing gate_pupil_primary:\n")
  comparison <- table(dat$quality_primary, dat$gate_pupil_primary, useNA = "always")
  print(comparison)
  
  # Use existing if it matches, otherwise use our computed version
  if (all(dat$quality_primary == dat$gate_pupil_primary, na.rm = TRUE)) {
    cat("✓ quality_primary matches gate_pupil_primary\n")
  } else {
    cat("⚠ quality_primary differs from gate_pupil_primary, using computed version\n")
  }
}

# ============================================================================
# COMPUTE WITHIN-SUBJECT CENTERED PUPIL METRICS
# ============================================================================

cat("\n=== Computing Within-Subject Centered Pupil Metrics ===\n")

# Identify primary cognitive pupil metric
# Check for cog_auc (fixed-window cognitive AUC) or alternative metric
if (!"cog_auc" %in% names(dat)) {
  warning("cog_auc not found, checking for alternative cognitive pupil metrics...")
  # Could use total_auc or another metric if cog_auc unavailable
  if ("total_auc" %in% names(dat)) {
    dat$cog_auc <- dat$total_auc
    cat("Using total_auc as cognitive pupil metric\n")
  } else {
    stop("Cannot find cognitive pupil metric (cog_auc or total_auc)")
  }
}

# Compute subject-level means (trait component)
cat("Computing subject-level means (trait component)...\n")

# Check which AUC columns are available
has_cog_auc <- "cog_auc" %in% names(dat) && sum(!is.na(dat$cog_auc)) > 0
has_total_auc <- "total_auc" %in% names(dat) && sum(!is.na(dat$total_auc)) > 0

if (!has_cog_auc) {
  stop("ERROR: cog_auc column not found or all values are NA")
}

cat("  cog_auc available: ", has_cog_auc, " (", sum(!is.na(dat$cog_auc)), " non-NA values)\n", sep = "")
cat("  total_auc available: ", has_total_auc, " (", if(has_total_auc) sum(!is.na(dat$total_auc)) else 0, " non-NA values)\n", sep = "")

# Ensure sub is character for consistent joining
dat$sub <- as.character(dat$sub)

# Compute subject means for cognitive AUC
cat("  Computing subject means...\n")
subject_means <- dat %>%
  filter(!is.na(cog_auc)) %>%
  mutate(sub = as.character(sub)) %>%
  group_by(sub) %>%
  summarise(
    pupil_cognitive_trait = mean(cog_auc, na.rm = TRUE),
    n_trials_with_cog_auc = n(),
    .groups = "drop"
  )

cat("  Subject means computed for", nrow(subject_means), "subjects\n")
cat("  Sample subject means:\n")
print(head(subject_means))

# Add total AUC trait if available
if (has_total_auc) {
  total_auc_means <- dat %>%
    filter(!is.na(total_auc)) %>%
    mutate(sub = as.character(sub)) %>%
    group_by(sub) %>%
    summarise(
      pupil_total_auc_trait = mean(total_auc, na.rm = TRUE),
      .groups = "drop"
    )
  subject_means <- subject_means %>%
    left_join(total_auc_means, by = "sub")
}

# Remove old trait/state columns if they exist (from previous runs)
old_cols <- c("pupil_cognitive_trait", "pupil_total_auc_trait", 
              "pupil_cognitive_state", "pupil_total_auc_state",
              "n_trials_with_pupil", "n_trials_with_cog_auc")
cols_to_remove <- intersect(old_cols, names(dat))
if (length(cols_to_remove) > 0) {
  cat("  Removing old trait/state columns from previous run:", paste(cols_to_remove, collapse = ", "), "\n")
  dat <- dat %>% select(-any_of(cols_to_remove))
}

# Also remove any .x or .y versions that might exist
dat <- dat %>% select(-ends_with(".x"), -ends_with(".y"))

# Merge trait means back into dataset
cat("Merging trait means back into dataset...\n")
cat("  dat has", nrow(dat), "rows, subject_means has", nrow(subject_means), "rows\n")
cat("  Unique subjects in dat:", length(unique(dat$sub)), "\n")
cat("  Unique subjects in subject_means:", length(unique(subject_means$sub)), "\n")

# Perform the join
dat <- dat %>%
  left_join(subject_means, by = "sub")

# Verify join worked
if (!"pupil_cognitive_trait" %in% names(dat)) {
  stop("ERROR: Join failed - pupil_cognitive_trait column not found after join")
}

n_with_trait <- sum(!is.na(dat$pupil_cognitive_trait))
cat("  Rows with trait values:", n_with_trait, "out of", nrow(dat), "\n")

if (n_with_trait == 0) {
  stop("ERROR: Join failed - all pupil_cognitive_trait values are NA")
}

# Compute within-subject centered metrics (state component)
# State = trial value - subject mean
cat("Computing state components (trial - subject mean)...\n")
dat <- dat %>%
  mutate(
    pupil_cognitive_state = cog_auc - pupil_cognitive_trait
  )

if (has_total_auc && "pupil_total_auc_trait" %in% names(dat)) {
  dat <- dat %>%
    mutate(
      pupil_total_auc_state = total_auc - pupil_total_auc_trait
    )
}

cat("✓ Computed within-subject centered metrics\n")
cat("  Mean trait (cognitive): ", mean(dat$pupil_cognitive_trait, na.rm = TRUE), "\n")
cat("  SD trait (cognitive): ", sd(dat$pupil_cognitive_trait, na.rm = TRUE), "\n")
cat("  Mean state (cognitive): ", mean(dat$pupil_cognitive_state, na.rm = TRUE), " (should be ~0)\n")
cat("  SD state (cognitive): ", sd(dat$pupil_cognitive_state, na.rm = TRUE), "\n")

# ============================================================================
# SAVE UPDATED DATASET
# ============================================================================

cat("\n=== Saving updated dataset with quality tiers ===\n")
write_csv(dat, dat_file)
cat("✓ Updated dataset saved to:", dat_file, "\n")

# ============================================================================
# CREATE QUALITY SUMMARY TABLE
# ============================================================================

cat("\n=== Creating quality summary ===\n")
quality_summary <- dat %>%
  group_by(sub, task) %>%
  summarise(
    n_trials = n(),
    n_primary = sum(quality_primary, na.rm = TRUE),
    n_lenient = sum(quality_lenient, na.rm = TRUE),
    n_strict = sum(quality_strict, na.rm = TRUE),
    prop_primary = mean(quality_primary, na.rm = TRUE),
    prop_lenient = mean(quality_lenient, na.rm = TRUE),
    prop_strict = mean(quality_strict, na.rm = TRUE),
    mean_baseline_quality = mean(baseline_quality, na.rm = TRUE),
    mean_cog_quality = mean(cog_quality, na.rm = TRUE),
    .groups = "drop"
  )

quality_output <- file.path(qc_dir, "pupil_quality_summary.csv")
write_csv(quality_summary, quality_output)
cat("✓ Quality summary saved to:", quality_output, "\n")

cat("\n=== Quality tier computation complete ===\n")

