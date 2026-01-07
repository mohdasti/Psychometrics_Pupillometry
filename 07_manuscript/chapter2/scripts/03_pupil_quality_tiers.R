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

# Set paths
data_dir <- here("07_manuscript", "chapter2", "data")
processed_dir <- file.path(data_dir, "processed")
qc_dir <- file.path(data_dir, "qc")

# Load data
cat("Loading trial-level data...\n")
dat_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")
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
subject_means <- dat %>%
  filter(!is.na(cog_auc)) %>%
  group_by(sub) %>%
  summarise(
    pupil_cognitive_trait = mean(cog_auc, na.rm = TRUE),
    pupil_total_auc_trait = mean(total_auc, na.rm = TRUE),
    n_trials_with_pupil = n(),
    .groups = "drop"
  )

# Merge trait means back into dataset
dat <- dat %>%
  left_join(subject_means, by = "sub")

# Compute within-subject centered metrics (state component)
# State = trial value - subject mean
dat$pupil_cognitive_state <- dat$cog_auc - dat$pupil_cognitive_trait
dat$pupil_total_auc_state <- dat$total_auc - dat$pupil_total_auc_trait

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

