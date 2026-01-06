# Script 3: Test PF Parameters (Threshold and Slope) by Effort
# ==============================================================
# Purpose: Test whether High effort affects PF parameters (thresholds and slopes)
#          This addresses Hypotheses H1a and H1b
# Input: ch2_pf_parameters.csv
# Output: Model results, tables, and summary statistics
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(lme4)
library(lmerTest)  # Adds p-values to lmer models
library(broom.mixed)
library(here)

# Load path configuration
source(file.path(here(), "config", "paths_config.R"))

# Load PF parameters
cat("Loading PF parameters...\n")
pf_file <- pf_params_file
if (!file.exists(pf_file)) {
  stop("PF parameters file not found: ", pf_file)
}

pf_params <- read_csv(pf_file, show_col_types = FALSE)
cat("✓ Loaded PF parameters\n")
cat("  Rows:", nrow(pf_params), "\n")
cat("  Subjects:", length(unique(pf_params$sub)), "\n")
cat("  Converged fits:", sum(pf_params$converged, na.rm = TRUE), "\n")

# Filter to converged fits only
pf_analysis <- pf_params %>%
  filter(converged == TRUE) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

cat("\nUsing converged fits only: ", nrow(pf_analysis), " observations\n")
cat("  Subjects:", length(unique(pf_analysis$sub)), "\n")

# Check data structure
cat("\nData structure:\n")
pf_analysis %>%
  group_by(task_factor, effort_factor) %>%
  summarise(
    n = n(),
    n_subjects = n_distinct(sub),
    threshold_mean = mean(threshold, na.rm = TRUE),
    threshold_sd = sd(threshold, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    slope_sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ============================================================================
# MODEL 1: Threshold ~ Effort
# ============================================================================

cat("\n=== Model 1: Threshold ~ Effort + Task ===\n")
cat("Hypothesis H1a: Thresholds higher under High effort\n\n")

# Fit mixed-effects model using lmerTest for p-values
# Using subject-level random intercept to account for within-subject correlation
mod_threshold <- lmerTest::lmer(
  threshold ~ effort_factor * task_factor + (1 | sub),
  data = pf_analysis,
  REML = FALSE
)

# Summary
cat("Model summary:\n")
print(summary(mod_threshold))

# Extract fixed effects with p-values from lmerTest
fe_threshold <- broom.mixed::tidy(mod_threshold, effects = "fixed")
# lmerTest should include p.value, but if not, extract from summary
if (!"p.value" %in% names(fe_threshold)) {
  coef_table <- coef(summary(mod_threshold))
  if ("Pr(>|t|)" %in% colnames(coef_table)) {
    # Match by term name
    fe_threshold$p.value <- coef_table[fe_threshold$term, "Pr(>|t|)"]
  }
}
cat("\nFixed effects:\n")
print(fe_threshold)

# Save model
saveRDS(mod_threshold, file.path(models_dir, "mod_pf_threshold_effort.rds"))
cat("\n✓ Saved model: mod_pf_threshold_effort.rds\n")

# Save fixed effects table
write_csv(fe_threshold, file.path(tables_dir, "pf_threshold_effort_effects.csv"))
cat("✓ Saved effects table: pf_threshold_effort_effects.csv\n")

# Extract key effect: High vs Low effort (main effect)
effort_effect_threshold <- fe_threshold %>%
  filter(term == "effort_factorHigh")

if (nrow(effort_effect_threshold) > 0) {
  cat("\nHigh vs Low effort effect on Threshold:\n")
  cat(sprintf("  Estimate: %.4f\n", effort_effect_threshold$estimate))
  cat(sprintf("  SE: %.4f\n", effort_effect_threshold$std.error))
  cat(sprintf("  t-value: %.3f\n", effort_effect_threshold$statistic))
  if ("p.value" %in% names(effort_effect_threshold)) {
    cat(sprintf("  p-value: %.4f\n", effort_effect_threshold$p.value))
    if (effort_effect_threshold$p.value < 0.05) {
      cat("  ✓ SIGNIFICANT: High effort increases thresholds\n")
    } else {
      cat("  × Non-significant: No evidence for effort effect on thresholds\n")
    }
  }
}

# ============================================================================
# MODEL 2: Slope ~ Effort
# ============================================================================

cat("\n=== Model 2: Slope ~ Effort + Task ===\n")
cat("Hypothesis H1b: Slopes shallower (lower) under High effort\n\n")

# Fit mixed-effects model using lmerTest for p-values
mod_slope <- lmerTest::lmer(
  slope ~ effort_factor * task_factor + (1 | sub),
  data = pf_analysis,
  REML = FALSE
)

# Summary
cat("Model summary:\n")
print(summary(mod_slope))

# Extract fixed effects with p-values from lmerTest
fe_slope <- broom.mixed::tidy(mod_slope, effects = "fixed")
# lmerTest should include p.value, but if not, extract from summary
if (!"p.value" %in% names(fe_slope)) {
  coef_table <- coef(summary(mod_slope))
  if ("Pr(>|t|)" %in% colnames(coef_table)) {
    # Match by term name
    fe_slope$p.value <- coef_table[fe_slope$term, "Pr(>|t|)"]
  }
}
cat("\nFixed effects:\n")
print(fe_slope)

# Save model
saveRDS(mod_slope, file.path(models_dir, "mod_pf_slope_effort.rds"))
cat("\n✓ Saved model: mod_pf_slope_effort.rds\n")

# Save fixed effects table
write_csv(fe_slope, file.path(tables_dir, "pf_slope_effort_effects.csv"))
cat("✓ Saved effects table: pf_slope_effort_effects.csv\n")

# Extract key effect: High vs Low effort (main effect)
effort_effect_slope <- fe_slope %>%
  filter(term == "effort_factorHigh")

if (nrow(effort_effect_slope) > 0) {
  cat("\nHigh vs Low effort effect on Slope:\n")
  cat(sprintf("  Estimate: %.4f\n", effort_effect_slope$estimate))
  cat(sprintf("  SE: %.4f\n", effort_effect_slope$std.error))
  cat(sprintf("  t-value: %.3f\n", effort_effect_slope$statistic))
  if ("p.value" %in% names(effort_effect_slope)) {
    cat(sprintf("  p-value: %.4f\n", effort_effect_slope$p.value))
    if (effort_effect_slope$p.value < 0.05) {
      if (effort_effect_slope$estimate < 0) {
        cat("  ✓ SIGNIFICANT: High effort decreases slopes (shallower, as predicted)\n")
      } else {
        cat("  ✓ SIGNIFICANT: High effort increases slopes (steeper, opposite of prediction)\n")
      }
    } else {
      cat("  × Non-significant: No evidence for effort effect on slopes\n")
    }
  }
}

# ============================================================================
# SUMMARY STATISTICS BY TASK AND EFFORT
# ============================================================================

cat("\n=== Summary Statistics: PF Parameters by Task and Effort ===\n")

pf_summary <- pf_analysis %>%
  group_by(task_factor, effort_factor) %>%
  summarise(
    n = n(),
    n_subjects = n_distinct(sub),
    threshold_mean = mean(threshold, na.rm = TRUE),
    threshold_sd = sd(threshold, na.rm = TRUE),
    threshold_se = threshold_sd / sqrt(n),
    threshold_median = median(threshold, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    slope_sd = sd(slope, na.rm = TRUE),
    slope_se = slope_sd / sqrt(n),
    slope_median = median(slope, na.rm = TRUE),
    .groups = "drop"
  )

print(pf_summary)

# Save summary
write_csv(pf_summary, file.path(tables_dir, "pf_parameters_summary_by_effort.csv"))
cat("\n✓ Saved summary: pf_parameters_summary_by_effort.csv\n")

# ============================================================================
# PAIRED COMPARISONS (for visualization)
# ============================================================================

cat("\n=== Computing paired differences (High - Low) ===\n")

# Compute change scores for subjects with both High and Low effort data
pf_changes <- pf_analysis %>%
  select(sub, task_factor, effort_factor, threshold, slope) %>%
  pivot_wider(
    names_from = effort_factor,
    values_from = c(threshold, slope),
    names_sep = "_"
  ) %>%
  filter(!is.na(threshold_High) & !is.na(threshold_Low) &
         !is.na(slope_High) & !is.na(slope_Low)) %>%
  mutate(
    delta_threshold = threshold_High - threshold_Low,
    delta_slope = slope_High - slope_Low
  )

cat("Subjects with paired data (both High and Low):\n")
pf_changes %>%
  group_by(task_factor) %>%
  summarise(
    n = n(),
    delta_threshold_mean = mean(delta_threshold, na.rm = TRUE),
    delta_threshold_sd = sd(delta_threshold, na.rm = TRUE),
    delta_slope_mean = mean(delta_slope, na.rm = TRUE),
    delta_slope_sd = sd(delta_slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Save change scores
write_csv(pf_changes, file.path(tables_dir, "pf_parameters_changes.csv"))
cat("\n✓ Saved change scores: pf_parameters_changes.csv\n")

cat("\n=== PF parameter effort comparison complete ===\n")

