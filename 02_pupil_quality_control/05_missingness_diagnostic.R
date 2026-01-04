# Script 5: Missingness Diagnostic
# ==================================
# Purpose: Model pupil data missingness as outcome to test for systematic bias
#          If retention is predicted by task variables, document and plan robustness checks
# Input: ch2_triallevel_merged.csv
# Output: Missingness model results and diagnostic table
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(lme4)
library(broom.mixed)
library(here)

# Load path configuration
source(file.path(here(), "config", "paths_config.R"))

# Load data
cat("Loading trial-level data...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

# ============================================================================
# CREATE MISSINGNESS INDICATOR
# ============================================================================

cat("\n=== Creating missingness indicator ===\n")

# Define pupil usability based on primary quality tier
# Usable = primary quality tier passes (baseline ≥ 0.60 AND cognitive ≥ 0.60)
dat$pupil_usable <- dat$quality_primary == TRUE

# Check missingness pattern
cat("Pupil data availability:\n")
missing_table <- dat %>%
  group_by(task, effort) %>%
  summarise(
    n_trials = n(),
    n_usable = sum(pupil_usable, na.rm = TRUE),
    n_missing = sum(!pupil_usable, na.rm = TRUE),
    prop_usable = mean(pupil_usable, na.rm = TRUE),
    .groups = "drop"
  )
print(missing_table)

# ============================================================================
# FIT MISSINGNESS MODEL
# ============================================================================

cat("\n=== Fitting missingness model ===\n")
cat("Model: pupil_usable ~ effort + stimulus_intensity + task + rt + (1|sub)\n")

# Prepare data
dat_missing <- dat %>%
  filter(!is.na(pupil_usable)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

# Standardize continuous predictors for interpretability
dat_missing <- dat_missing %>%
  mutate(
    stimulus_intensity_scaled = scale(stimulus_intensity)[,1],
    rt_scaled = scale(rt)[,1]
  )

# Fit logistic mixed-effects model
mod_missing <- glmer(
  pupil_usable ~ effort_factor + stimulus_intensity_scaled + task_factor + rt_scaled +
    (1 | sub),
  data = dat_missing,
  family = binomial(link = "logit")
)

# Summary
cat("\nModel summary:\n")
print(summary(mod_missing))

# Extract fixed effects with confidence intervals
fe_missing <- broom.mixed::tidy(mod_missing, effects = "fixed", 
                                 exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
cat("\nFixed effects (odds ratios):\n")
print(fe_missing)

# Save model
saveRDS(mod_missing, file.path(models_dir, "mod_missingness_diagnostic.rds"))

# Save fixed effects table
write_csv(fe_missing, file.path(tables_dir, "missingness_diagnostic_effects.csv"))

# ============================================================================
# INTERPRETATION
# ============================================================================

cat("\n=== Missingness Diagnostic Interpretation ===\n")

# Check if effort predicts missingness
effort_coef <- fe_missing %>% filter(term == "effort_factorHigh")
if (nrow(effort_coef) > 0) {
  or_effort <- effort_coef$estimate
  p_effort <- effort_coef$p.value
  
  cat("\nEffort effect on missingness:\n")
  cat(sprintf("  Odds Ratio: %.3f\n", or_effort))
  if ("conf.low" %in% names(effort_coef) && "conf.high" %in% names(effort_coef)) {
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
                effort_coef$conf.low, effort_coef$conf.high))
  }
  cat(sprintf("  p-value: %.4f\n", p_effort))
  
  if (p_effort < 0.05) {
    cat("  ⚠ WARNING: Effort significantly predicts missingness\n")
    cat("    Results should be interpreted with this bias in mind\n")
    cat("    Robustness analyses will be emphasized\n")
  } else {
    cat("  ✓ Effort does not significantly predict missingness\n")
  }
}

# Check if stimulus intensity predicts missingness
intensity_coef <- fe_missing %>% filter(term == "stimulus_intensity_scaled")
if (nrow(intensity_coef) > 0) {
  or_intensity <- intensity_coef$estimate
  p_intensity <- intensity_coef$p.value
  
  cat("\nStimulus intensity effect on missingness:\n")
  cat(sprintf("  Odds Ratio: %.3f\n", or_intensity))
  cat(sprintf("  p-value: %.4f\n", p_intensity))
  
  if (p_intensity < 0.05) {
    cat("  ⚠ WARNING: Stimulus intensity significantly predicts missingness\n")
  } else {
    cat("  ✓ Stimulus intensity does not significantly predict missingness\n")
  }
}

# Check if RT predicts missingness
rt_coef <- fe_missing %>% filter(term == "rt_scaled")
if (nrow(rt_coef) > 0) {
  or_rt <- rt_coef$estimate
  p_rt <- rt_coef$p.value
  
  cat("\nRT effect on missingness:\n")
  cat(sprintf("  Odds Ratio: %.3f\n", or_rt))
  cat(sprintf("  p-value: %.4f\n", p_rt))
  
  if (p_rt < 0.05) {
    cat("  ⚠ WARNING: RT significantly predicts missingness\n")
  } else {
    cat("  ✓ RT does not significantly predict missingness\n")
  }
}

# ============================================================================
# CREATE MISSINGNESS SUMMARY TABLE
# ============================================================================

cat("\n=== Creating missingness summary table ===\n")

missingness_summary <- dat %>%
  group_by(sub, task, effort) %>%
  summarise(
    n_trials = n(),
    n_usable = sum(pupil_usable, na.rm = TRUE),
    prop_usable = mean(pupil_usable, na.rm = TRUE),
    mean_stimulus_intensity = mean(stimulus_intensity, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    .groups = "drop"
  )

missingness_output <- file.path(qc_dir, "missingness_diagnostic.csv")
write_csv(missingness_summary, missingness_output)
cat("✓ Missingness summary saved to:", missingness_output, "\n")

# Create inclusion bias table (for reporting)
inclusion_bias <- dat %>%
  group_by(task, effort, pupil_usable) %>%
  summarise(
    n = n(),
    mean_stimulus_intensity = mean(stimulus_intensity, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = pupil_usable,
    values_from = c(n, mean_stimulus_intensity, mean_rt, sd_rt),
    names_prefix = "usable_"
  )

bias_output <- file.path(qc_dir, "inclusion_bias_table.csv")
write_csv(inclusion_bias, bias_output)
cat("✓ Inclusion bias table saved to:", bias_output, "\n")

cat("\n=== Missingness diagnostic complete ===\n")

