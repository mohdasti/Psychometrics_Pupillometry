# Script 2: Compute Psychometric Function (PF) Parameters
# ========================================================
# Purpose: Fit psychometric functions or load existing PF fits
#          Extract thresholds and slopes per subject × task × effort
# Input: ch2_triallevel_merged.csv
# Output: ch2_pf_parameters.csv (subject-level PF parameters)
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(lme4)
library(here)

# Set paths
data_dir <- here("07_manuscript", "chapter2", "data")
processed_dir <- file.path(data_dir, "processed")

# Load data
cat("Loading trial-level data...\n")
dat_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")
dat <- read_csv(dat_file, show_col_types = FALSE)

# ============================================================================
# OPTION 1: Load existing PF parameters if available
# ============================================================================
# Check if PF parameters already exist in processed directory
pf_file <- file.path(processed_dir, "ch2_pf_parameters.csv")
if (file.exists(pf_file)) {
  cat("Loading existing PF parameters from processed directory...\n")
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
  cat("✓ Loaded existing PF parameters\n")
  cat("  Rows:", nrow(pf_params), "\n")
  cat("  Subjects:", length(unique(pf_params$sub)), "\n")
  quit()
}

# Alternative: Load from master behavioral spreadsheet
# The master spreadsheet contains PF parameters by subject × task × effort
# File: "LC Aging Subject Data master spreadsheet - behavioral.csv"
# Columns include: aud_thresh, vis_thresh, aud_slope, vis_slope (with Low/High variants)
# If these represent the "existing behavioral manuscript" PF fits, they could be
# extracted and reformatted here instead of re-computing
master_beh_file <- file.path(raw_dir, "behavioral", 
                             "LC Aging Subject Data master spreadsheet - behavioral.csv")
if (file.exists(master_beh_file)) {
  cat("\nNote: Master behavioral spreadsheet found at:", master_beh_file, "\n")
  cat("  This file contains PF parameters (thresholds, slopes) by subject × task × effort\n")
  cat("  Consider extracting these if they represent the existing behavioral manuscript fits\n")
  cat("  Columns: aud_thresh_low, aud_thresh_high, vis_thresh_low, vis_thresh_high, etc.\n")
}

# ============================================================================
# OPTION 2: Fit psychometric functions
# ============================================================================

cat("\n=== Fitting Psychometric Functions ===\n")
cat("Using probit link function (natural for psychometric modeling)\n\n")

# Prepare data for PF fitting
# Ensure we have binary choice outcome (0/1 or FALSE/TRUE)
# 'different' = 1, 'same' = 0

if (!"choice_num" %in% names(dat)) {
  # Create binary choice if not already present
  if ("choice" %in% names(dat)) {
    dat$choice_num <- ifelse(dat$choice == "DIFFERENT" | dat$choice == 1 | dat$choice == TRUE, 1, 0)
  } else if ("correct_final" %in% names(dat)) {
    # Use correct_final if choice not available
    dat$choice_num <- ifelse(dat$correct_final == 1, 1, 0)
  } else {
    stop("Cannot find choice variable. Need 'choice' or 'choice_num' or 'correct_final'")
  }
}

# Standardize effort levels
dat$effort_factor <- factor(dat$effort, levels = c("Low", "High"))
dat$task_factor <- factor(dat$task)

# Fit PF for each subject × task × effort combination
pf_results <- dat %>%
  group_by(sub, task_factor, effort_factor) %>%
  filter(!is.na(stimulus_intensity), !is.na(choice_num)) %>%
  filter(n() >= 20) %>%  # Need minimum trials for stable fit
  do({
    df <- .
    
    # Fit probit GLM (simple version, can extend to mixed-effects if needed)
    tryCatch({
      # Simple probit model: choice ~ stimulus_intensity
      mod <- glm(choice_num ~ stimulus_intensity, 
                 family = binomial(link = "probit"), 
                 data = df)
      
      # Extract parameters
      # Slope is the coefficient on stimulus_intensity
      slope <- coef(mod)["stimulus_intensity"]
      
      # Threshold (PSE/JND) is -intercept/slope (point of 50% performance)
      intercept <- coef(mod)["(Intercept)"]
      threshold <- -intercept / slope
      
      # Compute fit quality metrics
      pred <- predict(mod, type = "response")
      deviance_resid <- residuals(mod, type = "deviance")
      r_squared <- 1 - (mod$deviance / mod$null.deviance)
      
      tibble(
        threshold = as.numeric(threshold),
        slope = as.numeric(slope),
        intercept = as.numeric(intercept),
        r_squared = r_squared,
        n_trials = nrow(df),
        converged = mod$converged,
        log_likelihood = logLik(mod)[1]
      )
    }, error = function(e) {
      tibble(
        threshold = NA_real_,
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = NA_real_,
        n_trials = nrow(df),
        converged = FALSE,
        log_likelihood = NA_real_
      )
    })
  }) %>%
  ungroup() %>%
  rename(task = task_factor, effort = effort_factor)

# ============================================================================
# ALTERNATIVE: Hierarchical mixed-effects PF fitting
# ============================================================================
# For more robust estimation, can use glmer with subject-level random effects:
#
# pf_mod <- glmer(choice_num ~ stimulus_intensity * effort * task_factor + 
#                 (1 + stimulus_intensity | sub),
#                 family = binomial(link = "probit"), data = dat)
#
# Then extract subject-specific coefficients using coef() or ranef()

# ============================================================================
# SAVE PF PARAMETERS
# ============================================================================

cat("\n=== Saving PF Parameters ===\n")
pf_output <- file.path(processed_dir, "ch2_pf_parameters.csv")
write_csv(pf_results, pf_output)
cat("✓ Saved PF parameters to:", pf_output, "\n")
cat("  Subjects:", length(unique(pf_results$sub)), "\n")
cat("  Total fits:", nrow(pf_results), "\n")
cat("  Successful fits:", sum(pf_results$converged, na.rm = TRUE), "\n")
cat("  Failed fits:", sum(!pf_results$converged, na.rm = TRUE), "\n")

# Summary statistics
cat("\n=== PF Parameter Summary ===\n")
pf_summary <- pf_results %>%
  filter(converged) %>%
  group_by(task, effort) %>%
  summarise(
    n = n(),
    threshold_mean = mean(threshold, na.rm = TRUE),
    threshold_sd = sd(threshold, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    slope_sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  )
print(pf_summary)

cat("\n=== PF computation complete ===\n")

