# Script 2: Compute Psychometric Function (PF) Parameters
# ========================================================
# Purpose: Build subject-level PF parameters for Chapter 2
# Priority:
#   1. If master behavioral spreadsheet exists — import Psignifit/MATLAB parameters
#      (wide aud_/vis_ × _low/_high) → long format (sub × ADT/VDT × Low/High).
#   2. Else if ch2_pf_parameters.csv exists — load (legacy).
#   3. Else — fit simple probit GLMs to trial-level merged data.
#
# Input:  data/raw/behavioral/LC Aging Subject Data master spreadsheet - behavioral.csv
#         (preferred), or ch2_triallevel_merged.csv (fallback fits)
# Output: data/processed/ch2_pf_parameters.csv
#
# Author: Mohammad Dastgheib

library(tidyverse)
library(lme4)
library(here)

source(file.path(here(), "config", "paths_config.R"))

pf_file <- pf_params_file
merged_file <- merged_trial_file

# ----------------------------------------------------------------------------
# Import PF parameters from master behavioral spreadsheet (canonical source)
# ----------------------------------------------------------------------------

import_pf_from_master_beh <- function(path) {
  master <- read_csv(path, skip = 1, show_col_types = FALSE, na = c("", "NA", "N/A", " "))
  subj_col <- c("SUBJECT NUMBER_1", "SUBJECT_NUMBER_1")[c("SUBJECT NUMBER_1", "SUBJECT_NUMBER_1") %in% names(master)][1]
  if (is.na(subj_col)) {
    stop("Could not find SUBJECT NUMBER_1 column in master behavioral file.")
  }
  req <- c(
    "aud_thresh_low", "aud_thresh_high", "vis_thresh_low", "vis_thresh_high",
    "aud_slope_low", "aud_slope_high", "vis_slope_low", "vis_slope_high"
  )
  missing_cols <- setdiff(req, names(master))
  if (length(missing_cols) > 0) {
    stop("Master behavioral file missing columns: ", paste(missing_cols, collapse = ", "))
  }

  th <- master %>%
    mutate(sub = str_trim(as.character(.data[[subj_col]]))) %>%
    filter(!is.na(sub), sub != "") %>%
    select(sub, aud_thresh_low, aud_thresh_high, vis_thresh_low, vis_thresh_high) %>%
    pivot_longer(-sub, names_to = "key", values_to = "threshold") %>%
    mutate(
      task = case_when(
        str_starts(key, "aud_") ~ "ADT",
        str_starts(key, "vis_") ~ "VDT",
        TRUE ~ NA_character_
      ),
      effort = case_when(
        str_ends(key, "_low") ~ factor("Low", levels = c("Low", "High")),
        str_ends(key, "_high") ~ factor("High", levels = c("Low", "High")),
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(task)) %>%
    select(sub, task, effort, threshold)

  sl <- master %>%
    mutate(sub = str_trim(as.character(.data[[subj_col]]))) %>%
    filter(!is.na(sub), sub != "") %>%
    select(sub, aud_slope_low, aud_slope_high, vis_slope_low, vis_slope_high) %>%
    pivot_longer(-sub, names_to = "key", values_to = "slope") %>%
    mutate(
      task = case_when(
        str_starts(key, "aud_") ~ "ADT",
        str_starts(key, "vis_") ~ "VDT",
        TRUE ~ NA_character_
      ),
      effort = case_when(
        str_ends(key, "_low") ~ factor("Low", levels = c("Low", "High")),
        str_ends(key, "_high") ~ factor("High", levels = c("Low", "High")),
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(task)) %>%
    select(sub, task, effort, slope)

  pf_results <- inner_join(th, sl, by = c("sub", "task", "effort")) %>%
    mutate(
      intercept = NA_real_,
      r_squared = NA_real_,
      n_trials = NA_integer_,
      converged = is.finite(threshold) & is.finite(slope),
      log_likelihood = NA_real_
    ) %>%
    arrange(sub, task, effort)

  pf_results
}

# ----------------------------------------------------------------------------
# Main logic
# ----------------------------------------------------------------------------

if (file.exists(master_beh_file)) {
  cat("Importing PF parameters from master behavioral spreadsheet:\n  ", master_beh_file, "\n", sep = "")
  pf_results <- import_pf_from_master_beh(master_beh_file)
  write_csv(pf_results, pf_file)
  cat("✓ Saved PF parameters to:", pf_file, "\n")
  cat("  Rows:", nrow(pf_results), "\n")
  cat("  Subjects:", length(unique(pf_results$sub)), "\n")
  cat("  Converged / valid rows:", sum(pf_results$converged, na.rm = TRUE), "\n")

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
  cat("\n=== PF import from master complete ===\n")

} else if (file.exists(pf_file)) {
  cat("Keeping existing PF parameters (no master spreadsheet at expected path).\n")
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
  cat("✓ Loaded:", pf_file, "\n")
  cat("  Rows:", nrow(pf_params), "\n")
  cat("  Subjects:", length(unique(pf_params$sub)), "\n")

} else {
# ----------------------------------------------------------------------------
# Fallback: fit probit GLMs to trial-level data
# ----------------------------------------------------------------------------

cat("No master spreadsheet and no existing PF file — fitting probit GLMs to trial data...\n")
cat("Loading trial-level data...\n")
dat <- read_csv(merged_file, show_col_types = FALSE)

if (!"choice_num" %in% names(dat)) {
  if ("choice" %in% names(dat)) {
    dat$choice_num <- ifelse(dat$choice == "DIFFERENT" | dat$choice == 1 | dat$choice == TRUE, 1, 0)
  } else if ("correct_final" %in% names(dat)) {
    dat$choice_num <- ifelse(dat$correct_final == 1, 1, 0)
  } else {
    stop("Cannot find choice variable. Need 'choice' or 'choice_num' or 'correct_final'")
  }
}

dat$effort_factor <- factor(dat$effort, levels = c("Low", "High"))
dat$task_factor <- factor(dat$task)

pf_results <- dat %>%
  group_by(sub, task_factor, effort_factor) %>%
  filter(!is.na(stimulus_intensity), !is.na(choice_num)) %>%
  filter(n() >= 20) %>%
  do({
    df <- .
    tryCatch({
      mod <- glm(
        choice_num ~ stimulus_intensity,
        family = binomial(link = "probit"),
        data = df
      )
      slope <- coef(mod)["stimulus_intensity"]
      intercept <- coef(mod)["(Intercept)"]
      threshold <- -intercept / slope
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

write_csv(pf_results, pf_file)
cat("✓ Saved PF parameters to:", pf_file, "\n")
cat("\n=== PF computation complete ===\n")
}
