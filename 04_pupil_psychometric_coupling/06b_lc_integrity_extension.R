# Script 6b: LC Integrity Extension (EXPLORATORY)
# =================================================
# Purpose: Extend primary GLMM to include LC integrity as moderator
#          Test whether LC structural integrity moderates pupil-psychometric coupling
# Input: ch2_triallevel_merged.csv, ch2_lc_integrity.csv, ch2_demographics.csv
# Output: Model results, LC subset comparison table
#
# Note: These analyses are EXPLORATORY and SECONDARY to the primary pupil-psychometric
#       coupling analyses specified in the prospectus. LC integrity data were not
#       part of the original prospectus and are available for only a subset of participants.
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

# Load LC integrity data
cat("Loading LC integrity data...\n")
lc_file <- file.path(processed_dir, "ch2_lc_integrity.csv")
if (!file.exists(lc_file)) {
  warning("LC integrity file not found. Skipping LC analyses.")
  lc_data <- NULL
} else {
  lc_data <- read_csv(lc_file, show_col_types = FALSE)
  cat("  Loaded LC data for", length(unique(lc_data$sub)), "subjects\n")
}

# Load demographics
cat("Loading demographics data...\n")
demo_file <- file.path(processed_dir, "ch2_demographics.csv")
if (!file.exists(demo_file)) {
  warning("Demographics file not found.")
  demographics <- NULL
} else {
  demographics <- read_csv(demo_file, show_col_types = FALSE)
  cat("  Loaded demographics for", length(unique(demographics$sub)), "subjects\n")
}

# ============================================================================
# LC SUBSET BIAS CHECK
# ============================================================================

cat("\n=== LC Subset Bias Check ===\n")

if (!is.null(lc_data) && !is.null(demographics)) {
  subjects_with_lc <- unique(lc_data$sub)
  
  # Merge demographics with trial data to get behavioral outcomes
  dat_with_demo <- dat %>%
    left_join(demographics %>% select(sub, age, sex, education), by = "sub") %>%
    mutate(has_lc_data = sub %in% subjects_with_lc)
  
  # Compute subject-level summaries
  subject_summary <- dat_with_demo %>%
    filter(quality_primary == TRUE) %>%
    group_by(sub) %>%
    summarise(
      has_lc = first(has_lc_data),
      age = first(age),
      sex = first(sex),
      education = first(education),
      mean_accuracy = mean(correct_final, na.rm = TRUE),
      mean_rt = mean(rt, na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(has_lc))
  
  cat("  Subjects with LC data:", sum(subject_summary$has_lc), "\n")
  cat("  Subjects without LC data:", sum(!subject_summary$has_lc), "\n")
  
  # Statistical comparisons
  comparison_results <- list()
  
  # Age
  if (sum(!is.na(subject_summary$age)) > 0) {
    age_test <- t.test(age ~ has_lc, data = subject_summary)
    comparison_results$age <- tibble(
      Variable = "Age (years)",
      `With LC Data` = sprintf("%.1f (%.1f)", 
                              mean(subject_summary$age[subject_summary$has_lc], na.rm = TRUE),
                              sd(subject_summary$age[subject_summary$has_lc], na.rm = TRUE)),
      `Without LC Data` = sprintf("%.1f (%.1f)", 
                                  mean(subject_summary$age[!subject_summary$has_lc], na.rm = TRUE),
                                  sd(subject_summary$age[!subject_summary$has_lc], na.rm = TRUE)),
      `Test Statistic` = sprintf("t = %.2f", age_test$statistic),
      p = age_test$p.value
    )
    cat("  Age comparison: t =", sprintf("%.2f", age_test$statistic), 
        ", p =", sprintf("%.4f", age_test$p.value), "\n")
  }
  
  # Sex
  if (sum(!is.na(subject_summary$sex)) > 0) {
    sex_table <- table(subject_summary$has_lc, subject_summary$sex)
    if (ncol(sex_table) > 0 && nrow(sex_table) > 0) {
      sex_test <- chisq.test(sex_table)
      comparison_results$sex <- tibble(
        Variable = "Sex (M/F)",
        `With LC Data` = paste(sex_table[2,], collapse = "/"),
        `Without LC Data` = paste(sex_table[1,], collapse = "/"),
        `Test Statistic` = sprintf("χ² = %.2f", sex_test$statistic),
        p = sex_test$p.value
      )
      cat("  Sex comparison: χ² =", sprintf("%.2f", sex_test$statistic), 
          ", p =", sprintf("%.4f", sex_test$p.value), "\n")
    }
  }
  
  # Education
  if (sum(!is.na(subject_summary$education)) > 0) {
    edu_test <- t.test(education ~ has_lc, data = subject_summary)
    comparison_results$education <- tibble(
      Variable = "Education (years)",
      `With LC Data` = sprintf("%.1f (%.1f)", 
                               mean(subject_summary$education[subject_summary$has_lc], na.rm = TRUE),
                               sd(subject_summary$education[subject_summary$has_lc], na.rm = TRUE)),
      `Without LC Data` = sprintf("%.1f (%.1f)", 
                                  mean(subject_summary$education[!subject_summary$has_lc], na.rm = TRUE),
                                  sd(subject_summary$education[!subject_summary$has_lc], na.rm = TRUE)),
      `Test Statistic` = sprintf("t = %.2f", edu_test$statistic),
      p = edu_test$p.value
    )
    cat("  Education comparison: t =", sprintf("%.2f", edu_test$statistic), 
        ", p =", sprintf("%.4f", edu_test$p.value), "\n")
  }
  
  # Behavioral outcomes
  if (sum(!is.na(subject_summary$mean_accuracy)) > 0) {
    acc_test <- t.test(mean_accuracy ~ has_lc, data = subject_summary)
    comparison_results$accuracy <- tibble(
      Variable = "Mean Accuracy",
      `With LC Data` = sprintf("%.3f (%.3f)", 
                               mean(subject_summary$mean_accuracy[subject_summary$has_lc], na.rm = TRUE),
                               sd(subject_summary$mean_accuracy[subject_summary$has_lc], na.rm = TRUE)),
      `Without LC Data` = sprintf("%.3f (%.3f)", 
                                  mean(subject_summary$mean_accuracy[!subject_summary$has_lc], na.rm = TRUE),
                                  sd(subject_summary$mean_accuracy[!subject_summary$has_lc], na.rm = TRUE)),
      `Test Statistic` = sprintf("t = %.2f", acc_test$statistic),
      p = acc_test$p.value
    )
  }
  
  # Save comparison results
  if (length(comparison_results) > 0) {
    lc_subset_comparison <- bind_rows(comparison_results)
    write_csv(lc_subset_comparison, file.path(tables_dir, "lc_subset_bias_check.csv"))
    cat("✓ Saved: lc_subset_bias_check.csv\n")
  }
} else {
  cat("  LC data or demographics not available. Skipping subset check.\n")
}

# ============================================================================
# PREPARE DATA FOR LC EXTENSION
# ============================================================================

if (is.null(lc_data)) {
  cat("\n=== Skipping LC extension analyses (LC data not available) ===\n")
} else {
  cat("\n=== Preparing data for LC integrity extension ===\n")
  
  # Pre-specify primary LC metric: mean-within-mask MTC (more robust than maximum-voxel)
  # Based on Bennett et al. (2024) recommendations
  if ("mtc_lc_pons_mean" %in% names(lc_data)) {
    lc_primary_metric <- "mtc_lc_pons_mean"
    cat("  Using primary LC metric: mean-within-mask MTC (mtc_lc_pons_mean)\n")
  } else if ("lc_mtc_mean" %in% names(lc_data)) {
    lc_primary_metric <- "lc_mtc_mean"
    cat("  Using primary LC metric: mean-within-mask MTC (lc_mtc_mean)\n")
  } else {
    # Try to find any MTC mean metric
    mtc_cols <- grep("mtc.*mean|mean.*mtc", names(lc_data), value = TRUE, ignore.case = TRUE)
    if (length(mtc_cols) > 0) {
      lc_primary_metric <- mtc_cols[1]
      cat("  Using LC metric:", lc_primary_metric, "\n")
    } else {
      warning("Could not find appropriate LC metric. Skipping LC extension.")
      lc_primary_metric <- NULL
    }
  }
  
  if (!is.null(lc_primary_metric)) {
    # Merge LC data with trial-level data
    dat_lc <- dat %>%
      filter(quality_primary == TRUE) %>%
      filter(!is.na(pupil_cognitive_state), !is.na(stimulus_intensity), !is.na(choice_num)) %>%
      left_join(lc_data %>% select(sub, !!sym(lc_primary_metric)), by = "sub") %>%
      left_join(demographics %>% select(sub, age, sex, education), by = "sub") %>%
      filter(!is.na(!!sym(lc_primary_metric))) %>%  # Keep only subjects with LC data
      mutate(
        effort_factor = factor(effort, levels = c("Low", "High")),
        task_factor = factor(task),
        lc_integrity = !!sym(lc_primary_metric)
      )
    
    cat("  LC extension dataset: ", nrow(dat_lc), " trials\n")
    cat("  Subjects with LC data: ", length(unique(dat_lc$sub)), "\n")
    
    # Standardize variables
    dat_lc <- dat_lc %>%
      group_by(task_factor) %>%
      mutate(
        stimulus_intensity_scaled = scale(stimulus_intensity)[,1]
      ) %>%
      ungroup() %>%
      mutate(
        pupil_cognitive_state_scaled = scale(pupil_cognitive_state)[,1],
        pupil_cognitive_trait_scaled = scale(pupil_cognitive_trait)[,1],
        lc_integrity_scaled = scale(lc_integrity)[,1],
        age_scaled = scale(age)[,1],
        education_scaled = scale(education)[,1]
      )
    
    # Ensure choice_num is binary
    if (!"choice_num" %in% names(dat_lc)) {
      if ("choice" %in% names(dat_lc)) {
        dat_lc$choice_num <- ifelse(dat_lc$choice == "DIFFERENT" | 
                                     dat_lc$choice == 1 | 
                                     dat_lc$choice == TRUE, 1, 0)
      } else {
        stop("Cannot find choice variable for GLMM")
      }
    }
    
    # ============================================================================
    # LC EXTENSION MODEL: Three-way interaction
    # ============================================================================
    
    cat("\n=== Fitting LC extension GLMM ===\n")
    cat("Model: choice ~ stimulus_intensity * pupil_state * LC_integrity + \n")
    cat("       effort + task + pupil_trait + age + sex + education + \n")
    cat("       (1 + stimulus_intensity | sub)\n")
    
    # Fit GLMM with LC integrity as moderator
    mod_lc_extension <- glmer(
      choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled * lc_integrity_scaled +
        effort_factor + task_factor +
        pupil_cognitive_trait_scaled +
        age_scaled + sex + education_scaled +
        (1 + stimulus_intensity_scaled | sub),
      data = dat_lc,
      family = binomial(link = "probit"),
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
    )
    
    cat("\nModel summary:\n")
    print(summary(mod_lc_extension))
    
    # Extract fixed effects
    fe_lc <- broom.mixed::tidy(mod_lc_extension, effects = "fixed")
    cat("\nFixed effects:\n")
    print(fe_lc)
    
    # Save model
    saveRDS(mod_lc_extension, file.path(models_dir, "mod_lc_psychometric_extension.rds"))
    
    # Save fixed effects table
    write_csv(fe_lc, file.path(tables_dir, "lc_psychometric_extension_effects.csv"))
    cat("✓ Saved: lc_psychometric_extension_effects.csv\n")
    
    # ============================================================================
    # KEY INTERACTION TEST
    # ============================================================================
    
    cat("\n=== Testing three-way interaction ===\n")
    
    three_way_term <- fe_lc %>%
      filter(term == "stimulus_intensity_scaled:pupil_cognitive_state_scaled:lc_integrity_scaled")
    
    if (nrow(three_way_term) > 0) {
      cat("\nThree-way interaction effect:\n")
      cat(sprintf("  Estimate: %.4f\n", three_way_term$estimate))
      cat(sprintf("  SE: %.4f\n", three_way_term$std.error))
      cat(sprintf("  95%% CI: [%.4f, %.4f]\n", 
                  three_way_term$estimate - 1.96 * three_way_term$std.error,
                  three_way_term$estimate + 1.96 * three_way_term$std.error))
      cat(sprintf("  z-value: %.3f\n", three_way_term$statistic))
      cat(sprintf("  p-value: %.4f\n", three_way_term$p.value))
      
      if (three_way_term$p.value < 0.05) {
        cat("\n  ✓ SIGNIFICANT: LC integrity moderates pupil-psychometric coupling\n")
      } else {
        cat("\n  × Non-significant: No evidence for LC moderation\n")
      }
    }
    
    # Also test two-way interaction (stimulus × pupil state) in LC subset
    two_way_term <- fe_lc %>%
      filter(term == "stimulus_intensity_scaled:pupil_cognitive_state_scaled")
    
    if (nrow(two_way_term) > 0) {
      cat("\nTwo-way interaction (stimulus × pupil state) in LC subset:\n")
      cat(sprintf("  Estimate: %.4f (p = %.4f)\n", 
                  two_way_term$estimate, two_way_term$p.value))
    }
    
    cat("\n=== LC extension analyses complete ===\n")
    cat("Note: These analyses are exploratory and should be interpreted with caution\n")
    cat("      given that LC data are available for only a subset of participants.\n")
  }
}

cat("\n=== Script 6b complete ===\n")

