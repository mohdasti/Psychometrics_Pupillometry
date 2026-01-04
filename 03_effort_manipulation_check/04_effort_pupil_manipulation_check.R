# Script 4: Effort-Pupil Manipulation Check
# ==========================================
# Purpose: Test whether High effort increases pupil metrics relative to Low effort
#          This provides a physiological manipulation check
# Input: ch2_triallevel_merged.csv (with quality tiers)
# Output: Model results, tables, and figures
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

# Use primary quality tier (≥0.60)
dat_analysis <- dat %>%
  filter(quality_primary == TRUE) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

cat("Using primary quality tier: ", nrow(dat_analysis), " trials\n")
cat("Subjects: ", length(unique(dat_analysis$sub)), "\n")

# ============================================================================
# MODEL 1: Total AUC ~ Effort
# ============================================================================

cat("\n=== Model 1: Total AUC ~ Effort + Task ===\n")

# Check if total_auc exists
if (!"total_auc" %in% names(dat_analysis)) {
  warning("total_auc not found, skipping Model 1")
} else {
  # Fit mixed-effects model
  mod_total_auc <- lmer(
    total_auc ~ effort_factor * task_factor + (1 | sub),
    data = dat_analysis,
    REML = FALSE
  )
  
  # Summary
  cat("\nModel summary:\n")
  print(summary(mod_total_auc))
  
  # Extract fixed effects
  fe_total_auc <- broom.mixed::tidy(mod_total_auc, effects = "fixed")
  cat("\nFixed effects:\n")
  print(fe_total_auc)
  
  # Save model
  saveRDS(mod_total_auc, file.path(models_dir, "mod_effort_total_auc.rds"))
  
  # Save fixed effects table
  write_csv(fe_total_auc, file.path(tables_dir, "effort_total_auc_effects.csv"))
  
  # Effect size: High vs Low effort (main effect)
  effort_effect_total <- fe_total_auc %>%
    filter(term == "effort_factorHigh") %>%
    pull(estimate)
  
  cat("\nHigh vs Low effort effect (Total AUC): ", round(effort_effect_total, 3), "\n")
}

# ============================================================================
# MODEL 2: Cognitive Pupil Metric ~ Effort
# ============================================================================

cat("\n=== Model 2: Cognitive Pupil (cog_auc) ~ Effort + Task ===\n")

# Check if cog_auc exists
if (!"cog_auc" %in% names(dat_analysis)) {
  warning("cog_auc not found, cannot fit Model 2")
} else {
  # Fit mixed-effects model
  mod_cog_auc <- lmer(
    cog_auc ~ effort_factor * task_factor + (1 | sub),
    data = dat_analysis,
    REML = FALSE
  )
  
  # Summary
  cat("\nModel summary:\n")
  print(summary(mod_cog_auc))
  
  # Extract fixed effects
  fe_cog_auc <- broom.mixed::tidy(mod_cog_auc, effects = "fixed")
  cat("\nFixed effects:\n")
  print(fe_cog_auc)
  
  # Save model
  saveRDS(mod_cog_auc, file.path(models_dir, "mod_effort_cog_auc.rds"))
  
  # Save fixed effects table
  write_csv(fe_cog_auc, file.path(tables_dir, "effort_cog_auc_effects.csv"))
  
  # Effect size: High vs Low effort (main effect)
  effort_effect_cog <- fe_cog_auc %>%
    filter(term == "effort_factorHigh") %>%
    pull(estimate)
  
  cat("\nHigh vs Low effort effect (Cognitive AUC): ", round(effort_effect_cog, 3), "\n")
}

# ============================================================================
# CREATE FIGURES
# ============================================================================

cat("\n=== Creating figures ===\n")

# Figure 1: Total AUC by Effort (boxplot)
if ("total_auc" %in% names(dat_analysis)) {
  # Filter out non-finite values for plotting
  dat_plot_total <- dat_analysis %>%
    filter(is.finite(total_auc), !is.na(total_auc))
  
  n_removed_total <- nrow(dat_analysis) - nrow(dat_plot_total)
  if (n_removed_total > 0) {
    cat("  Note: Removed", n_removed_total, "rows with non-finite total_auc values for plotting\n")
  }
  
  p1 <- dat_plot_total %>%
    ggplot(aes(x = effort_factor, y = total_auc, fill = effort_factor)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    facet_wrap(~ task_factor) +
    scale_fill_manual(values = c("Low" = "lightblue", "High" = "darkred")) +
    labs(
      x = "Effort Condition",
      y = "Total AUC (baseline-corrected)",
      title = "Effort-Pupil Manipulation Check: Total AUC",
      fill = "Effort"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(figures_dir, "effort_manipulation_total_auc.png"),
         p1, width = 8, height = 6, dpi = 300)
  cat("✓ Saved: effort_manipulation_total_auc.png\n")
}

# Figure 2: Cognitive AUC by Effort (boxplot)
if ("cog_auc" %in% names(dat_analysis)) {
  # Filter out non-finite values for plotting
  dat_plot_cog <- dat_analysis %>%
    filter(is.finite(cog_auc), !is.na(cog_auc))
  
  n_removed_cog <- nrow(dat_analysis) - nrow(dat_plot_cog)
  if (n_removed_cog > 0) {
    cat("  Note: Removed", n_removed_cog, "rows with non-finite cog_auc values for plotting\n")
  }
  
  p2 <- dat_plot_cog %>%
    ggplot(aes(x = effort_factor, y = cog_auc, fill = effort_factor)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    facet_wrap(~ task_factor) +
    scale_fill_manual(values = c("Low" = "lightblue", "High" = "darkred")) +
    labs(
      x = "Effort Condition",
      y = "Cognitive AUC (fixed post-target window)",
      title = "Effort-Pupil Manipulation Check: Cognitive Pupil",
      fill = "Effort"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(figures_dir, "effort_manipulation_cog_auc.png"),
         p2, width = 8, height = 6, dpi = 300)
  cat("✓ Saved: effort_manipulation_cog_auc.png\n")
}

# Figure 3: Subject-level scatter (High vs Low effort)
if ("total_auc" %in% names(dat_analysis) && "cog_auc" %in% names(dat_analysis)) {
  subject_means <- dat_analysis %>%
    filter(is.finite(cog_auc), !is.na(cog_auc)) %>%
    group_by(sub, task_factor, effort_factor) %>%
    summarise(
      mean_total_auc = mean(total_auc, na.rm = TRUE),
      mean_cog_auc = mean(cog_auc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = effort_factor,
      values_from = c(mean_total_auc, mean_cog_auc),
      names_sep = "_"
    )
  
  if ("mean_cog_auc_High" %in% names(subject_means) && "mean_cog_auc_Low" %in% names(subject_means)) {
    # Filter out any remaining non-finite values
    subject_means_plot <- subject_means %>%
      filter(is.finite(mean_cog_auc_Low), is.finite(mean_cog_auc_High),
             !is.na(mean_cog_auc_Low), !is.na(mean_cog_auc_High))
    
    n_removed_scatter <- nrow(subject_means) - nrow(subject_means_plot)
    if (n_removed_scatter > 0) {
      cat("  Note: Removed", n_removed_scatter, "subjects with non-finite values for scatter plot\n")
    }
    
    p3 <- subject_means_plot %>%
      ggplot(aes(x = mean_cog_auc_Low, y = mean_cog_auc_High)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      facet_wrap(~ task_factor) +
      labs(
        x = "Mean Cognitive AUC (Low Effort)",
        y = "Mean Cognitive AUC (High Effort)",
        title = "Subject-Level Effort Effect: Cognitive Pupil"
      ) +
      theme_minimal()
    
    ggsave(file.path(figures_dir, "effort_manipulation_subject_scatter.png"),
           p3, width = 8, height = 6, dpi = 300)
    cat("✓ Saved: effort_manipulation_subject_scatter.png\n")
  }
}

cat("\n=== Effort-pupil manipulation check complete ===\n")

