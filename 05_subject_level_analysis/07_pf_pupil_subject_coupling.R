# Script 7: Subject-Level PF-Pupil Coupling
# ==========================================
# Purpose: Correlate subject-level changes in pupil metrics with changes in PF parameters
#          Test consistency between full behavioral dataset and pupil subset
# Input: ch2_pf_parameters.csv, ch2_triallevel_merged.csv
# Output: Correlation results, tables, and figures
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(GGally)
library(here)

# Load path configuration
source(file.path(here(), "config", "paths_config.R"))

# Load data
cat("Loading data...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

pf_file <- pf_params_file
if (!file.exists(pf_file)) {
  warning("PF parameters file not found. Run script 02_compute_pf_parameters.R first.")
  # Create empty structure for now
  pf_params <- NULL
} else {
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
  cat("Loaded PF parameters for", length(unique(pf_params$sub)), "subjects\n")
}

# ============================================================================
# COMPUTE SUBJECT-LEVEL CHANGES IN PUPIL METRICS
# ============================================================================

cat("\n=== Computing subject-level pupil changes (High - Low effort) ===\n")

# Compute mean pupil metrics by subject × task × effort
subject_pupil <- dat %>%
  filter(quality_primary == TRUE) %>%
  group_by(sub, task, effort) %>%
  summarise(
    mean_cog_auc = mean(cog_auc, na.rm = TRUE),
    mean_total_auc = mean(total_auc, na.rm = TRUE),
    .groups = "drop"
  )

# Compute change scores (High - Low)
pupil_changes <- subject_pupil %>%
  pivot_wider(
    names_from = effort,
    values_from = c(mean_cog_auc, mean_total_auc),
    names_sep = "_"
  ) %>%
  mutate(
    delta_cog_auc = mean_cog_auc_High - mean_cog_auc_Low,
    delta_total_auc = mean_total_auc_High - mean_total_auc_Low
  ) %>%
  select(sub, task, delta_cog_auc, delta_total_auc)

cat("Computed pupil change scores for", nrow(pupil_changes), "subject × task combinations\n")

# ============================================================================
# COMPUTE SUBJECT-LEVEL CHANGES IN PF PARAMETERS
# ============================================================================

if (!is.null(pf_params) && nrow(pf_params) > 0) {
  cat("\n=== Computing subject-level PF parameter changes (High - Low effort) ===\n")
  
  # Compute change scores for PF parameters
  # First select only needed columns, then pivot
  pf_changes <- pf_params %>%
    filter(converged) %>%
    select(sub, task, effort, threshold, slope) %>%
    pivot_wider(
      names_from = effort,
      values_from = c(threshold, slope),
      names_sep = "_"
    ) %>%
    mutate(
      delta_threshold = threshold_High - threshold_Low,
      delta_slope = slope_High - slope_Low
    ) %>%
    select(sub, task, delta_threshold, delta_slope)
  
  # Check how many have both High and Low
  n_complete_pf <- sum(is.finite(pf_changes$delta_threshold) & is.finite(pf_changes$delta_slope))
  cat("  PF change scores with both High and Low:", n_complete_pf, "\n")
  
  cat("Computed PF change scores for", nrow(pf_changes), "subject × task combinations\n")
  
  # ============================================================================
  # MERGE PUPIL AND PF CHANGES
  # ============================================================================
  
  cat("\n=== Merging pupil and PF change scores ===\n")
  
  coupling_data <- pupil_changes %>%
    inner_join(pf_changes, by = c("sub", "task"))
  
  cat("Merged data: ", nrow(coupling_data), "subject × task combinations\n")
  
  # Check for finite observations
  cat("\nData availability after merge:\n")
  cat("  Finite delta_cog_auc:", sum(is.finite(coupling_data$delta_cog_auc)), "\n")
  cat("  Finite delta_total_auc:", sum(is.finite(coupling_data$delta_total_auc)), "\n")
  cat("  Finite delta_threshold:", sum(is.finite(coupling_data$delta_threshold)), "\n")
  cat("  Finite delta_slope:", sum(is.finite(coupling_data$delta_slope)), "\n")
  
  # Filter to rows with complete data for correlations
  coupling_data_complete <- coupling_data %>%
    filter(is.finite(delta_cog_auc), is.finite(delta_threshold), 
           is.finite(delta_slope), is.finite(delta_total_auc))
  
  cat("  Complete cases (all deltas finite):", nrow(coupling_data_complete), "\n")
  
  if (nrow(coupling_data_complete) < 3) {
    warning("Not enough complete cases for correlation analysis (need at least 3, have ", 
            nrow(coupling_data_complete), ")")
    cat("\n⚠ Skipping correlation analysis due to insufficient data\n")
    cat("  This may occur if subjects are missing data for one effort condition\n")
  } else {
    # ============================================================================
    # CORRELATIONS: Pupil changes vs PF changes
    # ============================================================================
    
    cat("\n=== Computing correlations ===\n")
    cat("Using", nrow(coupling_data_complete), "complete cases\n")
    
    # Correlation: Δpupil (cognitive) vs ΔPF threshold
    cor_threshold_cog <- cor.test(coupling_data_complete$delta_cog_auc, 
                                   coupling_data_complete$delta_threshold)
  cat("\nΔCognitive Pupil vs ΔPF Threshold:\n")
  cat(sprintf("  r = %.3f\n", cor_threshold_cog$estimate))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
              cor_threshold_cog$conf.int[1], cor_threshold_cog$conf.int[2]))
  cat(sprintf("  p = %.4f\n", cor_threshold_cog$p.value))
  
    # Correlation: Δpupil (cognitive) vs ΔPF slope
    cor_slope_cog <- cor.test(coupling_data_complete$delta_cog_auc, 
                              coupling_data_complete$delta_slope)
    cat("\nΔCognitive Pupil vs ΔPF Slope:\n")
    cat(sprintf("  r = %.3f\n", cor_slope_cog$estimate))
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n", 
                cor_slope_cog$conf.int[1], cor_slope_cog$conf.int[2]))
    cat(sprintf("  p = %.4f\n", cor_slope_cog$p.value))
    
    # Correlation: Δpupil (total) vs ΔPF threshold
    cor_threshold_total <- cor.test(coupling_data_complete$delta_total_auc, 
                                    coupling_data_complete$delta_threshold)
    cat("\nΔTotal Pupil vs ΔPF Threshold:\n")
    cat(sprintf("  r = %.3f\n", cor_threshold_total$estimate))
    cat(sprintf("  p = %.4f\n", cor_threshold_total$p.value))
    
    # Correlation: Δpupil (total) vs ΔPF slope
    cor_slope_total <- cor.test(coupling_data_complete$delta_total_auc, 
                                coupling_data_complete$delta_slope)
  cat("\nΔTotal Pupil vs ΔPF Slope:\n")
  cat(sprintf("  r = %.3f\n", cor_slope_total$estimate))
  cat(sprintf("  p = %.4f\n", cor_slope_total$p.value))
  
    # ============================================================================
    # CREATE CORRELATION SUMMARY TABLE
    # ============================================================================
    
    cor_summary <- tibble(
      pupil_metric = c("Cognitive AUC", "Cognitive AUC", "Total AUC", "Total AUC"),
      pf_parameter = c("Threshold", "Slope", "Threshold", "Slope"),
      correlation = c(cor_threshold_cog$estimate, cor_slope_cog$estimate,
                     cor_threshold_total$estimate, cor_slope_total$estimate),
      ci_lower = c(cor_threshold_cog$conf.int[1], cor_slope_cog$conf.int[1],
                  cor_threshold_total$conf.int[1], cor_slope_total$conf.int[1]),
      ci_upper = c(cor_threshold_cog$conf.int[2], cor_slope_cog$conf.int[2],
                  cor_threshold_total$conf.int[2], cor_slope_total$conf.int[2]),
      p_value = c(cor_threshold_cog$p.value, cor_slope_cog$p.value,
                 cor_threshold_total$p.value, cor_slope_total$p.value),
      n = rep(nrow(coupling_data_complete), 4)
    )
    
    cor_output <- file.path(tables_dir, "pf_pupil_coupling_correlations.csv")
    write_csv(cor_summary, cor_output)
    cat("\n✓ Correlation summary saved to:", cor_output, "\n")
    
    # ============================================================================
    # CREATE FIGURES
    # ============================================================================
    
    cat("\n=== Creating figures ===\n")
    
    # Figure 1: ΔCognitive Pupil vs ΔPF Threshold
    p1 <- coupling_data_complete %>%
    ggplot(aes(x = delta_cog_auc, y = delta_threshold)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    facet_wrap(~ task) +
    labs(
      x = "ΔCognitive Pupil (High - Low Effort)",
      y = "ΔPF Threshold (High - Low Effort)",
      title = "Subject-Level Coupling: Pupil vs PF Threshold"
    ) +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "pf_pupil_coupling_threshold.png"),
         p1, width = 8, height = 6, dpi = 300)
  cat("✓ Saved: pf_pupil_coupling_threshold.png\n")
  
    # Figure 2: ΔCognitive Pupil vs ΔPF Slope
    p2 <- coupling_data_complete %>%
    ggplot(aes(x = delta_cog_auc, y = delta_slope)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    facet_wrap(~ task) +
    labs(
      x = "ΔCognitive Pupil (High - Low Effort)",
      y = "ΔPF Slope (High - Low Effort)",
      title = "Subject-Level Coupling: Pupil vs PF Slope"
    ) +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "pf_pupil_coupling_slope.png"),
         p2, width = 8, height = 6, dpi = 300)
  cat("✓ Saved: pf_pupil_coupling_slope.png\n")
  
    # Figure 3: Combined scatter matrix
    p3 <- coupling_data_complete %>%
    select(delta_cog_auc, delta_total_auc, delta_threshold, delta_slope) %>%
    GGally::ggpairs() +
    theme_minimal()
  
    ggsave(file.path(figures_dir, "pf_pupil_coupling_matrix.png"),
           p3, width = 10, height = 10, dpi = 300)
    cat("✓ Saved: pf_pupil_coupling_matrix.png\n")
  }
  
} else {
  cat("\n⚠ PF parameters not available. Skipping PF-pupil coupling analysis.\n")
  cat("  Run script 02_compute_pf_parameters.R first.\n")
}

# ============================================================================
# CONSISTENCY CHECK: Full behavioral dataset vs pupil subset
# ============================================================================

if (!is.null(pf_params) && nrow(pf_params) > 0) {
  cat("\n=== Consistency check: Full dataset vs pupil subset ===\n")
  
  # This would compare PF parameters estimated from all trials vs. only trials with usable pupil data
  # Implementation depends on whether you have separate PF fits for the full dataset
  # For now, just note that this check should be performed
  
  cat("Note: Consistency check requires PF fits from full behavioral dataset\n")
  cat("      Compare PF parameters from all trials vs. pupil subset trials\n")
}

cat("\n=== Subject-level PF-pupil coupling analysis complete ===\n")

