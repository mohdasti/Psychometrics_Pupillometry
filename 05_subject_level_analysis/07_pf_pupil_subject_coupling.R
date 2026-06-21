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
source(file.path(here(), "R", "colors_manuscript.R"))
source(file.path(here(), "R", "pf_analysis_helpers.R"))
source(file.path(here(), "R", "subject_coupling_helpers.R"))
source(file.path(here(), "R", "plot_subject_coupling_forest.R"))

# Load data
cat("Loading data...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

pf_file <- pf_params_file
if (!file.exists(pf_file)) {
  warning("PF parameters file not found. Run script 02_compute_pf_parameters.R first.")
  pf_params <- NULL
} else {
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
  cat("Loaded PF parameters for", length(unique(pf_params$sub)), "subjects\n")
}

if (!is.null(pf_params) && nrow(pf_params) > 0) {
  cat("\n=== Building subject-level coupling data ===\n")
  coupling_data_complete <- build_subject_coupling_data(dat, pf_params)
  cat("Complete cases:", nrow(coupling_data_complete), "\n")
  cat("N by task:\n")
  print(table(coupling_data_complete$task))

  cat("\n=== Computing correlations and sensitivity analyses ===\n")
  cor_summary <- build_coupling_correlation_summary(coupling_data_complete)
  sensitivity <- build_coupling_sensitivity_summary(coupling_data_complete)
  influence_audit <- build_coupling_influence_audit(coupling_data_complete, pf_params)

  print(cor_summary)
  print(sensitivity %>% select(task, row_label, n_influential, r_pearson, r_pearson_excl, r_spearman))

  write_csv(cor_summary, file.path(tables_dir, "pf_pupil_coupling_correlations.csv"))
  write_csv(sensitivity, file.path(tables_dir, "pf_pupil_coupling_sensitivity.csv"))
  write_csv(influence_audit, file.path(tables_dir, "pf_pupil_coupling_influence_audit.csv"))
  cat("\n✓ Correlation and sensitivity tables saved\n")

  cat("\n=== Creating figures ===\n")
  p_forest <- plot_subject_coupling_forest(cor_summary, task_color_map = task_colors)
  ggsave(
    file.path(figures_dir, "fig_subject_coupling_forest.png"),
    p_forest, width = 8.5, height = 5.5, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(figures_dir, "fig_subject_coupling_forest.pdf"),
    p_forest, width = 8.5, height = 5.5, bg = "white"
  )
  cat("✓ Saved: fig_subject_coupling_forest.png / .pdf\n")

  p_z <- plot_subject_coupling_zscatter(coupling_data_complete, task_color_map = task_colors)
  ggsave(
    file.path(figures_dir, "fig_subject_coupling_zscatter.png"),
    p_z, width = 9, height = 7, dpi = 300, bg = "white"
  )
  cat("✓ Saved: fig_subject_coupling_zscatter.png\n")

  # Legacy scatter panels (appendix / archival)
  p1 <- coupling_data_complete %>%
    ggplot(aes(x = delta_cog_auc, y = delta_threshold)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = coupling_line_color) +
    facet_wrap(~ task, scales = "free_y") +
    labs(
      x = "ΔCognitive Pupil (High - Low Effort)",
      y = "ΔPF Threshold (High - Low Effort)",
      title = "Subject-Level Coupling: Pupil vs PF Threshold (legacy panels)"
    ) +
    theme_minimal()
  ggsave(file.path(figures_dir, "pf_pupil_coupling_threshold.png"), p1, width = 8, height = 6, dpi = 300)

  p2 <- coupling_data_complete %>%
    ggplot(aes(x = delta_cog_auc, y = delta_slope)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = coupling_line_color) +
    facet_wrap(~ task, scales = "free_y") +
    labs(
      x = "ΔCognitive Pupil (High - Low Effort)",
      y = "ΔPF Slope (High - Low Effort)",
      title = "Subject-Level Coupling: Pupil vs PF Slope (legacy panels)"
    ) +
    theme_minimal()
  ggsave(file.path(figures_dir, "pf_pupil_coupling_slope.png"), p2, width = 8, height = 6, dpi = 300)

  p3 <- coupling_data_complete %>%
    select(delta_cog_auc, delta_total_auc, delta_threshold, delta_slope) %>%
    GGally::ggpairs() +
    theme_minimal()
  ggsave(file.path(figures_dir, "pf_pupil_coupling_matrix.png"), p3, width = 10, height = 10, dpi = 300)
  cat("✓ Saved legacy panel figures\n")
} else {
  cat("\n⚠ PF parameters not available. Skipping PF-pupil coupling analysis.\n")
  cat("  Run script 02_compute_pf_parameters.R first.\n")
}

if (!is.null(pf_params) && nrow(pf_params) > 0) {
  cat("\n=== Consistency check: Full dataset vs pupil subset ===\n")
  cat("Note: Consistency check requires PF fits from full behavioral dataset\n")
  cat("      Compare PF parameters from all trials vs. pupil subset trials\n")
}

cat("\n=== Subject-level PF-pupil coupling analysis complete ===\n")
