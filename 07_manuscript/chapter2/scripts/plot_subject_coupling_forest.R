# Regenerate subject-level coupling forest figure and sensitivity tables.

library(here)
library(tidyverse)

source(file.path(here(), "config", "paths_config.R"))
source(file.path(here(), "R", "colors_manuscript.R"))
source(file.path(here(), "R", "pf_analysis_helpers.R"))
source(file.path(here(), "R", "subject_coupling_helpers.R"))
source(file.path(here(), "R", "plot_subject_coupling_forest.R"))

dat <- readr::read_csv(merged_trial_file, show_col_types = FALSE)
pf_params <- readr::read_csv(pf_params_file, show_col_types = FALSE)

coupling_data_complete <- build_subject_coupling_data(dat, pf_params)
cor_summary <- build_coupling_correlation_summary(coupling_data_complete)
sensitivity <- build_coupling_sensitivity_summary(coupling_data_complete)
influence_audit <- build_coupling_influence_audit(coupling_data_complete, pf_params)

readr::write_csv(cor_summary, file.path(tables_dir, "pf_pupil_coupling_correlations.csv"))
readr::write_csv(sensitivity, file.path(tables_dir, "pf_pupil_coupling_sensitivity.csv"))
readr::write_csv(influence_audit, file.path(tables_dir, "pf_pupil_coupling_influence_audit.csv"))

p_forest <- plot_subject_coupling_forest(
  cor_summary,
  sensitivity_df = sensitivity,
  task_color_map = task_colors
)
ggplot2::ggsave(
  file.path(figures_dir, "fig_subject_coupling_forest.png"),
  p_forest, width = 9, height = 5.5, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  file.path(figures_dir, "fig_subject_coupling_forest.pdf"),
  p_forest, width = 9, height = 5.5, bg = "white"
)

p_z <- plot_subject_coupling_zscatter(coupling_data_complete, task_color_map = task_colors)
ggplot2::ggsave(
  file.path(figures_dir, "fig_subject_coupling_zscatter.png"),
  p_z, width = 9, height = 7, dpi = 300, bg = "white"
)

cat("✓ Saved fig_subject_coupling_forest.png / .pdf\n")
cat("✓ Saved fig_subject_coupling_zscatter.png\n")
print(cor_summary)
