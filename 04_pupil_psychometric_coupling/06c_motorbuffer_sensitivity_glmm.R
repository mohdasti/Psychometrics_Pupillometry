# Motor-buffered pupil sensitivity GLMM (Tier-1 appendix analysis).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lme4)
  library(broom.mixed)
  library(here)
})

source(file.path(here(), "config", "paths_config.R"))
source(file.path(here(), "R", "motorbuffer_pupil_helpers.R"))
source(file.path(here(), "R", "tost_equivalence_helpers.R"))

dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(merged_trial_file)) {
  stop("Merged trial file not found: ", merged_trial_file)
}

dat <- read_csv(merged_trial_file, show_col_types = FALSE)

if (!"cog_mean_motorbuffered" %in% names(dat)) {
  cat("Motor-buffer columns missing; running compute_motorbuffered_cog_mean.R ...\n")
  source(file.path(here(), "01_data_preparation", "compute_motorbuffered_cog_mean.R"))
  dat <- read_csv(merged_trial_file, show_col_types = FALSE)
}

if (!"pupil_cognitive_state_motorbuffer" %in% names(dat)) {
  dat <- add_motorbuffer_state_trait(dat)
}

cat("\n=== Motor-buffer sensitivity GLMM ===\n")
fit <- fit_motorbuffer_glmm(dat)
mod <- fit$model
fe <- broom.mixed::tidy(mod, effects = "fixed")

cat(sprintf(
  "Sample: %d trials, %d subjects\n",
  nrow(fit$data),
  dplyr::n_distinct(fit$data$sub)
))

int <- fe[fe$term == CH2_MOTORBUFFER_INTERACTION_TERM, , drop = FALSE]
if (nrow(int)) {
  cat(sprintf(
    "Interaction: beta = %.4f, SE = %.4f, z = %.3f, p = %.4f\n",
    int$estimate, int$std.error, int$statistic, int$p.value
  ))
}

saveRDS(mod, file.path(models_dir, "mod_pupil_psychometric_motorbuffer.rds"))
write_csv(fe, file.path(tables_dir, "pupil_psychometric_motorbuffer_effects.csv"))

summary_row <- summarize_motorbuffer_glmm(fit)
write_csv(summary_row, file.path(tables_dir, "pupil_psychometric_motorbuffer_summary.csv"))

tost_row <- extract_interaction_tost(
  mod,
  term = CH2_MOTORBUFFER_INTERACTION_TERM,
  analysis_id = "motorbuffer",
  analysis_label = "Motor-buffered window (truncated 150 ms pre-press)"
)
write_csv(tost_row, file.path(tables_dir, "pupil_psychometric_motorbuffer_tost.csv"))

# Refresh combined TOST table and forest plot (includes motor-buffer row when present)
tost_summary <- build_interaction_tost_summary(models_dir)
write_csv(
  tost_summary,
  file.path(tables_dir, "pupil_psychometric_tost_equivalence.csv")
)
source(file.path(here(), "R", "plot_equivalence_forest.R"))
source(file.path(here(), "R", "colors_manuscript.R"))
p_eq <- plot_equivalence_forest(tost_summary)
ggplot2::ggsave(
  file.path(figures_dir, "fig_equivalence_forest.png"),
  p_eq, width = 8, height = 6, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  file.path(figures_dir, "fig_equivalence_forest.pdf"),
  p_eq, width = 8, height = 6, bg = "white"
)
cat("Updated fig_equivalence_forest with motor-buffer row\n")

cat("\n=== Motor-buffer sensitivity complete ===\n")
