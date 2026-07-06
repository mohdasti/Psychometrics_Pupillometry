# Script 6: Pupil-Psychometric Coupling (PRIMARY ANALYSIS)
# =========================================================
# Purpose: Fit hierarchical GLMM linking phasic arousal to psychometric sensitivity
#          Key test: Does stimulus_intensity × pupil_cognitive_state interaction predict choice?
# Input: ch2_triallevel_merged.csv (with quality tiers and centered pupil metrics)
# Output: Model results, tables, and figures
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(lme4)
library(broom.mixed)
library(here)

source(file.path(here(), "config", "paths_config.R"))
source(file.path(here(), "R", "colors_manuscript.R"))

dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# Load data
cat("Loading trial-level data...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

# ============================================================================
# PREPARE DATA FOR PRIMARY ANALYSIS
# ============================================================================

cat("\n=== Preparing data for primary GLMM analysis ===\n")

# Use primary quality tier
dat_primary <- dat %>%
  filter(quality_primary == TRUE) %>%
  filter(!is.na(pupil_cognitive_state), !is.na(stimulus_intensity), !is.na(choice_num)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

cat("Primary quality tier: ", nrow(dat_primary), " trials\n")
cat("Subjects: ", length(unique(dat_primary$sub)), "\n")

# Ensure choice_num is binary (0/1)
if (!"choice_num" %in% names(dat_primary)) {
  if ("choice" %in% names(dat_primary)) {
    dat_primary$choice_num <- ifelse(dat_primary$choice == "DIFFERENT" | 
                                     dat_primary$choice == 1 | 
                                     dat_primary$choice == TRUE, 1, 0)
  } else {
    stop("Cannot find choice variable for GLMM")
  }
}

# Standardize stimulus intensity for better convergence (optional but recommended)
dat_primary <- dat_primary %>%
  group_by(task_factor) %>%
  mutate(
    stimulus_intensity_scaled = scale(stimulus_intensity)[,1]
  ) %>%
  ungroup()

# Standardize pupil state (within-subject centered, so mean should be ~0)
# Can scale for interpretability if needed
dat_primary$pupil_cognitive_state_scaled <- scale(dat_primary$pupil_cognitive_state)[,1]
dat_primary$pupil_cognitive_trait_scaled <- scale(dat_primary$pupil_cognitive_trait)[,1]

# ============================================================================
# PRIMARY MODEL: Full GLMM with probit link
# ============================================================================

cat("\n=== Fitting primary GLMM (probit link) ===\n")
cat("Model: choice ~ stimulus_intensity + effort + task + pupil_state + 
     stimulus_intensity:pupil_state + pupil_trait + (1 + stimulus_intensity | sub)\n")

# Fit GLMM with probit link
# Key interaction: stimulus_intensity × pupil_cognitive_state
mod_primary <- glmer(
  choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled +
    effort_factor + task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_primary,
  family = binomial(link = "probit")
)

cat("\nModel summary:\n")
print(summary(mod_primary))

# Extract fixed effects
fe_primary <- broom.mixed::tidy(mod_primary, effects = "fixed")
cat("\nFixed effects:\n")
print(fe_primary)

# Save model
saveRDS(mod_primary, file.path(models_dir, "mod_pupil_psychometric_primary.rds"))

# Save fixed effects table
write_csv(fe_primary, file.path(tables_dir, "pupil_psychometric_primary_effects.csv"))

# ============================================================================
# KEY INTERACTION TEST
# ============================================================================

cat("\n=== Testing key interaction: stimulus_intensity × pupil_cognitive_state ===\n")

interaction_term <- fe_primary %>%
  filter(term == "stimulus_intensity_scaled:pupil_cognitive_state_scaled")

if (nrow(interaction_term) > 0) {
  cat("\nInteraction effect:\n")
  cat(sprintf("  Estimate: %.4f\n", interaction_term$estimate))
  cat(sprintf("  SE: %.4f\n", interaction_term$std.error))
  cat(sprintf("  95%% CI: [%.4f, %.4f]\n", 
              interaction_term$estimate - 1.96 * interaction_term$std.error,
              interaction_term$estimate + 1.96 * interaction_term$std.error))
  cat(sprintf("  z-value: %.3f\n", interaction_term$statistic))
  cat(sprintf("  p-value: %.4f\n", interaction_term$p.value))
  
  if (interaction_term$p.value < 0.05) {
    cat("\n  ✓ SIGNIFICANT: Pupil state modulates psychometric sensitivity\n")
  } else {
    cat("\n  × Non-significant: No evidence for pupil-state modulation of sensitivity\n")
  }
}

# ============================================================================
# ALTERNATIVE MODEL: Without interaction (for comparison)
# ============================================================================

cat("\n=== Fitting alternative model (no interaction) ===\n")

mod_no_interaction <- glmer(
  choice_num ~ stimulus_intensity_scaled + pupil_cognitive_state_scaled +
    effort_factor + task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_primary,
  family = binomial(link = "probit")
)

# Compare models (AIC)
aic_primary <- AIC(mod_primary)
aic_no_int <- AIC(mod_no_interaction)

cat("\nModel comparison:\n")
cat(sprintf("  Primary model (with interaction): AIC = %.2f\n", aic_primary))
cat(sprintf("  Alternative model (no interaction): AIC = %.2f\n", aic_no_int))
cat(sprintf("  ΔAIC: %.2f\n", aic_primary - aic_no_int))

if (aic_primary < aic_no_int) {
  cat("  ✓ Primary model (with interaction) has better fit\n")
} else {
  cat("  × Alternative model (no interaction) has better fit\n")
}

saveRDS(mod_no_interaction, file.path(models_dir, "mod_pupil_psychometric_no_interaction.rds"))

# ============================================================================
# EFFORT × INTENSITY MODEL: Direct test of effort modulating psychometric slope
# ============================================================================
# This model adds effort_factor * stimulus_intensity_scaled to the primary model.
# The effort main effect tests whether effort shifts overall "different" responding
# (criterion/intercept); the effort × intensity interaction tests whether effort
# changes the steepness of the psychometric function (sensitivity).

cat("\n=== Fitting effort × intensity model ===\n")
cat("Model: choice ~ stimulus_intensity * pupil_state + stimulus_intensity * effort + task + pupil_trait + (1 + intensity | sub)\n")

mod_effort_intensity <- glmer(
  choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled +
    stimulus_intensity_scaled * effort_factor +
    task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_primary,
  family = binomial(link = "probit")
)

cat("\nEffort × intensity model summary:\n")
print(summary(mod_effort_intensity))

fe_effort_intensity <- broom.mixed::tidy(mod_effort_intensity, effects = "fixed")
cat("\nFixed effects:\n")
print(fe_effort_intensity)

saveRDS(mod_effort_intensity, file.path(models_dir, "mod_pupil_psychometric_effort_intensity.rds"))
write_csv(fe_effort_intensity, file.path(tables_dir, "pupil_psychometric_effort_intensity_effects.csv"))

eff_int_term <- fe_effort_intensity %>%
  filter(term == "stimulus_intensity_scaled:effort_factorHigh")
if (nrow(eff_int_term) > 0) {
  cat(sprintf("\n  Effort × Intensity: beta = %.4f, SE = %.4f, z = %.3f, p = %.4f\n",
              eff_int_term$estimate, eff_int_term$std.error,
              eff_int_term$statistic, eff_int_term$p.value))
}

# ============================================================================
# ROBUSTNESS CHECKS: Test across quality tiers
# ============================================================================

cat("\n=== Robustness checks: Testing across quality tiers ===\n")

# Lenient tier
dat_lenient <- dat %>%
  filter(quality_lenient == TRUE) %>%
  filter(!is.na(pupil_cognitive_state), !is.na(stimulus_intensity), !is.na(choice_num)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task),
    stimulus_intensity_scaled = scale(stimulus_intensity)[,1],
    pupil_cognitive_state_scaled = scale(pupil_cognitive_state)[,1],
    pupil_cognitive_trait_scaled = scale(pupil_cognitive_trait)[,1]
  )

mod_lenient <- glmer(
  choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled +
    effort_factor + task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_lenient,
  family = binomial(link = "probit")
)

fe_lenient <- broom.mixed::tidy(mod_lenient, effects = "fixed")
interaction_lenient <- fe_lenient %>%
  filter(term == "stimulus_intensity_scaled:pupil_cognitive_state_scaled")

cat("\nLenient tier (≥0.50):\n")
cat(sprintf("  N trials: %d\n", nrow(dat_lenient)))
if (nrow(interaction_lenient) > 0) {
  cat(sprintf("  Interaction estimate: %.4f (p = %.4f)\n", 
              interaction_lenient$estimate, interaction_lenient$p.value))
}

saveRDS(mod_lenient, file.path(models_dir, "mod_pupil_psychometric_lenient.rds"))
write_csv(fe_lenient, file.path(tables_dir, "pupil_psychometric_lenient_effects.csv"))

# Strict tier
dat_strict <- dat %>%
  filter(quality_strict == TRUE) %>%
  filter(!is.na(pupil_cognitive_state), !is.na(stimulus_intensity), !is.na(choice_num)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task),
    stimulus_intensity_scaled = scale(stimulus_intensity)[,1],
    pupil_cognitive_state_scaled = scale(pupil_cognitive_state)[,1],
    pupil_cognitive_trait_scaled = scale(pupil_cognitive_trait)[,1]
  )

mod_strict <- glmer(
  choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled +
    effort_factor + task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_strict,
  family = binomial(link = "probit")
)

fe_strict <- broom.mixed::tidy(mod_strict, effects = "fixed")
interaction_strict <- fe_strict %>%
  filter(term == "stimulus_intensity_scaled:pupil_cognitive_state_scaled")

cat("\nStrict tier (≥0.70):\n")
cat(sprintf("  N trials: %d\n", nrow(dat_strict)))
if (nrow(interaction_strict) > 0) {
  cat(sprintf("  Interaction estimate: %.4f (p = %.4f)\n", 
              interaction_strict$estimate, interaction_strict$p.value))
}

saveRDS(mod_strict, file.path(models_dir, "mod_pupil_psychometric_strict.rds"))
write_csv(fe_strict, file.path(tables_dir, "pupil_psychometric_strict_effects.csv"))

# ============================================================================
# SLOW-RT SENSITIVITY: Motor contamination subset (RT > 1.5 s)
# ============================================================================
# Pre-specified: trials where button press occurs late enough that motor execution
# cannot overlap the primary cognitive pupil window (see manuscript Methods).
# Merged file may omit cog_win_uncontaminated_by_motor; use RT > 1.5 s filter.

cat("\n=== Slow-RT sensitivity GLMM (RT > 1.5 s) ===\n")

dat_slow_rt <- dat %>%
  filter(quality_primary == TRUE) %>%
  filter(!is.na(pupil_cognitive_state), !is.na(stimulus_intensity), !is.na(choice_num)) %>%
  filter(!is.na(rt), rt > 1.5) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

cat(sprintf("  Trials: %d; Subjects: %d\n", nrow(dat_slow_rt), length(unique(dat_slow_rt$sub))))

dat_slow_rt <- dat_slow_rt %>%
  group_by(task_factor) %>%
  mutate(stimulus_intensity_scaled = scale(stimulus_intensity)[, 1]) %>%
  ungroup()
dat_slow_rt$pupil_cognitive_state_scaled <- scale(dat_slow_rt$pupil_cognitive_state)[, 1]
dat_slow_rt$pupil_cognitive_trait_scaled <- scale(dat_slow_rt$pupil_cognitive_trait)[, 1]

mod_slow_rt <- glmer(
  choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_scaled +
    effort_factor + task_factor +
    pupil_cognitive_trait_scaled +
    (1 + stimulus_intensity_scaled | sub),
  data = dat_slow_rt,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

fe_slow_rt <- broom.mixed::tidy(mod_slow_rt, effects = "fixed")
saveRDS(mod_slow_rt, file.path(models_dir, "mod_pupil_psychometric_slow_rt.rds"))
write_csv(fe_slow_rt, file.path(tables_dir, "pupil_psychometric_slow_rt_effects.csv"))

# ============================================================================
# CREATE FIGURES
# ============================================================================

cat("\n=== Creating figures ===\n")

# Figure 1: Psychometric curves by pupil state tertiles
# Compute tertiles of pupil_cognitive_state
dat_primary <- dat_primary %>%
  mutate(
    pupil_state_tertile = cut(pupil_cognitive_state, 
                              breaks = quantile(pupil_cognitive_state, 
                                               probs = c(0, 1/3, 2/3, 1), 
                                               na.rm = TRUE),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)
  )

# Plot psychometric functions by pupil tertile
p1 <- dat_primary %>%
  filter(!is.na(pupil_state_tertile)) %>%
  ggplot(aes(x = stimulus_intensity, y = choice_num, color = pupil_state_tertile)) +
  stat_summary_bin(fun = "mean", bins = 10, geom = "point", size = 2, alpha = 0.7) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_grid(task_factor ~ effort_factor) +
  scale_color_manual(values = pupil_tertile_colors) +
  labs(
    x = "Stimulus Intensity",
    y = "Proportion 'Different'",
    color = "Pupil State\nTertile",
    title = "Psychometric Functions by Pupil State"
  ) +
  theme_ch2_pf()

ggsave(file.path(figures_dir, "psychometric_by_pupil_state.png"),
       p1, width = 10, height = 8, dpi = 300)
cat("✓ Saved: psychometric_by_pupil_state.png\n")

# Figure 2: Interaction plot (predicted probabilities)
# Generate predictions for visualization
pred_data <- expand.grid(
  stimulus_intensity_scaled = seq(-2, 2, length.out = 50),
  pupil_cognitive_state_scaled = c(-1, 0, 1),  # Low, Medium, High
  effort_factor = levels(dat_primary$effort_factor),
  task_factor = levels(dat_primary$task_factor),
  pupil_cognitive_trait_scaled = 0  # Set to mean
)

pred_data$pred <- predict(mod_primary, newdata = pred_data, type = "response", re.form = NA)

p2 <- pred_data %>%
  mutate(
    pupil_state_label = factor(pupil_cognitive_state_scaled,
                              levels = c(-1, 0, 1),
                              labels = c("Low Pupil", "Medium Pupil", "High Pupil"))
  ) %>%
  ggplot(aes(x = stimulus_intensity_scaled, y = pred, color = pupil_state_label)) +
  geom_line(size = 1.2) +
  facet_grid(task_factor ~ effort_factor) +
  scale_color_manual(values = pupil_state_label_colors) +
  labs(
    x = "Stimulus Intensity (scaled)",
    y = "Predicted Probability 'Different'",
    color = "Pupil State",
    title = "Model Predictions: Stimulus × Pupil State Interaction"
  ) +
  theme_ch2_pf()

ggsave(file.path(figures_dir, "pupil_psychometric_interaction.png"),
       p2, width = 10, height = 8, dpi = 300)
cat("✓ Saved: pupil_psychometric_interaction.png\n")
# Alias for manuscript / Quarto (Appendix B, Figure B1)
ggsave(file.path(figures_dir, "fig_predicted_psychometric_pupil_state.png"),
       p2, width = 10, height = 8, dpi = 300)
cat("✓ Saved: fig_predicted_psychometric_pupil_state.png\n")

# TOST equivalence summary and forest figure (Chapter 2 B2)
source(file.path(here(), "R", "tost_equivalence_helpers.R"))
source(file.path(here(), "R", "plot_equivalence_forest.R"))
tost_summary <- build_interaction_tost_summary(models_dir)
write_csv(
  tost_summary,
  file.path(tables_dir, "pupil_psychometric_tost_equivalence.csv")
)
p_eq <- plot_equivalence_forest(
  tost_summary,
  line_color = unname(effort_colors["Low"])
)
ggplot2::ggsave(
  file.path(figures_dir, "fig_equivalence_forest.png"),
  p_eq, width = 8, height = 5.5, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  file.path(figures_dir, "fig_equivalence_forest.pdf"),
  p_eq, width = 8, height = 5.5, bg = "white"
)
cat("✓ Saved: fig_equivalence_forest.png / .pdf\n")

cat("\n=== Pupil-psychometric coupling analysis complete ===\n")

