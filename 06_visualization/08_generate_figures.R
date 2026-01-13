# Script 8: Generate All Figures for Chapter 2
# =============================================
# Purpose: Create publication-ready figures integrating all analyses
# Input: Results from previous scripts (models, processed data)
# Output: Comprehensive figures for dissertation
#
# Author: Mohammad Dastgheib
# Date: Created for Chapter 2 analysis

library(tidyverse)
library(here)

# Set paths
data_dir <- here("07_manuscript", "chapter2", "data")
processed_dir <- file.path(data_dir, "processed")
output_dir <- here("07_manuscript", "chapter2", "output")
figures_dir <- file.path(output_dir, "figures")

# Load data
cat("Loading data for figure generation...\n")
dat_file <- file.path(processed_dir, "ch2_triallevel_merged.csv")
dat <- read_csv(dat_file, show_col_types = FALSE)

pf_file <- file.path(processed_dir, "ch2_pf_parameters.csv")
if (file.exists(pf_file)) {
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
} else {
  pf_params <- NULL
  warning("PF parameters not found. Some figures will be skipped.")
}

# ============================================================================
# FIGURE 1: Psychometric Functions by Effort (Behavioral Backbone)
# ============================================================================

cat("\n=== Figure 1: Psychometric Functions by Effort ===\n")

if (!is.null(pf_params)) {
  # Plot PF parameters
  # This would show thresholds and slopes by effort condition
  # Implementation depends on how PF parameters are structured
  cat("PF parameter visualization (to be implemented based on PF structure)\n")
}

# Alternative: Plot raw psychometric functions from trial data
dat_fig1 <- dat %>%
  filter(!is.na(choice_num)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

if (!"choice_num" %in% names(dat_fig1)) {
  if ("choice" %in% names(dat_fig1)) {
    dat_fig1$choice_num <- ifelse(dat_fig1$choice == "DIFFERENT" | 
                                  dat_fig1$choice == 1, 1, 0)
  }
}

p1 <- dat_fig1 %>%
  ggplot(aes(x = stimulus_intensity, y = choice_num, color = effort_factor)) +
  stat_summary_bin(fun = "mean", bins = 8, geom = "point", size = 2, alpha = 0.7) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ task_factor, scales = "free_x") +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    x = "Stimulus Intensity",
    y = "Proportion 'Different'",
    color = "Effort",
    title = "Psychometric Functions by Effort Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(figures_dir, "fig1_psychometric_by_effort.png"),
       p1, width = 10, height = 6, dpi = 300)
cat("✓ Saved: fig1_psychometric_by_effort.png\n")

# ============================================================================
# FIGURE 2: Effort-Pupil Manipulation Check (Summary)
# ============================================================================

cat("\n=== Figure 2: Effort-Pupil Manipulation Check ===\n")

dat_fig2 <- dat %>%
  filter(quality_primary == TRUE) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

# Combine both Total AUC and Cognitive AUC in one figure
p2a <- dat_fig2 %>%
  select(sub, task_factor, effort_factor, total_auc, cog_auc) %>%
  pivot_longer(cols = c(total_auc, cog_auc), names_to = "metric", values_to = "value") %>%
  mutate(metric_label = factor(metric, 
                               levels = c("total_auc", "cog_auc"),
                               labels = c("Total AUC", "Cognitive AUC"))) %>%
  ggplot(aes(x = effort_factor, y = value, fill = effort_factor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
  facet_grid(metric_label ~ task_factor, scales = "free_y") +
  scale_fill_manual(values = c("Low" = "lightblue", "High" = "darkred")) +
  labs(
    x = "Effort Condition",
    y = "Pupil Metric (baseline-corrected)",
    fill = "Effort",
    title = "Effort-Pupil Manipulation Check"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(figures_dir, "fig2_effort_pupil_manipulation.png"),
       p2a, width = 10, height = 8, dpi = 300)
cat("✓ Saved: fig2_effort_pupil_manipulation.png\n")

# ============================================================================
# FIGURE 3: Psychometric Functions by Pupil State (Primary Result)
# ============================================================================

cat("\n=== Figure 3: Psychometric Functions by Pupil State ===\n")

dat_fig3 <- dat %>%
  filter(quality_primary == TRUE) %>%
  filter(!is.na(pupil_cognitive_state), !is.na(choice_num)) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task),
    pupil_state_tertile = cut(pupil_cognitive_state,
                              breaks = quantile(pupil_cognitive_state,
                                               probs = c(0, 1/3, 2/3, 1),
                                               na.rm = TRUE),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)
  )

if (!"choice_num" %in% names(dat_fig3)) {
  if ("choice" %in% names(dat_fig3)) {
    dat_fig3$choice_num <- ifelse(dat_fig3$choice == "DIFFERENT" | 
                                  dat_fig3$choice == 1, 1, 0)
  }
}

p3 <- dat_fig3 %>%
  filter(!is.na(pupil_state_tertile)) %>%
  ggplot(aes(x = stimulus_intensity, y = choice_num, color = pupil_state_tertile)) +
  stat_summary_bin(fun = "mean", bins = 8, geom = "point", size = 2, alpha = 0.7) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_grid(task_factor ~ effort_factor) +
  scale_color_manual(values = c("Low" = "blue", "Medium" = "gray", "High" = "red")) +
  labs(
    x = "Stimulus Intensity",
    y = "Proportion 'Different'",
    color = "Pupil State\nTertile",
    title = "Psychometric Functions by Pupil State (Primary Analysis)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(figures_dir, "fig3_psychometric_by_pupil_state.png"),
       p3, width = 12, height = 8, dpi = 300)
cat("✓ Saved: fig3_psychometric_by_pupil_state.png\n")

# ============================================================================
# FIGURE 4: Missingness Diagnostic
# ============================================================================

cat("\n=== Figure 4: Missingness Diagnostic ===\n")

dat_fig4 <- dat %>%
  mutate(
    pupil_usable = quality_primary == TRUE,
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

p4 <- dat_fig4 %>%
  group_by(task_factor, effort_factor, pupil_usable) %>%
  summarise(
    mean_stimulus_intensity = mean(stimulus_intensity, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(usable_label = ifelse(pupil_usable, "Usable", "Missing")) %>%
  ggplot(aes(x = effort_factor, y = mean_rt, fill = usable_label)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_wrap(~ task_factor) +
  scale_fill_manual(values = c("Usable" = "green", "Missing" = "red")) +
  labs(
    x = "Effort Condition",
    y = "Mean RT (seconds)",
    fill = "Pupil Data",
    title = "Missingness Diagnostic: RT by Pupil Data Availability"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "fig4_missingness_diagnostic.png"),
       p4, width = 8, height = 6, dpi = 300)
cat("✓ Saved: fig4_missingness_diagnostic.png\n")

# ============================================================================
# FIGURE 5: Subject-Level PF-Pupil Coupling (if available)
# ============================================================================

cat("\n=== Figure 5: Subject-Level PF-Pupil Coupling ===\n")

# This would use results from script 07
# For now, create placeholder note
cat("Subject-level coupling figures should be generated by script 07\n")
cat("  Check figures: pf_pupil_coupling_threshold.png, pf_pupil_coupling_slope.png\n")

# ============================================================================
# CREATE COMPOSITE FIGURE (Optional)
# ============================================================================

cat("\n=== Creating composite figure layout ===\n")
cat("Note: Use external tools (e.g., Inkscape, Adobe Illustrator) to combine figures\n")
cat("      for final publication layout\n")

cat("\n=== Figure generation complete ===\n")
cat("\nAll figures saved to:", figures_dir, "\n")

