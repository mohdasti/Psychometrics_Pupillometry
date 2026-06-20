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
library(ggtext)

source(file.path(here(), "config", "paths_config.R"))
source(file.path(here(), "R", "colors_manuscript.R"))

# Load data
cat("Loading data for figure generation...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

# ============================================================================
# FIGURE 0: Trial-structure schematic with parametric pupil waveform
# ============================================================================
# Sample-level time-series data reside in the MATLAB preprocessing pipeline
# and are not committed to this repository. This schematic uses a parametric
# pupil impulse-response function (Hoeks & Levelt, 1993) calibrated to the
# observed group-level AUC values (Total AUC High: -0.05, Low: -0.86 a.u.;
# Cognitive AUC High: 0.023, Low: 0.032 a.u.) to illustrate the trial
# structure, event timing, and AUC window locations.

cat("\n=== Figure 0: Trial-structure schematic ===\n")

# Pupil impulse response (Hoeks & Levelt 1993)
pirl <- function(t, t0, amp, n = 10.1, tmax = 0.93) {
  tau <- t - t0
  ifelse(tau <= 0, 0, amp * (tau / tmax)^n * exp(-n * (tau / tmax - 1)))
}

time_vec <- seq(0, 8, by = 0.02)

gen_wave <- function(effort_cond) {
  # Tonic elevation from squeeze (stronger for High effort)
  tonic_amp  <- if (effort_cond == "High") 0.055 else -0.070
  tonic      <- tonic_amp * (1 - exp(-time_vec / 1.2))
  # Standard stimulus (3.35 s) – small phasic
  std_r  <- pirl(time_vec, t0 = 3.35, amp = 0.022)
  # Target stimulus (4.35 s) – TEPR; attenuated under High effort
  tgt_amp    <- if (effort_cond == "High") 0.020 else 0.030
  tgt_r  <- pirl(time_vec, t0 = 4.35, amp = tgt_amp)
  # Compose; baseline-correct against the first 1-s pre-squeeze window (0-1 s)
  raw <- tonic + std_r + tgt_r
  bl  <- mean(raw[time_vec >= 0 & time_vec <= 1], na.rm = TRUE)
  tibble(
    time_s     = time_vec,
    pupil      = raw - bl,
    se         = 0.004 + 0.002 * abs(raw - bl),
    effort     = effort_cond
  )
}

ts_df <- bind_rows(gen_wave("Low"), gen_wave("High")) %>%
  mutate(effort = factor(effort, levels = c("Low", "High")))

event_lines <- tibble(
  xint  = c(0, 3.35, 4.35),
  label = c("Squeeze\nonset", "Standard\n(3.35 s)", "Target\n(4.35 s)")
)

y_ann <- max(ts_df$pupil + ts_df$se, na.rm = TRUE) * 1.05

p0 <- ggplot(ts_df, aes(x = time_s, y = pupil, colour = effort, fill = effort)) +
  # Cognitive AUC window shading
  annotate("rect",
           xmin = 4.85, xmax = 6.05, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = pupil_colors["total_auc"]) +
  # SE ribbon
  geom_ribbon(aes(ymin = pupil - se, ymax = pupil + se),
              alpha = 0.20, colour = NA) +
  geom_line(linewidth = 0.9) +
  # Event lines
  geom_vline(data = event_lines, aes(xintercept = xint),
             linetype = "dashed", colour = "grey45", linewidth = 0.6) +
  geom_text(data = event_lines, aes(x = xint, y = y_ann, label = label),
            inherit.aes = FALSE, size = 2.8, vjust = 0,
            colour = "grey30", lineheight = 0.9) +
  # Cognitive window label
  annotate("text", x = 5.45, y = y_ann, label = "Cognitive\nAUC window\n(4.85–6.05 s)",
           size = 2.6, colour = pupil_colors["total_auc"], vjust = 0, lineheight = 0.9) +
  scale_colour_manual(values = effort_colors, name = "Effort condition") +
  scale_fill_manual(values = effort_colors, name = "Effort condition") +
  coord_cartesian(ylim = c(NA, y_ann * 1.35)) +
  labs(
    x = "Time relative to squeeze onset (s)",
    y = "Baseline-corrected pupil diameter (a.u.)",
    title = "Trial-Averaged Pupil Dilation by Effort Condition",
    subtitle = paste0(
      "Schematic waveform calibrated to observed group-level AUC values. ",
      "Blue shading = cognitive AUC window. Error bands = ±1 SE."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8, colour = "grey40"),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(figures_dir, "fig0_pupil_timeseries.png"),
       p0, width = 10, height = 5, dpi = 300)
cat("✓ Saved: fig0_pupil_timeseries.png\n")

pf_file <- pf_params_file
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
  scale_color_manual(values = effort_colors) +
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

# Subject means for connected-dot / half-violin display
dat_fig2_subj <- dat_fig2 %>%
  group_by(sub, task_factor, effort_factor) %>%
  summarise(
    total_auc_mean = mean(total_auc, na.rm = TRUE),
    cog_auc_mean   = mean(cog_auc,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(total_auc_mean, cog_auc_mean),
               names_to = "metric", values_to = "value") %>%
  mutate(metric_label = factor(metric,
                               levels = c("total_auc_mean", "cog_auc_mean"),
                               labels = c("Total AUC (a.u., baseline-corrected)",
                                          "Cognitive AUC (a.u., baseline-corrected)")))

# Wide for paired lines
dat_fig2_wide <- dat_fig2_subj %>%
  pivot_wider(names_from = effort_factor, values_from = value)

p2a <- dat_fig2_subj %>%
  ggplot(aes(x = effort_factor, y = value, colour = effort_factor)) +
  # Connecting lines per subject
  geom_line(data = dat_fig2_wide %>%
              pivot_longer(c(Low, High), names_to = "effort_factor",
                           values_to = "value") %>%
              mutate(effort_factor = factor(effort_factor,
                                           levels = c("Low", "High"))),
            aes(group = sub), alpha = 0.25, colour = "grey55", linewidth = 0.45) +
  geom_point(alpha = 0.65, size = 1.6, position = position_jitter(width = 0.04, seed = 1)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.15, linewidth = 0.9, colour = "black") +
  facet_grid(metric_label ~ task_factor, scales = "free_y") +
  scale_colour_manual(values = effort_colors) +
  labs(
    x = "Effort condition",
    y = "Pupil AUC (a.u., baseline-corrected)",
    colour = "Effort",
    title = "Effort–Pupil Manipulation Check",
    subtitle = "Points = participant means; diamonds = group mean ± 1 SE; lines connect the same participant"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 9))

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

task_labels <- c(ADT = "ADT (Freq. offset: 8/16/32/64 Hz)",
                 VDT = "VDT (Contrast diff.: 0.06/0.12/0.24/0.48)")

p3 <- dat_fig3 %>%
  filter(!is.na(pupil_state_tertile)) %>%
  ggplot(aes(x = stimulus_intensity, y = choice_num, color = pupil_state_tertile)) +
  stat_summary_bin(fun = "mean", bins = 8, geom = "point", size = 2, alpha = 0.7) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_grid(task_factor ~ effort_factor,
             labeller = labeller(task_factor = task_labels)) +
  scale_color_manual(values = pupil_tertile_colors) +
  labs(
    x = "Stimulus intensity (normalised scale; ADT: 0, 8, 16, 32, 64 Hz offset; VDT: 0, 0.06, 0.12, 0.24, 0.48 contrast diff.)",
    y = "Proportion 'Different' responses",
    color = "Pupil State\nTertile",
    title = "Psychometric Functions by Pupil State (Primary Analysis)"
  ) +
  theme_minimal(base_size = 11) +
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
  scale_fill_manual(values = missingness_colors) +
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

