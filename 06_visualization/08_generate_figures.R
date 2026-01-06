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

# Load path configuration
source(file.path(here(), "config", "paths_config.R"))

# Load data
cat("Loading data for figure generation...\n")
dat_file <- merged_trial_file
dat <- read_csv(dat_file, show_col_types = FALSE)

# Check for quality tier column and create if missing
# The data might have gate_pupil_primary or we need to create quality_primary
if (!"quality_primary" %in% names(dat)) {
  if ("gate_pupil_primary" %in% names(dat)) {
    dat$quality_primary <- dat$gate_pupil_primary
    cat("Using gate_pupil_primary as quality_primary\n")
  } else if ("baseline_quality" %in% names(dat) && "cog_quality" %in% names(dat)) {
    # Create quality_primary based on quality thresholds
    dat$quality_primary <- dat$baseline_quality >= 0.60 & dat$cog_quality >= 0.60
    cat("Created quality_primary from baseline_quality and cog_quality\n")
  } else {
    # If no quality columns exist, set all to TRUE (no filtering)
    dat$quality_primary <- TRUE
    warning("No quality columns found. Setting all trials as quality_primary = TRUE")
  }
}

# Check for pupil state/trait columns and create if missing
# These are computed by the quality tiers script (within-subject centering)
if (!"pupil_cognitive_state" %in% names(dat) || !"pupil_cognitive_trait" %in% names(dat)) {
  if ("cog_auc" %in% names(dat)) {
    cat("Computing pupil state/trait components from cog_auc...\n")
    # Compute trait (subject mean)
    subject_means <- dat %>%
      filter(!is.na(cog_auc)) %>%
      group_by(sub) %>%
      summarise(
        pupil_cognitive_trait = mean(cog_auc, na.rm = TRUE),
        pupil_total_auc_trait = if("total_auc" %in% names(dat)) mean(total_auc, na.rm = TRUE) else NA,
        .groups = "drop"
      )
    
    # Merge back and compute state (deviation from trait)
    dat <- dat %>%
      left_join(subject_means, by = "sub") %>%
      mutate(
        pupil_cognitive_state = ifelse(!is.na(cog_auc) & !is.na(pupil_cognitive_trait),
                                      cog_auc - pupil_cognitive_trait, NA),
        pupil_total_auc_state = if("total_auc" %in% names(dat) && "pupil_total_auc_trait" %in% names(dat)) {
          ifelse(!is.na(total_auc) & !is.na(pupil_total_auc_trait),
                 total_auc - pupil_total_auc_trait, NA)
        } else NA
      )
    cat("Created pupil_cognitive_state and pupil_cognitive_trait\n")
  } else {
    warning("cog_auc not found. Cannot compute pupil state/trait. Some figures will be skipped.")
  }
}

pf_file <- pf_params_file
if (file.exists(pf_file)) {
  pf_params <- read_csv(pf_file, show_col_types = FALSE)
} else {
  pf_params <- NULL
  cat("Note: PF parameters file not found. Some figures will be skipped.\n")
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
  scale_color_manual(values = c("Low" = "#2E86AB", "High" = "#A23B72")) +
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
# FIGURE 2: Effort-Pupil Manipulation Check (Raincloud Plot)
# ============================================================================

cat("\n=== Figure 2: Effort-Pupil Manipulation Check ===\n")

# Check for ggdist (recommended for proper half-violins)
has_ggdist <- requireNamespace("ggdist", quietly = TRUE)
if (!has_ggdist) {
  cat("  Note: Install 'ggdist' for better raincloud plots: install.packages('ggdist')\n")
  cat("  Using fallback violin plots for now.\n")
}

dat_fig2 <- dat %>%
  filter(quality_primary == TRUE) %>%
  mutate(
    effort_factor = factor(effort, levels = c("Low", "High")),
    task_factor = factor(task)
  )

# Combine both Total AUC and Cognitive AUC in one figure
# Filter out non-finite values before plotting
dat_fig2_plot <- dat_fig2 %>%
  select(sub, task_factor, effort_factor, total_auc, cog_auc) %>%
  filter(is.finite(total_auc) | is.finite(cog_auc)) %>%
  pivot_longer(cols = c(total_auc, cog_auc), names_to = "metric", values_to = "value") %>%
  filter(is.finite(value)) %>%
  mutate(metric_label = factor(metric, 
                               levels = c("total_auc", "cog_auc"),
                               labels = c("Total AUC", "Cognitive AUC")))

n_removed_fig2 <- nrow(dat_fig2) - nrow(dat_fig2_plot %>% distinct(sub, task_factor, effort_factor))
if (n_removed_fig2 > 0) {
  cat("  Note: Removed", n_removed_fig2, "rows with non-finite pupil values for plotting\n")
}

# Create raincloud plot with mirrored half-violins
# Use ggdist::stat_slab for proper half-violins with better density estimation
p2a <- ggplot(dat_fig2_plot, aes(x = effort_factor, y = value, fill = effort_factor, color = effort_factor)) +
  # Mirrored half-violins: Low extends LEFT, High extends RIGHT
  {
    if (has_ggdist) {
      list(
        # Low effort: half-violin extending LEFT
        ggdist::stat_slab(
          data = dat_fig2_plot %>% filter(effort_factor == "Low"),
          aes(fill = effort_factor, group = interaction(task_factor, metric_label, effort_factor)),
          side = "left",
          alpha = 0.8,
          width = 0.8,
          justification = 1.0,
          normalize = "panels",  # Normalize within each panel (facet) for better visibility
          trim = FALSE,
          slab_type = "pdf",  # Use probability density function
          adjust = 2.0  # Increase bandwidth adjustment for smoother, more visible distributions
        ),
        # High effort: half-violin extending RIGHT
        ggdist::stat_slab(
          data = dat_fig2_plot %>% filter(effort_factor == "High"),
          aes(fill = effort_factor, group = interaction(task_factor, metric_label, effort_factor)),
          side = "right",
          alpha = 0.8,
          width = 0.8,
          justification = 0.0,
          normalize = "panels",  # Normalize within each panel (facet) for better visibility
          trim = FALSE,
          slab_type = "pdf",  # Use probability density function
          adjust = 2.0  # Increase bandwidth adjustment for smoother, more visible distributions
        )
      )
    } else {
      # Fallback: use geom_violin with manual positioning
      list(
        # Low effort: position to the left
        geom_violin(
          data = dat_fig2_plot %>% filter(effort_factor == "Low"),
          alpha = 0.7,
          trim = FALSE,
          scale = "count",  # Use "count" for better visibility
          width = 0.5,
          position = position_nudge(x = -0.2),
          color = NA
        ),
        # High effort: position to the right
        geom_violin(
          data = dat_fig2_plot %>% filter(effort_factor == "High"),
          alpha = 0.7,
          trim = FALSE,
          scale = "count",  # Use "count" for better visibility
          width = 0.5,
          position = position_nudge(x = 0.2),
          color = NA
        )
      )
    }
  } +
  # Boxplots inside violins
  geom_boxplot(
    width = 0.15,
    alpha = 0.9,
    outlier.shape = NA,
    position = position_dodge(width = 0),
    fill = "grey90",
    color = "grey70",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  # Connecting lines (by subject, for paired data visualization)
  geom_line(
    aes(group = sub),
    alpha = 0.3,
    linewidth = 0.4,
    color = "grey60",
    position = position_dodge(width = 0),
    show.legend = FALSE
  ) +
  # Individual points
  geom_point(
    alpha = 0.5,
    size = 1.5,
    position = position_dodge(width = 0),
    show.legend = FALSE
  ) +
  # Facet by metric and task
  facet_grid(metric_label ~ task_factor, scales = "free_y") +
  # Custom colors
  scale_fill_manual(values = c("Low" = "#2E86AB", "High" = "#A23B72")) +
  scale_color_manual(values = c("Low" = "#2E86AB", "High" = "#A23B72")) +
  # Labels
  labs(
    x = "Effort Condition",
    y = "Pupil Metric (baseline-corrected)",
    fill = "Effort",
    color = "Effort",
    title = "Effort-Pupil Manipulation Check"
  ) +
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_blank(),  # Remove grey background for minimal look
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),  # Remove panel borders for cleaner look
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

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
  ggplot(aes(x = stimulus_intensity, y = choice_num, 
             color = pupil_state_tertile, fill = pupil_state_tertile)) +
  stat_summary_bin(fun = "mean", bins = 8, geom = "point", size = 2, alpha = 0.7) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, alpha = 0.2, linewidth = 1.2) +
  facet_grid(task_factor ~ effort_factor) +
  scale_color_manual(
    values = c("Low" = "#2E86AB", "Medium" = "#F18F01", "High" = "#A23B72"),
    name = "Pupil State\nTertile"
  ) +
  scale_fill_manual(
    values = c("Low" = "#2E86AB", "Medium" = "#F18F01", "High" = "#A23B72"),
    name = "Pupil State\nTertile"
  ) +
  labs(
    x = "Stimulus Intensity",
    y = "Proportion 'Different'",
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
  filter(is.finite(rt)) %>%  # Remove rows with non-finite RT values
  group_by(task_factor, effort_factor, pupil_usable) %>%
  summarise(
    mean_stimulus_intensity = mean(stimulus_intensity, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(is.finite(mean_rt)) %>%  # Remove groups with no valid RT data
  mutate(usable_label = ifelse(pupil_usable, "Usable", "Missing")) %>%
  ggplot(aes(x = effort_factor, y = mean_rt, fill = usable_label)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_wrap(~ task_factor) +
  scale_fill_manual(values = c("Usable" = "#2E7D32", "Missing" = "#1565C0")) +
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

