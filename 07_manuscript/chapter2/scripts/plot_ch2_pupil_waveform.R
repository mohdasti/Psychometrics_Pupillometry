#!/usr/bin/env Rscript
# Chapter 2 pupil waveform — matches modeling-pupil-DDM fig_tepr_timecourse calculation.
#
# Pipeline: subject-level B0-corrected traces -> winsorize -> squeeze anchor ->
#           downsample to 200 time samples -> GAM (k = 10, 95% CI), re-anchored at t = 0.
#
# Data: pupil_waveforms_subject_condition_mean.csv from modeling-pupil-DDM
#       (set DDM_PUPIL_DATA_ROOT if the sibling repo is elsewhere).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(here)
})

repo_root <- here::here()
source(file.path(repo_root, "R", "colors_manuscript.R"))
source(file.path(repo_root, "R", "tepr_waveform_helpers.R"))

chapter2_dir <- file.path(repo_root, "07_manuscript", "chapter2")
figures_dir  <- file.path(chapter2_dir, "output", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
output_file  <- file.path(figures_dir, "waveform.png")

resolve_ddm_root <- function() {
  env <- Sys.getenv("DDM_PUPIL_DATA_ROOT", "")
  if (nzchar(env) && dir.exists(env)) return(normalizePath(env, mustWork = TRUE))
  sibling <- file.path(dirname(repo_root), "modeling-pupil-DDM")
  if (dir.exists(sibling)) return(normalizePath(sibling, mustWork = TRUE))
  stop(
    "Cannot find modeling-pupil-DDM data. Set DDM_PUPIL_DATA_ROOT to that repo path."
  )
}

ddm_root <- resolve_ddm_root()
waveform_candidates <- c(
  file.path(ddm_root, "data/pupil_processed/analysis/pupil_waveforms_subject_condition_mean.csv"),
  file.path(ddm_root, "quick_share_v7/analysis/pupil_waveforms_subject_condition_mean.csv")
)
waveform_path <- waveform_candidates[file.exists(waveform_candidates)][1]
if (is.na(waveform_path)) {
  stop("Subject-level waveform CSV not found under ", ddm_root)
}

trial_path <- file.path(chapter2_dir, "data/processed/ch2_triallevel_pupil.csv")
if (!file.exists(trial_path)) {
  trial_path <- file.path(ddm_root, "data/pupil_processed/analysis_ready/ch3_triallevel.csv")
}

TARGET_ONSET <- 4.35
S1_ONSET     <- 3.75
RESP_START   <- 4.70
B0_WIN       <- c(-0.5, 0.0)
B1_WIN_START <- TARGET_ONSET - 0.5
B1_WIN_END   <- TARGET_ONSET
COG_START    <- TARGET_ONSET + 0.50
COG_END      <- TARGET_ONSET + 1.70
GAM_K        <- 10L
GAM_N        <- 200L

condition_colors <- stats::setNames(unname(cond_colors), gsub("/", " / ", names(cond_colors)))
timeline_bar_colors <- list(
  baseline      = pupil_colors["baseline"],
  total_auc     = pupil_colors["total_auc"],
  cognitive_auc = pupil_colors["tepr"]
)

waveform_data <- read_csv(waveform_path, show_col_types = FALSE)
trials        <- read_csv(trial_path, show_col_types = FALSE)
if (!"t_resp_actual" %in% names(trials) &&
    all(c("t_resp_start_rel", "rt") %in% names(trials))) {
  trials <- trials %>%
    mutate(
      t_resp_actual = if_else(
        !is.na(rt) & is.finite(rt) & rt > 0,
        t_resp_start_rel + rt,
        NA_real_
      )
    )
}

timing <- trials %>%
  filter(
    !(stimulus_intensity == 0 & isOddball == 0),
    stimulus_intensity %in% 1:4 |
      (isOddball == 1L & !is.na(stimulus_intensity))
  ) %>%
  group_by(task) %>%
  summarise(
    med_target = median(t_target_onset_rel, na.rm = TRUE),
    med_resp   = median(t_resp_actual, na.rm = TRUE),
    .groups = "drop"
  )

waveform_ch2 <- waveform_data %>%
  filter(
    task %in% c("ADT", "VDT"),
    condition %in% c("Easy / Low", "Easy / High", "Hard / Low", "Hard / High")
  ) %>%
  filter(!is.na(t_rel), !is.na(mean_pupil_full))

if (nrow(waveform_ch2) == 0) {
  stop("No subject-level waveform rows for Easy/Hard x Low/High conditions.")
}

plot_prep <- prepare_tepr_plot_data(waveform_ch2, value_col = "mean_pupil_full", winsorize = TRUE)

plot_task <- function(task_name) {
  tg <- timing$med_target[timing$task == task_name]
  tr <- timing$med_resp[timing$task == task_name]
  if (length(tg) != 1L || !is.finite(tg)) tg <- TARGET_ONSET
  if (length(tr) != 1L || !is.finite(tr)) tr <- RESP_START + 0.8

  x_end <- max(COG_END + 0.15, tr + 0.15)
  x_range <- c(B0_WIN[1], x_end)

  task_prep <- list(
    subject = plot_prep$subject %>% filter(task == task_name),
    condition = plot_prep$condition %>% filter(task == task_name)
  )
  gam_input <- prepare_tepr_gam_plot_data(
    task_prep,
    x_range = x_range,
    n_timepoints = GAM_N
  )

  gam_curves <- fit_tepr_gam_curves(
    gam_input,
    gam_k = GAM_K,
    x_range = x_range
  )

  y_limits <- tepr_gam_curves_ylimits(gam_curves, x_range = x_range)
  y_lower_limit <- y_limits[1]
  y_upper_limit <- y_limits[2]
  y_range_span  <- y_upper_limit - y_lower_limit
  extra_margin  <- y_range_span * 0.30
  b0_vertical_drop <- y_range_span * 0.045
  y_lower_limit <- y_lower_limit - extra_margin * 2.0 - b0_vertical_drop
  y_upper_limit <- y_upper_limit

  conds <- sort(unique(gam_curves$condition))
  gam_curves$condition <- factor(gam_curves$condition, levels = conds)
  colors_use <- condition_colors[names(condition_colors) %in% conds]
  if (length(colors_use) == 0) {
    colors_use <- stats::setNames(scales::hue_pal()(length(conds)), conds)
  }

  event_label_y <- y_upper_limit - y_range_span * 0.005
  primary_events <- tibble(
    event     = c("Squeeze", "S2/Target onset", paste0("Response\n(median = ", round(tr, 2), "s)")),
    time      = c(0, tg, tr),
    label_x   = c(0, tg, tr - 0.12),
    hjust_val = c(0.5, 0.5, 1.0)
  )
  s1_event <- tibble(
    event = "S1 onset\n(3.75s)",
    time = S1_ONSET,
    label_x = S1_ONSET,
    hjust_val = 0.5
  )

  bar_positions <- tibble(
    label  = c(
      "Pre-trial Baseline (B0)",
      "Total AUC (0 → 4.70s)",
      "Pre-stim Baseline (B1)",
      "Cognitive AUC (4.85 → 6.05s)"
    ),
    xstart = c(B0_WIN[1], 0, B1_WIN_START, COG_START),
    xend   = c(B0_WIN[2], RESP_START, B1_WIN_END, COG_END),
    color  = c(
      timeline_bar_colors$baseline,
      timeline_bar_colors$total_auc,
      timeline_bar_colors$baseline,
      timeline_bar_colors$cognitive_auc
    )
  )
  bar_spacing <- extra_margin / (nrow(bar_positions) + 1)
  bar_positions <- bar_positions %>%
    mutate(
      row_idx = seq_len(n()),
      y = y_lower_limit + bar_spacing * row_idx,
      text_y = y + bar_spacing * 1.2,
      x_label = (xstart + xend) / 2
    ) %>%
    mutate(
      y = if_else(row_idx == 1L, y - b0_vertical_drop, y),
      text_y = if_else(row_idx == 1L, text_y - b0_vertical_drop, text_y)
    ) %>%
    select(-row_idx)

  ggplot(gam_curves, aes(x = t_rel, y = fit, color = condition, fill = condition)) +
    geom_vline(
      data = s1_event, aes(xintercept = time),
      inherit.aes = FALSE, linetype = "dashed", color = "grey65", linewidth = 0.4
    ) +
    geom_text(
      data = s1_event,
      aes(x = label_x, y = event_label_y - y_range_span * 0.08, label = event, hjust = hjust_val),
      inherit.aes = FALSE, size = 3.2, color = "grey55", vjust = 1.1, fontface = "italic"
    ) +
    geom_vline(
      data = primary_events, aes(xintercept = time),
      inherit.aes = FALSE, linetype = "dashed", color = "grey40", linewidth = 0.6
    ) +
    geom_text(
      data = primary_events,
      aes(x = label_x, y = event_label_y, label = event, hjust = hjust_val),
      inherit.aes = FALSE, size = 4.0, color = "grey20", vjust = 1.1, fontface = "bold"
    ) +
    geom_ribbon(aes(ymin = fit_lo, ymax = fit_hi), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1.2, alpha = 0.95) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey70", linewidth = 0.35) +
    geom_segment(
      data = bar_positions,
      aes(x = xstart, xend = xend, y = y, yend = y, color = I(color)),
      inherit.aes = FALSE, linewidth = 2.2, lineend = "round"
    ) +
    geom_text(
      data = bar_positions,
      aes(x = x_label, y = text_y, label = label, color = I(color)),
      inherit.aes = FALSE, size = 3.3, fontface = "bold"
    ) +
    scale_color_manual(values = colors_use, name = "Condition") +
    scale_fill_manual(values = colors_use, name = "Condition") +
    scale_x_continuous(breaks = seq(0, ceiling(x_end), by = 1)) +
    coord_cartesian(xlim = c(B0_WIN[1], x_end), ylim = c(y_lower_limit, y_upper_limit)) +
    labs(
      title = paste0(task_name, ": Full Trial Pupil Waveform Time-Locked to Squeeze Onset"),
      subtitle = sprintf(
        "Winsorized subject means; GAM on %d time samples (95%% CI). B0: %.1fs–%.1fs; S2 median: %.2fs; response median: %.2fs",
        GAM_N, B0_WIN[1], B0_WIN[2], tg, tr
      ),
      x = if (task_name == "VDT") "Time Relative to Squeeze Onset (seconds)" else NULL,
      y = "Isolated Pupil (arbitrary units)",
      color = "Condition",
      fill = "Condition"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey30"),
      axis.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      plot.margin = margin(12, 20, 32, 16)
    )
}

p_adt <- plot_task("ADT")
p_vdt <- plot_task("VDT")

combined <- (p_adt / p_vdt) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.spacing = grid::unit(2.4, "lines")
  )

ggsave(output_file, combined, width = 12, height = 15, dpi = 300)
message("Saved: ", output_file)
