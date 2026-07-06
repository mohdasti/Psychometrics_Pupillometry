# Participant-level effort–pupil manipulation check (raincloud-style paired plot).

prepare_effort_pupil_subject_means <- function(dat) {
  dat %>%
    dplyr::filter(.data$quality_primary == TRUE) %>%
    dplyr::mutate(
      effort_factor = factor(.data$effort, levels = c("Low", "High")),
      task_factor = factor(.data$task)
    ) %>%
    dplyr::group_by(.data$sub, .data$task_factor, .data$effort_factor) %>%
    dplyr::summarise(
      total_auc_mean = mean(.data$total_auc, na.rm = TRUE),
      cog_auc_mean = mean(.data$cog_auc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(total_auc_mean, cog_auc_mean),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::filter(is.finite(.data$value)) %>%
    dplyr::mutate(
      metric_label = factor(
        .data$metric,
        levels = c("total_auc_mean", "cog_auc_mean"),
        labels = c(
          "Total AUC (a.u., baseline-corrected)",
          "Cognitive AUC (a.u., baseline-corrected)"
        )
      )
    )
}

plot_effort_pupil_manipulation <- function(
    subject_means_df,
    effort_colors_map = effort_colors,
    line_color = "#9CA3AF") {
  if (!nrow(subject_means_df)) {
    stop("No participant-level pupil means to plot.")
  }

  line_df <- subject_means_df %>%
    tidyr::pivot_wider(
      names_from = effort_factor,
      values_from = value
    ) %>%
    tidyr::pivot_longer(
      cols = c(Low, High),
      names_to = "effort_factor",
      values_to = "value"
    ) %>%
    dplyr::filter(is.finite(.data$value)) %>%
    dplyr::mutate(
      effort_factor = factor(.data$effort_factor, levels = c("Low", "High"))
    )

  ggplot2::ggplot(
    subject_means_df,
    ggplot2::aes(x = effort_factor, y = value, fill = effort_factor, colour = effort_factor)
  ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(group = sub),
      inherit.aes = TRUE,
      alpha = 0.22,
      colour = line_color,
      linewidth = 0.45
    ) +
    ggplot2::geom_violin(
      trim = TRUE,
      alpha = 0.35,
      colour = NA,
      linewidth = 0,
      scale = "width"
    ) +
    ggplot2::geom_boxplot(
      width = 0.11,
      outlier.shape = NA,
      alpha = 0.55,
      colour = "#374151",
      linewidth = 0.35,
      fill = "white"
    ) +
    ggplot2::geom_point(
      alpha = 0.65,
      size = 1.5,
      position = ggplot2::position_jitter(width = 0.05, seed = 1L)
    ) +
    ggplot2::facet_grid(metric_label ~ task_factor, scales = "free_y") +
    ggplot2::scale_fill_manual(values = effort_colors_map, guide = "none") +
    ggplot2::scale_colour_manual(values = effort_colors_map, guide = "none") +
    ggplot2::labs(
      x = "Effort condition",
      y = "Pupil AUC (a.u., baseline-corrected)",
      title = "Effort\u2013Pupil Manipulation Check",
      subtitle = paste0(
        "Violin, box, and participant means with within-subject lines ",
        "(primary pupil-quality tier)"
      )
    ) +
    theme_ch2(11) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(size = 9),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "#4B5563")
    )
}
