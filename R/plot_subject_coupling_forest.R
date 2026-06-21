# Forest plot: subject-level Δpupil vs ΔPF parameter correlations (Pearson r, 95% CI).

plot_subject_coupling_forest <- function(
    cor_df,
    task_color_map = NULL,
    ref_line_color = "#9CA3AF",
    x_lim = c(-0.45, 0.45)) {
  if (is.null(task_color_map)) {
    task_color_map <- task_colors
  }
  if (!nrow(cor_df)) {
    stop("No coupling correlation rows to plot.")
  }

  task_n <- cor_df %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(task_n = max(.data$n, na.rm = TRUE), .groups = "drop")

  plot_df <- cor_df %>%
    dplyr::left_join(task_n, by = "task") %>%
    dplyr::mutate(
      y_label = paste0(.data$task, ": ", .data$row_label),
      p_text = vapply(.data$p_value, format_coupling_p, character(1L)),
      task = factor(.data$task, levels = TASK_COUPLING_ORDER)
    ) %>%
    dplyr::arrange(.data$task, .data$pupil_metric, .data$pf_parameter)

  y_levels <- rev(unique(plot_df$y_label))
  plot_df$y_label <- factor(plot_df$y_label, levels = y_levels)

  p_x <- x_lim[2] - 0.01
  shapes <- c("ADT" = 16L, "VDT" = 18L)
  n_adt <- task_n$task_n[task_n$task == "ADT"]
  n_vdt <- task_n$task_n[task_n$task == "VDT"]

  ggplot2::ggplot(plot_df, ggplot2::aes(y = y_label, x = correlation, colour = task)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = ref_line_color,
      linewidth = 0.45
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper, colour = task),
      orientation = "y",
      width = 0.22,
      linewidth = 0.75
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = task, shape = task),
      size = 2.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = y_label, x = p_x, label = p_text),
      hjust = 1,
      size = 3.1,
      colour = "#374151",
      inherit.aes = FALSE,
      data = plot_df
    ) +
    ggplot2::scale_colour_manual(values = task_color_map, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, name = NULL) +
    ggplot2::coord_cartesian(xlim = x_lim, clip = "off") +
    ggplot2::scale_x_continuous(
      breaks = seq(-0.4, 0.4, by = 0.2),
      expand = ggplot2::expansion(mult = c(0.02, 0.14))
    ) +
    ggplot2::labs(
      title = "Subject-Level PF\u2013Pupil Coupling",
      subtitle = sprintf(
        "Pearson r (95%% CI) for effort-evoked \u0394pupil vs \u0394PF parameter; ADT n = %d, VDT n = %d; all CIs cross zero",
        n_adt, n_vdt
      ),
      x = "correlation r (\u0394pupil vs \u0394PF parameter, 95% CI)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 9.5, colour = "#4B5563", hjust = 0),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, colour = "#1F2937"),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(10, 22, 10, 10)
    )
}

plot_subject_coupling_zscatter <- function(
    coupling_df,
    task_color_map = NULL,
    ref_line_color = "#9CA3AF") {
  if (is.null(task_color_map)) {
    task_color_map <- task_colors
  }

  z_df <- prepare_coupling_zscore_data(coupling_df)
  pair_specs <- COUPLING_CORR_SPECS %>%
    dplyr::mutate(
      x_z = paste0("z_", .data$x_col),
      y_z = paste0("z_", .data$y_col)
    )

  z_long <- purrr::pmap_dfr(pair_specs, function(
      pupil_metric, pf_parameter, x_col, y_col, row_label, x_z, y_z) {
    z_df %>%
      dplyr::transmute(
        sub,
        task,
        pupil_metric = pupil_metric,
        pf_parameter = pf_parameter,
        x = .data[[x_z]],
        y = .data[[y_z]]
      )
  })

  ggplot2::ggplot(z_long, ggplot2::aes(x = x, y = y, colour = task, shape = task)) +
    ggplot2::geom_hline(yintercept = 0, colour = ref_line_color, linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = ref_line_color, linewidth = 0.35) +
    ggplot2::geom_point(alpha = 0.65, size = 2.2) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, alpha = 0.15) +
    ggplot2::facet_grid(pf_parameter ~ pupil_metric) +
    ggplot2::scale_colour_manual(values = task_color_map, name = NULL) +
    ggplot2::scale_shape_manual(values = c("ADT" = 16L, "VDT" = 18L), name = NULL) +
    ggplot2::labs(
      title = "Z-Scored Subject-Level Coupling (Appendix)",
      subtitle = "\u0394pupil and \u0394PF parameters standardized within modality; OLS slope equals Pearson r",
      x = "z(\u0394pupil, High \u2212 Low)",
      y = "z(\u0394PF parameter, High \u2212 Low)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}
