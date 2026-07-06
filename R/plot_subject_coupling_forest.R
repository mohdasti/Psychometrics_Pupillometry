# Forest plot: subject-level Δpupil vs ΔPF parameter correlations (Pearson r, 95% CI).

COUPLING_INFLUENCE_PAIR <- list(
  task = "VDT",
  pupil_metric = "Cognitive AUC",
  pf_parameter = "Threshold"
)

plot_subject_coupling_forest <- function(
    cor_df,
    sensitivity_df = NULL,
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
    dplyr::mutate(
      p_text = vapply(.data$p_value, format_coupling_p, character(1L)),
      task = factor(.data$task, levels = TASK_COUPLING_ORDER),
      pf_parameter = factor(.data$pf_parameter, levels = c("Slope", "Threshold")),
      pupil_metric = factor(.data$pupil_metric, levels = c("Cognitive AUC", "Total AUC")),
      highlight_influence = .data$task == COUPLING_INFLUENCE_PAIR$task &
        .data$pupil_metric == COUPLING_INFLUENCE_PAIR$pupil_metric &
        .data$pf_parameter == COUPLING_INFLUENCE_PAIR$pf_parameter
    )

  influence_ann <- NULL
  if (!is.null(sensitivity_df) && nrow(sensitivity_df)) {
    infl_row <- sensitivity_df %>%
      dplyr::filter(
        .data$task == COUPLING_INFLUENCE_PAIR$task,
        .data$pupil_metric == COUPLING_INFLUENCE_PAIR$pupil_metric,
        .data$pf_parameter == COUPLING_INFLUENCE_PAIR$pf_parameter
      )
    if (nrow(infl_row)) {
      infl_row <- infl_row[1, , drop = FALSE]
      if (is.finite(infl_row$r_pearson_excl) && infl_row$n_influential > 0L) {
        influence_ann <- tibble::tibble(
          task = factor(COUPLING_INFLUENCE_PAIR$task, levels = TASK_COUPLING_ORDER),
          pf_parameter = factor(
            COUPLING_INFLUENCE_PAIR$pf_parameter,
            levels = c("Slope", "Threshold")
          ),
          pupil_metric = factor(
            COUPLING_INFLUENCE_PAIR$pupil_metric,
            levels = c("Cognitive AUC", "Total AUC")
          ),
          label_x = x_lim[1] + 0.04,
          label = sprintf(
            "Outlier-sensitive\nr = %.3f excl. %d\n(r = %.3f full)",
            infl_row$r_pearson_excl,
            infl_row$n_influential,
            infl_row$r_pearson
          )
        )
      }
    }
  }

  p_x <- x_lim[2] - 0.01
  shapes <- c("ADT" = 16L, "VDT" = 18L)
  n_adt <- task_n$task_n[task_n$task == "ADT"]
  n_vdt <- task_n$task_n[task_n$task == "VDT"]

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = task, x = correlation, colour = task)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = ref_line_color,
      linewidth = 0.45
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      orientation = "y",
      width = 0.18,
      linewidth = 0.75,
      position = ggplot2::position_dodge(width = 0.35)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = task),
      size = 2.8,
      position = ggplot2::position_dodge(width = 0.35)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = p_x, label = p_text),
      hjust = 1,
      size = 2.8,
      colour = "#374151",
      position = ggplot2::position_dodge(width = 0.35)
    )

  if (any(plot_df$highlight_influence)) {
    p <- p +
      ggplot2::geom_point(
        data = plot_df %>% dplyr::filter(.data$highlight_influence),
        ggplot2::aes(y = task, x = correlation),
        shape = 1L,
        size = 4.2,
        stroke = 0.9,
        colour = "#C62828",
        inherit.aes = FALSE
      )
  }

  if (!is.null(influence_ann)) {
    p <- p +
      ggplot2::geom_label(
        data = influence_ann,
        ggplot2::aes(x = label_x, y = task, label = label),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 1.1,
        size = 2.35,
        colour = "#C62828",
        fill = ggplot2::alpha("white", 0.92),
        linewidth = 0.22,
        label.padding = ggplot2::unit(0.12, "lines"),
        lineheight = 0.9
      )
  }

  subtitle <- sprintf(
    "Pearson r (95%% CI) for effort-evoked \u0394pupil vs \u0394PF; ADT n = %d, VDT n = %d; all CIs cross zero",
    n_adt, n_vdt
  )

  p +
    ggplot2::facet_grid(pf_parameter ~ pupil_metric) +
    ggplot2::scale_colour_manual(values = task_color_map, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, name = NULL) +
    ggplot2::coord_cartesian(xlim = x_lim, clip = "off") +
    ggplot2::scale_x_continuous(
      breaks = seq(-0.4, 0.4, by = 0.2),
      expand = ggplot2::expansion(mult = c(0.02, 0.16))
    ) +
    ggplot2::labs(
      title = "Subject-Level PF\u2013Pupil Coupling",
      subtitle = subtitle,
      x = "correlation r (\u0394pupil vs \u0394PF parameter, 95% CI)",
      y = NULL
    ) +
    theme_ch2(11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 8.5, colour = "#4B5563", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 10, colour = "#1F2937"),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(10, 24, 10, 10)
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
    theme_ch2(11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}
