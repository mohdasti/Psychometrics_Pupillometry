# Forest plot: intensity x pupil-state interaction vs TOST equivalence bounds.

EQUIV_VERDICT_COLORS <- c(
  "Equivalent" = "#2E7D32",
  "Not equivalent" = "#C62828"
)

plot_equivalence_forest <- function(
    tost_df,
    sesoi = CH2_TOST_SESOI,
    equiv_fill = "#E5E7EB",
    ref_line_color = "#9CA3AF",
    verdict_colors = EQUIV_VERDICT_COLORS) {
  if (!nrow(tost_df)) {
    stop("No TOST rows to plot.")
  }

  plot_df <- tost_df %>%
    dplyr::arrange(.data$plot_order) %>%
    dplyr::mutate(
      verdict = dplyr::if_else(
        .data$equivalent %in% TRUE,
        "Equivalent",
        "Not equivalent"
      ),
      verdict = factor(.data$verdict, levels = names(verdict_colors)),
      y_label = .data$analysis_label,
      y_label = factor(.data$y_label, levels = rev(unique(.data$y_label))),
      right_label = paste0(
        format(.data$n_trials, big.mark = ","),
        " trials; ",
        format(.data$n_sub, big.mark = ","),
        " participants<br>Wald <i>p</i> ",
        vapply(.data$p_wald, format_equiv_p, character(1L))
      )
    )

  x_min <- min(c(-sesoi - 0.05, plot_df$ci90_lower, plot_df$estimate), na.rm = TRUE)
  x_max <- max(c(sesoi + 0.05, plot_df$ci90_upper, plot_df$estimate), na.rm = TRUE)
  x_pad <- 0.04
  label_x <- x_max + x_pad + 0.14
  x_lim <- c(x_min - x_pad, label_x + 0.08)

  ggplot2::ggplot(plot_df, ggplot2::aes(y = y_label, x = estimate, colour = verdict)) +
    ggplot2::annotate(
      "rect",
      xmin = -sesoi,
      xmax = sesoi,
      ymin = -Inf,
      ymax = Inf,
      fill = equiv_fill,
      alpha = 0.85
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = ref_line_color, linewidth = 0.45) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci90_lower, xmax = ci90_upper),
      orientation = "y",
      width = 0.22,
      linewidth = 0.75
    ) +
    ggplot2::geom_point(size = 2.8) +
    ggtext::geom_richtext(
      ggplot2::aes(x = label_x, label = right_label),
      hjust = 0,
      size = 2.85,
      colour = "#374151",
      lineheight = 0.95,
      fill = NA,
      label.color = NA
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = length(levels(plot_df$y_label)) + 0.55,
      label = "equivalence region",
      size = 3.1,
      colour = "#6B7280"
    ) +
    ggplot2::annotate(
      "text",
      x = -sesoi,
      y = length(levels(plot_df$y_label)) + 0.55,
      label = sprintf("-%.2f", sesoi),
      size = 2.8,
      colour = "#6B7280",
      hjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = sesoi,
      y = length(levels(plot_df$y_label)) + 0.55,
      label = sprintf("+%.2f", sesoi),
      size = 2.8,
      colour = "#6B7280",
      hjust = 0.5
    ) +
    ggplot2::coord_cartesian(xlim = x_lim, clip = "off") +
    ggplot2::scale_colour_manual(values = verdict_colors, name = "TOST verdict") +
    ggplot2::scale_x_continuous(
      breaks = seq(floor(x_min * 20) / 20, ceiling(x_max * 20) / 20, by = 0.05),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::labs(
      title = "TOST Equivalence: Intensity \u00d7 Pupil-State Interaction",
      subtitle = sprintf(
        "90%% CI vs pre-specified SESOI (\u00b1%.2f probit units); sample size and Wald <i>p</i> at right",
        sesoi
      ),
      x = "intensity \u00d7 pupil-state coefficient (probit)",
      y = NULL
    ) +
    theme_ch2(11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 9.5, colour = "#4B5563", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 10, colour = "#1F2937"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(10, 110, 10, 10)
    )
}
