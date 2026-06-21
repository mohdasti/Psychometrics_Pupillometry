# Subject-level PF–pupil coupling: correlations, influence diagnostics, sensitivity.

COUPLING_CORR_SPECS <- tibble::tribble(
  ~pupil_metric,   ~pf_parameter, ~x_col,            ~y_col,            ~row_label,
  "Cognitive AUC", "Slope",       "delta_cog_auc",   "delta_slope",     "Cog AUC \u00d7 slope",
  "Cognitive AUC", "Threshold",   "delta_cog_auc",   "delta_threshold", "Cog AUC \u00d7 threshold",
  "Total AUC",     "Slope",       "delta_total_auc", "delta_slope",     "Total AUC \u00d7 slope",
  "Total AUC",     "Threshold",   "delta_total_auc", "delta_threshold", "Total AUC \u00d7 threshold"
)

TASK_COUPLING_ORDER <- c("ADT", "VDT")

format_coupling_p <- function(p) {
  if (is.na(p)) {
    return("—")
  }
  if (p < 0.001) {
    return("< .001")
  }
  sub(".", "", sprintf("%.3f", p), fixed = TRUE)
}

coupling_pair_data <- function(df, task_label, x_col, y_col) {
  df %>%
    dplyr::filter(.data$task == task_label) %>%
    dplyr::filter(is.finite(.data[[x_col]]), is.finite(.data[[y_col]]))
}

compute_coupling_correlation <- function(
    df,
    task_label,
    pupil_metric,
    pf_parameter,
    x_col,
    y_col,
    row_label = NA_character_,
    method = c("pearson", "spearman")) {
  method <- match.arg(method)
  d <- coupling_pair_data(df, task_label, x_col, y_col)

  empty <- tibble::tibble(
    task = task_label,
    pupil_metric = pupil_metric,
    pf_parameter = pf_parameter,
    row_label = row_label,
    method = method,
    correlation = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    p_value = NA_real_,
    n = nrow(d)
  )

  if (nrow(d) < 3L || stats::sd(d[[x_col]]) == 0 || stats::sd(d[[y_col]]) == 0) {
    return(empty)
  }

  ct <- stats::cor.test(d[[x_col]], d[[y_col]], method = method, exact = FALSE)
  empty$correlation <- unname(ct$estimate)
  empty$p_value <- ct$p.value
  empty$n <- nrow(d)
  if (method == "pearson") {
    empty$ci_lower <- ct$conf.int[1]
    empty$ci_upper <- ct$conf.int[2]
  }
  empty
}

compute_coupling_influence <- function(df, task_label, x_col, y_col) {
  d <- coupling_pair_data(df, task_label, x_col, y_col)
  n <- nrow(d)
  if (n < 4L) {
    return(d %>% dplyr::mutate(
      cooks_d = NA_real_,
      hat = NA_real_,
      dfbeta_slope = NA_real_,
      influential = FALSE
    ))
  }

  fit <- stats::lm(stats::as.formula(paste(y_col, "~", x_col)), data = d)
  cooks <- as.numeric(stats::cooks.distance(fit))
  hat_vals <- as.numeric(stats::hatvalues(fit))
  dfbetas <- as.numeric(stats::dfbetas(fit)[, 2L, drop = TRUE])

  cook_cut <- 4 / n
  hat_cut <- 2 * mean(hat_vals)
  dfbeta_cut <- 2 / sqrt(n)

  d %>%
    dplyr::mutate(
      cooks_d = cooks,
      hat = hat_vals,
      dfbeta_slope = dfbetas,
      influential = cooks > cook_cut |
        hat_vals > hat_cut |
        abs(dfbetas) > dfbeta_cut
    )
}

build_coupling_correlation_summary <- function(coupling_df) {
  tasks <- intersect(TASK_COUPLING_ORDER, sort(unique(coupling_df$task)))
  tidyr::expand_grid(task = tasks, COUPLING_CORR_SPECS) %>%
    purrr::pmap_dfr(function(task, pupil_metric, pf_parameter, x_col, y_col, row_label) {
      compute_coupling_correlation(
        coupling_df,
        task_label = task,
        pupil_metric = pupil_metric,
        pf_parameter = pf_parameter,
        x_col = x_col,
        y_col = y_col,
        row_label = row_label,
        method = "pearson"
      ) %>%
        dplyr::select(-method)
    }) %>%
    dplyr::mutate(
      task = factor(.data$task, levels = TASK_COUPLING_ORDER),
      pf_parameter = factor(.data$pf_parameter, levels = c("Slope", "Threshold")),
      pupil_metric = factor(.data$pupil_metric, levels = c("Cognitive AUC", "Total AUC"))
    ) %>%
    dplyr::arrange(.data$task, .data$pupil_metric, .data$pf_parameter)
}

build_coupling_sensitivity_summary <- function(coupling_df) {
  tasks <- intersect(TASK_COUPLING_ORDER, sort(unique(coupling_df$task)))
  tidyr::expand_grid(task = tasks, COUPLING_CORR_SPECS) %>%
    purrr::pmap_dfr(function(task, pupil_metric, pf_parameter, x_col, y_col, row_label) {
      pearson <- compute_coupling_correlation(
        coupling_df, task, pupil_metric, pf_parameter, x_col, y_col,
        row_label = row_label, method = "pearson"
      )
      spearman <- compute_coupling_correlation(
        coupling_df, task, pupil_metric, pf_parameter, x_col, y_col,
        row_label = row_label, method = "spearman"
      )

      infl <- compute_coupling_influence(coupling_df, task, x_col, y_col)
      flagged <- infl$sub[infl$influential %in% TRUE]
      n_flag <- length(flagged)

      pearson_excl <- if (n_flag > 0L && nrow(infl) - n_flag >= 3L) {
        compute_coupling_correlation(
          infl %>% dplyr::filter(!.data$sub %in% flagged),
          task, pupil_metric, pf_parameter, x_col, y_col,
          row_label = row_label, method = "pearson"
        )
      } else {
        pearson
      }

      tibble::tibble(
        task = task,
        pupil_metric = pupil_metric,
        pf_parameter = pf_parameter,
        row_label = row_label,
        n = pearson$n,
        r_pearson = pearson$correlation,
        ci_lower = pearson$ci_lower,
        ci_upper = pearson$ci_upper,
        p_pearson = pearson$p_value,
        r_spearman = spearman$correlation,
        p_spearman = spearman$p_value,
        n_influential = n_flag,
        flagged_subjects = if (n_flag) paste(sort(flagged), collapse = ", ") else "",
        r_pearson_excl = pearson_excl$correlation,
        p_pearson_excl = pearson_excl$p_value
      )
    }) %>%
    dplyr::mutate(
      task = factor(.data$task, levels = TASK_COUPLING_ORDER),
      pf_parameter = factor(.data$pf_parameter, levels = c("Slope", "Threshold")),
      pupil_metric = factor(.data$pupil_metric, levels = c("Cognitive AUC", "Total AUC"))
    ) %>%
    dplyr::arrange(.data$task, .data$pupil_metric, .data$pf_parameter)
}

build_coupling_influence_audit <- function(coupling_df, pf_params = NULL) {
  tasks <- intersect(TASK_COUPLING_ORDER, sort(unique(coupling_df$task)))
  audit <- tidyr::expand_grid(task = tasks, COUPLING_CORR_SPECS) %>%
    purrr::pmap_dfr(function(task, pupil_metric, pf_parameter, x_col, y_col, row_label) {
      infl <- compute_coupling_influence(coupling_df, task, x_col, y_col)
      infl %>%
        dplyr::filter(.data$influential %in% TRUE) %>%
        dplyr::transmute(
          task = task,
          pupil_metric = pupil_metric,
          pf_parameter = pf_parameter,
          row_label = row_label,
          sub = .data$sub,
          delta_x = .data[[x_col]],
          delta_y = .data[[y_col]],
          cooks_d = .data$cooks_d,
          hat = .data$hat,
          dfbeta_slope = .data$dfbeta_slope
        )
    })

  if (is.null(pf_params) || !nrow(audit)) {
    return(audit)
  }

  pf_wide <- filter_pf_analysis(pf_params) %>%
    dplyr::select(
      sub, task, effort, threshold, slope, converged,
      dplyr::any_of(c("n_trials", "r_squared"))
    ) %>%
    tidyr::pivot_wider(
      names_from = effort,
      values_from = c(threshold, slope, converged, n_trials, r_squared),
      names_sep = "_"
    )

  audit %>%
    dplyr::left_join(pf_wide, by = c("sub", "task"))
}

prepare_coupling_zscore_data <- function(coupling_df) {
  coupling_df %>%
    dplyr::group_by(.data$task) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("delta_"),
        ~ as.numeric(scale(.x)),
        .names = "z_{.col}"
      )
    ) %>%
    dplyr::ungroup()
}

build_subject_coupling_data <- function(dat, pf_params) {
  subject_pupil <- dat %>%
    dplyr::filter(.data$quality_primary == TRUE) %>%
    dplyr::group_by(.data$sub, .data$task, .data$effort) %>%
    dplyr::summarise(
      mean_cog_auc = mean(.data$cog_auc, na.rm = TRUE),
      mean_total_auc = mean(.data$total_auc, na.rm = TRUE),
      .groups = "drop"
    )

  pupil_changes <- subject_pupil %>%
    tidyr::pivot_wider(
      names_from = effort,
      values_from = c(mean_cog_auc, mean_total_auc),
      names_sep = "_"
    ) %>%
    dplyr::mutate(
      delta_cog_auc = .data$mean_cog_auc_High - .data$mean_cog_auc_Low,
      delta_total_auc = .data$mean_total_auc_High - .data$mean_total_auc_Low
    ) %>%
    dplyr::select(sub, task, delta_cog_auc, delta_total_auc)

  pf_changes <- filter_pf_analysis(pf_params) %>%
    dplyr::select(sub, task, effort, threshold, slope) %>%
    tidyr::pivot_wider(
      names_from = effort,
      values_from = c(threshold, slope),
      names_sep = "_"
    ) %>%
    dplyr::mutate(
      delta_threshold = .data$threshold_High - .data$threshold_Low,
      delta_slope = .data$slope_High - .data$slope_Low
    ) %>%
    dplyr::select(sub, task, delta_threshold, delta_slope)

  pupil_changes %>%
    dplyr::inner_join(pf_changes, by = c("sub", "task")) %>%
    dplyr::filter(
      is.finite(.data$delta_cog_auc),
      is.finite(.data$delta_total_auc),
      is.finite(.data$delta_threshold),
      is.finite(.data$delta_slope)
    )
}
