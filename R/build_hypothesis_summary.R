# Build Table 6 (hypothesis summary) from the same source objects as Tables 3 and 5.

fmt_p_apa <- function(p) {
  if (length(p) == 0L || is.na(p[[1]])) {
    return("—")
  }
  p <- p[[1]]
  if (p < 0.001) {
    return("< .001")
  }
  sub("^0\\.", ".", sprintf("%.3f", p))
}

fmt_pf_contrast <- function(pf_h1_infer, task, parameter) {
  if (is.null(pf_h1_infer) || nrow(pf_h1_infer) == 0L) {
    return("—")
  }
  row <- pf_h1_infer %>%
    dplyr::filter(.data$Task == task, .data$`PF Parameter` == parameter) %>%
    dplyr::slice(1)
  if (nrow(row) == 0L) {
    return("—")
  }
  sprintf(
    "*t*(%.0f) = %+.3f, *p* = %s",
    row$df,
    row$t,
    fmt_p_apa(row$p)
  )
}

fmt_h1a_stats <- function(pf_h1_infer) {
  adt <- fmt_pf_contrast(pf_h1_infer, "ADT", "Threshold")
  vdt <- fmt_pf_contrast(pf_h1_infer, "VDT", "Threshold")
  if (adt == "—" && vdt == "—") {
    return("—")
  }
  sprintf("ADT: %s; VDT: %s", adt, vdt)
}

fmt_h1b_stats <- function(pf_h1_infer) {
  adt <- fmt_pf_contrast(pf_h1_infer, "ADT", "Slope")
  vdt <- fmt_pf_contrast(pf_h1_infer, "VDT", "Slope")
  if (adt == "—" && vdt == "—") {
    return("—")
  }
  sprintf("ADT: %s; VDT: %s", adt, vdt)
}

extract_lmer_term <- function(effects, term_pattern) {
  if (is.null(effects) || nrow(effects) == 0L) {
    return(NULL)
  }
  row <- effects %>%
    dplyr::filter(grepl(term_pattern, .data$term, fixed = TRUE)) %>%
    dplyr::slice(1)
  if (nrow(row) == 0L) {
    return(NULL)
  }
  row
}

#' Two-sided p from a tidy GLMM row; fallback from |z| only when p.value is missing.
glmm_p_value <- function(row) {
  if (is.null(row) || nrow(row) == 0L) {
    return(NA_real_)
  }
  if ("p.value" %in% names(row) && !is.na(row$p.value)) {
    return(as.numeric(row$p.value))
  }
  stats::pnorm(-abs(row$statistic)) * 2
}

#' H4 coupling summary from @tbl-pf-pupil-correlations (Table 5 source).
#' Returns identical primary-statistic text for Table 6 and Discussion prose.
summarize_h4_coupling_correlations <- function(pf_pupil_correlations) {
  unavailable <- list(
    primary_statistic = "—",
    support_status = "Not supported (correlations unavailable)",
    range_phrase = "correlations unavailable"
  )
  if (is.null(pf_pupil_correlations) || nrow(pf_pupil_correlations) == 0L) {
    return(unavailable)
  }

  r_col <- if ("correlation" %in% names(pf_pupil_correlations)) {
    "correlation"
  } else if ("r" %in% names(pf_pupil_correlations)) {
    "r"
  } else {
    return(unavailable)
  }
  p_col <- if ("p_value" %in% names(pf_pupil_correlations)) {
    "p_value"
  } else if ("p" %in% names(pf_pupil_correlations)) {
    "p"
  } else {
    return(unavailable)
  }

  r_vals <- pf_pupil_correlations[[r_col]]
  p_vals <- pf_pupil_correlations[[p_col]]
  if (!length(r_vals) || !length(p_vals)) {
    return(unavailable)
  }

  r_min <- min(r_vals, na.rm = TRUE)
  r_max <- max(r_vals, na.rm = TRUE)
  p_min <- min(p_vals, na.rm = TRUE)

  primary_statistic <- sprintf(
    "*r* range: %+.3f to %+.3f, smallest *p* = %s",
    r_min,
    r_max,
    fmt_p_apa(p_min)
  )

  list(
    primary_statistic = primary_statistic,
    support_status = sprintf(
      "Not supported (task-specific correlations small in both modalities, smallest *p* = %s)",
      fmt_p_apa(p_min)
    ),
    range_phrase = primary_statistic
  )
}

build_hypothesis_summary_table <- function(
    pf_h1_infer = NULL,
    effort_total_auc_effects = NULL,
    effort_cog_auc_effects = NULL,
    pupil_psychometric_effects = NULL,
    pupil_psychometric_lenient = NULL,
    pf_pupil_correlations = NULL) {
  h1a_stat <- fmt_h1a_stats(pf_h1_infer)
  h1b_stat <- fmt_h1b_stats(pf_h1_infer)

  total_effort <- extract_lmer_term(effort_total_auc_effects, "effort_factorHigh")
  h2a_stat <- if (is.null(total_effort)) {
    "—"
  } else {
    p_val <- if ("p.value" %in% names(total_effort) && !is.na(total_effort$p.value)) {
      total_effort$p.value
    } else {
      stats::pnorm(-abs(total_effort$statistic)) * 2
    }
    sprintf(
      "*t* = %+.3f, *p* = %s (Total AUC)",
      total_effort$statistic,
      fmt_p_apa(p_val)
    )
  }

  cog_effort <- extract_lmer_term(effort_cog_auc_effects, "effort_factorHigh")
  h2b_stat <- if (is.null(cog_effort)) {
    "—"
  } else {
    p_val <- if ("p.value" %in% names(cog_effort) && !is.na(cog_effort$p.value)) {
      cog_effort$p.value
    } else {
      stats::pnorm(-abs(cog_effort$statistic)) * 2
    }
    sprintf(
      "*t* = %+.3f, *p* = %s (Cognitive AUC)",
      cog_effort$statistic,
      fmt_p_apa(p_val)
    )
  }

  int_primary <- extract_lmer_term(
    pupil_psychometric_effects,
    "stimulus_intensity_scaled:pupil_cognitive_state_scaled"
  )
  h3_stat <- if (is.null(int_primary)) {
    "—"
  } else {
    lb <- int_primary$estimate - 1.96 * int_primary$std.error
    ub <- int_primary$estimate + 1.96 * int_primary$std.error
    p_val <- glmm_p_value(int_primary)
    sprintf(
      "beta = %+.3f, 95%% CI [%.3f, %+.3f], *p* = %s",
      int_primary$estimate,
      lb,
      ub,
      fmt_p_apa(p_val)
    )
  }

  int_lenient <- extract_lmer_term(
    pupil_psychometric_lenient,
    "stimulus_intensity_scaled:pupil_cognitive_state_scaled"
  )
  lenient_p <- if (is.null(int_lenient)) {
    NA_real_
  } else {
    glmm_p_value(int_lenient)
  }

  trait <- extract_lmer_term(pupil_psychometric_effects, "pupil_cognitive_trait_scaled")
  h3c_stat <- if (is.null(trait)) {
    "—"
  } else {
    p_val <- glmm_p_value(trait)
    sprintf("beta = %+.3f, *p* = %s", trait$estimate, fmt_p_apa(p_val))
  }
  h3c_support <- if (is.null(trait)) {
    "Supported (pupil trait not significant)"
  } else {
    p_val <- glmm_p_value(trait)
    sprintf("Supported (pupil trait *p* = %s, not significant)", fmt_p_apa(p_val))
  }

  h4 <- summarize_h4_coupling_correlations(pf_pupil_correlations)
  h4_stat <- h4$primary_statistic
  h4_support <- h4$support_status

  lenient_p_txt <- if (is.na(lenient_p)) {
    "—"
  } else {
    fmt_p_apa(lenient_p)
  }

  tibble::tibble(
    Hypothesis = c(
      "H1a: Thresholds higher under High effort",
      "H1b: Slopes shallower under High effort",
      "H2a: Baseline pupil larger under High effort",
      "H2b: Task-evoked pupil larger under High effort",
      "H3a / H3b: Stimulus × pupil-state interaction (directional hypotheses)",
      "H3c: Minimal pupil trait effects",
      "H4: ΔPupil correlated with ΔPF parameters"
    ),
    `Support Status` = c(
      "Partial support (ADT threshold p < .05; VDT threshold ns; @sec-pf-backbone)",
      "Not supported (slopes ns in both modalities; @sec-pf-backbone)",
      paste0(
        "Indirect support with qualification: operationalized as Total AUC (squeeze-epoch arousal) ",
        "rather than dedicated pre-stimulus baseline pupil; Total AUC increased under High effort. ",
        "See @sec-manipulation-check note."
      ),
      "Not supported (Cognitive AUC decreased under High effort)",
      sprintf(
        paste0(
          "Not supported at primary tier (positive point estimate; non-significant); ",
          "lenient tier only reached *p* = %s (not robust; @sec-pupil-quality-tier, Appendix B)"
        ),
        lenient_p_txt
      ),
      h3c_support,
      h4_support
    ),
    `Primary Statistic` = c(
      h1a_stat,
      h1b_stat,
      h2a_stat,
      h2b_stat,
      h3_stat,
      h3c_stat,
      h4_stat
    ),
    `Key Evidence` = c(
      "PF threshold comparison (High vs. Low effort)",
      "PF slope comparison (High vs. Low effort)",
      "Total AUC and Cognitive AUC by effort",
      "Total AUC and Cognitive AUC by effort",
      "Stimulus Intensity × Pupil State interaction",
      "Pupil trait main effect (between-person)",
      "Correlation: Δpupil vs. ΔThreshold, ΔSlope (@tbl-pf-pupil-correlations)"
    )
  )
}
