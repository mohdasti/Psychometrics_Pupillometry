# Chapter 2 analytic sample and response-timing summaries (single source of truth).

#' Primary pupil-quality tier (does not require complete GLMM covariates).
filter_quality_primary <- function(dat) {
  dat %>% dplyr::filter(.data$quality_primary == TRUE)
}

#' Primary GLMM analysis sample (matches 06_pupil_psychometric_coupling.R).
filter_glmm_primary <- function(dat) {
  dat %>%
    dplyr::filter(
      .data$quality_primary == TRUE,
      !is.na(.data$pupil_cognitive_state),
      !is.na(.data$stimulus_intensity),
      !is.na(.data$choice_num)
    )
}

#' Trial and participant counts by task for the quality-primary tier.
summarize_quality_primary_trials <- function(dat) {
  filter_quality_primary(dat) %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_sub = dplyr::n_distinct(.data$sub),
      .groups = "drop"
    )
}

#' GLMM primary-sample counts (complete cases on pupil, intensity, choice).
summarize_glmm_primary_sample <- function(dat) {
  md <- filter_glmm_primary(dat)
  by_task <- md %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_sub = dplyr::n_distinct(.data$sub),
      .groups = "drop"
    )
  n_sub <- dplyr::n_distinct(md$sub)
  n_trials <- nrow(md)
  list(
    n_trials = n_trials,
    n_sub = n_sub,
    mean_trials_per_sub = if (n_sub > 0L) n_trials / n_sub else NA_real_,
    by_task = by_task
  )
}

#' Median RT (response-screen locked) and press time (squeeze locked).
summarize_response_timing <- function(dat, use_glmm_primary = TRUE) {
  md <- if (isTRUE(use_glmm_primary)) {
    filter_glmm_primary(dat)
  } else {
    filter_quality_primary(dat)
  }

  if (!all(c("rt", "t_resp_start_rel") %in% names(md))) {
    return(list(
      median_rt = NA_real_,
      median_press = NA_real_,
      by_task = tibble::tibble()
    ))
  }

  md <- md %>%
    dplyr::mutate(
      press_time = .data$t_resp_start_rel + .data$rt,
      rt_ok = is.finite(.data$rt) & .data$rt > 0,
      press_ok = is.finite(.data$press_time)
    )

  by_task <- md %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(
      median_rt = stats::median(.data$rt[.data$rt_ok], na.rm = TRUE),
      median_press = stats::median(.data$press_time[.data$press_ok], na.rm = TRUE),
      .groups = "drop"
    )

  list(
    median_rt = stats::median(md$rt[md$rt_ok], na.rm = TRUE),
    median_press = stats::median(md$press_time[md$press_ok], na.rm = TRUE),
    by_task = by_task
  )
}

format_rt_s <- function(x, digits = 2L) {
  if (is.na(x) || !is.finite(x)) {
    return("—")
  }
  sprintf(paste0("%.", digits, "f"), x)
}

#' Inline text: "ADT x.xx s, VDT y.yy s" for a timing column.
format_task_timing <- function(timing, col, digits = 2L) {
  if (is.null(timing) || !nrow(timing$by_task)) {
    return("—")
  }
  bt <- timing$by_task
  adt <- bt[[col]][match("ADT", bt$task)]
  vdt <- bt[[col]][match("VDT", bt$task)]
  sprintf(
    "ADT %s s, VDT %s s",
    format_rt_s(adt, digits),
    format_rt_s(vdt, digits)
  )
}

CH2_RECRUITED_N <- c(ADT = 64L, VDT = 65L)

#' Subject-level PF–pupil coupling N (both-effort pupil + PF-valid deltas).
summarize_coupling_sample <- function(dat, pf) {
  if (is.null(dat) || is.null(pf) || !nrow(pf)) {
    return(tibble::tibble(task = character(), n_sub = integer()))
  }

  pupil_changes <- dat %>%
    dplyr::filter(.data$quality_primary == TRUE) %>%
    dplyr::group_by(.data$sub, .data$task, .data$effort) %>%
    dplyr::summarise(
      mean_cog_auc = mean(.data$cog_auc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$effort,
      values_from = .data$mean_cog_auc,
      names_prefix = "cog_"
    ) %>%
    dplyr::mutate(
      delta_cog_auc = .data$cog_High - .data$cog_Low
    )

  pf_changes <- filter_pf_analysis(pf) %>%
    dplyr::select(.data$sub, .data$task, .data$effort, .data$threshold, .data$slope) %>%
    tidyr::pivot_wider(
      names_from = .data$effort,
      values_from = c(.data$threshold, .data$slope),
      names_sep = "_"
    ) %>%
    dplyr::mutate(
      delta_threshold = .data$threshold_High - .data$threshold_Low,
      delta_slope = .data$slope_High - .data$slope_Low
    )

  pupil_changes %>%
    dplyr::filter(
      is.finite(.data$delta_cog_auc),
      is.finite(.data$cog_High),
      is.finite(.data$cog_Low)
    ) %>%
    dplyr::inner_join(
      pf_changes %>%
        dplyr::filter(
          is.finite(.data$delta_threshold),
          is.finite(.data$delta_slope)
        ),
      by = c("sub", "task")
    ) %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(n_sub = dplyr::n_distinct(.data$sub), .groups = "drop")
}

#' All participant-flow counts for the CONSORT-style diagram and Table 1.
summarize_participant_flow <- function(
    dat,
    pf,
    recruited = CH2_RECRUITED_N,
    pupil_qc_final = c(ADT = 54L, VDT = 56L),
    adt_pf_threshold_excluded = 3L,
    adt_pupil_qc_only_excluded = 7L) {
  pf_valid <- if (!is.null(pf)) pf_valid_both_effort_n_by_task(pf) else NULL
  quality <- if (!is.null(dat)) summarize_quality_primary_trials(dat) else NULL
  glmm <- if (!is.null(dat)) summarize_glmm_primary_sample(dat) else NULL
  coupling <- if (!is.null(dat) && !is.null(pf)) {
    summarize_coupling_sample(dat, pf)
  } else {
    NULL
  }

  pf_n <- stats::setNames(
    pf_valid$n_sub[match(c("ADT", "VDT"), pf_valid$task)],
    c("ADT", "VDT")
  )
  if (any(is.na(pf_n))) {
    pf_n <- c(ADT = recruited["ADT"] - adt_pf_threshold_excluded, VDT = recruited["VDT"])
  }

  list(
    recruited = recruited,
    pf_valid_both_effort = pf_n,
    pf_threshold_excluded = c(ADT = adt_pf_threshold_excluded, VDT = 0L),
    pupil_qc_final = pupil_qc_final,
    pupil_qc_excluded = recruited - pupil_qc_final,
    pupil_qc_only_excluded = c(
      ADT = adt_pupil_qc_only_excluded,
      VDT = as.integer(recruited["VDT"] - pupil_qc_final["VDT"])
    ),
    primary_trials = quality,
    glmm = glmm,
    coupling = coupling
  )
}

format_flow_n <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
