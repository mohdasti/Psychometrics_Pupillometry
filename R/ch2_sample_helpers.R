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
