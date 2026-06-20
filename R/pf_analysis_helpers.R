# Psychometric-function (PF) analysis sample rules — Option A (analysis-specific gates).
# Pupil-QC exclusions apply to pupil/GLMM streams only.
# PF-validity exclusions (converged fits; ADT threshold <= max offset) apply to PF stream only.

ADT_PF_MAX_THRESHOLD_HZ <- 64

#' Row-level PF validity for descriptive/inferential PF analyses.
pf_row_valid <- function(task, threshold, slope, converged) {
  converged %in% TRUE &
    is.finite(threshold) &
    is.finite(slope) &
    !(task == "ADT" & threshold > ADT_PF_MAX_THRESHOLD_HZ)
}

#' Filter PF parameters to the PF analysis stream (not pupil-QC restricted).
filter_pf_analysis <- function(pf) {
  pf %>%
    dplyr::filter(
      pf_row_valid(.data$task, .data$threshold, .data$slope, .data$converged)
    )
}

#' Subjects with valid Low and High PF rows for a task (for within-subject effort LMMs).
pf_effort_lmer_data <- function(pf, task_name, outcome) {
  pf_valid <- filter_pf_analysis(pf)

  pf_valid %>%
    dplyr::filter(
      .data$task == task_name,
      .data$effort %in% c("Low", "High"),
      is.finite(.data[[outcome]])
    ) %>%
    dplyr::group_by(.data$sub) %>%
    dplyr::filter(dplyr::n() == 2L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(effort = factor(.data$effort, levels = c("Low", "High")))
}

#' Within-subject effort LMM on PF threshold or slope (PF-valid sample).
extract_pf_effort_lmer <- function(pf, task_name, outcome) {
  d <- pf_effort_lmer_data(pf, task_name, outcome)
  if (length(unique(d$sub)) < 3L) {
    return(NULL)
  }

  form <- stats::as.formula(paste0(outcome, " ~ effort + (1|sub)"))
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    fit <- tryCatch(
      lmerTest::lmer(form, data = d),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(NULL)
    }
    co <- stats::coef(summary(fit))
    rn <- rownames(co)
    ir <- grep("effortHigh", rn)
    if (!length(ir)) {
      ir <- grep("High", rn)
    }
    if (!length(ir)) {
      return(NULL)
    }
    i <- ir[1]
    tibble::tibble(
      Task = task_name,
      `PF Parameter` = if (outcome == "threshold") "Threshold" else "Slope",
      `Est. (High − Low)` = as.numeric(co[i, "Estimate"]),
      SE = as.numeric(co[i, "Std. Error"]),
      t = as.numeric(co[i, "t value"]),
      df = as.numeric(co[i, "df"]),
      p = as.numeric(co[i, "Pr(>|t|)"]),
      n_sub = length(unique(d$sub))
    )
  } else {
    wide <- d %>%
      dplyr::select(sub, effort, v = dplyr::all_of(outcome)) %>%
      tidyr::pivot_wider(names_from = effort, values_from = v) %>%
      dplyr::filter(is.finite(High), is.finite(Low))
    if (nrow(wide) < 3L) {
      return(NULL)
    }
    diff <- wide$High - wide$Low
    tt <- stats::t.test(diff)
    tibble::tibble(
      Task = task_name,
      `PF Parameter` = if (outcome == "threshold") "Threshold" else "Slope",
      `Est. (High − Low)` = mean(diff),
      SE = stats::sd(diff) / sqrt(length(diff)),
      t = unname(tt$statistic),
      df = unname(tt$parameter),
      p = tt$p.value,
      n_sub = nrow(wide)
    )
  }
}

#' PF-valid participant counts by task (both effort conditions present).
pf_valid_n_by_task <- function(pf) {
  pf_valid <- filter_pf_analysis(pf)
  pf_valid %>%
    dplyr::group_by(.data$task) %>%
    dplyr::summarise(
      n_sub = dplyr::n_distinct(.data$sub),
      .groups = "drop"
    )
}

#' PF-valid N with Low and High effort rows (PF effort LMM / Table 2–3 sample).
pf_valid_both_effort_n_by_task <- function(pf) {
  tasks <- sort(unique(pf$task))
  res <- lapply(tasks, function(task_name) {
    d <- pf_effort_lmer_data(pf, task_name, "threshold")
    tibble::tibble(
      task = task_name,
      n_sub = dplyr::n_distinct(d$sub)
    )
  })
  dplyr::bind_rows(res)
}
