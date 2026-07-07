# TOST equivalence helpers for the primary GLMM interaction (Chapter 2).

CH2_INTERACTION_TERM <- "stimulus_intensity_scaled:pupil_cognitive_state_scaled"
CH2_MOTORBUFFER_INTERACTION_TERM <- "stimulus_intensity_scaled:pupil_cognitive_state_motorbuffer_scaled"
CH2_TOST_SESOI <- 0.10
CH2_TOST_ALPHA <- 0.05
CH2_TOST_Z90 <- stats::qnorm(1 - CH2_TOST_ALPHA)

#' Two one-sided tests for |b| < sesoi (Lakens, 2018; z approximation for GLMM).
tost_regression_z <- function(b, se, sesoi = CH2_TOST_SESOI, alpha = CH2_TOST_ALPHA) {
  z_lo <- (b + sesoi) / se
  z_hi <- (b - sesoi) / se
  p_lo <- stats::pnorm(z_lo, lower.tail = FALSE)
  p_hi <- stats::pnorm(z_hi, lower.tail = TRUE)
  tibble::tibble(
    z_lower = z_lo,
    p_tost_lower = p_lo,
    z_upper = z_hi,
    p_tost_upper = p_hi,
    p_tost = max(p_lo, p_hi),
    equivalent = p_lo < alpha && p_hi < alpha
  )
}

#' Extract interaction TOST summary from a fitted glmer model.
extract_interaction_tost <- function(
    model,
    term = CH2_INTERACTION_TERM,
    sesoi = CH2_TOST_SESOI,
    analysis_id = NA_character_,
    analysis_label = NA_character_) {
  fe <- broom.mixed::tidy(model, effects = "fixed")
  row <- fe[fe$term == term, , drop = FALSE]
  if (!nrow(row)) {
    stop("Interaction term not found: ", term)
  }

  b <- row$estimate[1]
  se <- row$std.error[1]
  p_wald <- row$p.value[1]
  ci90 <- c(b - CH2_TOST_Z90 * se, b + CH2_TOST_Z90 * se)
  n_trials <- nrow(stats::model.frame(model))
  n_sub <- length(unique(stats::model.frame(model)$sub))

  tost <- tost_regression_z(b, se, sesoi = sesoi)
  dplyr::bind_cols(
    tibble::tibble(
      analysis_id = analysis_id,
      analysis_label = analysis_label,
      term = term,
      estimate = b,
      std_error = se,
      p_wald = p_wald,
      ci90_lower = ci90[1],
      ci90_upper = ci90[2],
      n_trials = n_trials,
      n_sub = n_sub,
      sesoi = sesoi,
      inside_sesoi = ci90[1] >= -sesoi && ci90[2] <= sesoi
    ),
    tost
  )
}

#' Build TOST table for lenient, primary, strict, and slow-RT GLMMs.
build_interaction_tost_summary <- function(
    models_dir,
    sesoi = CH2_TOST_SESOI) {
  specs <- tibble::tibble(
    analysis_id = c("lenient", "primary", "strict", "slow_rt", "motorbuffer"),
    file = c(
      "mod_pupil_psychometric_lenient.rds",
      "mod_pupil_psychometric_primary.rds",
      "mod_pupil_psychometric_strict.rds",
      "mod_pupil_psychometric_slow_rt.rds",
      "mod_pupil_psychometric_motorbuffer.rds"
    ),
    analysis_label = c(
      "Lenient tier (validity >= .50)",
      "Primary tier (validity >= .60)",
      "Strict tier (validity >= .70)",
      "Slow-RT subset (RT > 1.5 s)",
      "Motor-buffered window (truncated 150 ms pre-press)"
    ),
    plot_order = c(1L, 2L, 3L, 4L, 5L)
  )

  res <- lapply(seq_len(nrow(specs)), function(i) {
    path <- file.path(models_dir, specs$file[i])
    if (!file.exists(path)) {
      return(NULL)
    }
    mod <- readRDS(path)
    term <- if (specs$analysis_id[i] == "motorbuffer") {
      CH2_MOTORBUFFER_INTERACTION_TERM
    } else {
      CH2_INTERACTION_TERM
    }
    out <- extract_interaction_tost(
      mod,
      term = term,
      sesoi = sesoi,
      analysis_id = specs$analysis_id[i],
      analysis_label = specs$analysis_label[i]
    )
    out$plot_order <- specs$plot_order[i]
    out
  })

  dplyr::bind_rows(res) %>%
    dplyr::arrange(.data$plot_order)
}

format_equiv_p <- function(p) {
  if (is.na(p)) {
    return("—")
  }
  if (p < 0.001) {
    return("< .001")
  }
  sprintf("= %.3f", p)
}

format_equiv_beta <- function(x, digits = 3L) {
  if (is.na(x)) {
    return("—")
  }
  sprintf("%+.3f", x)
}

format_inside_sesoi <- function(inside) {
  if (is.na(inside)) {
    return("—")
  }
  if (isTRUE(inside)) {
    "Yes"
  } else {
    "No"
  }
}
