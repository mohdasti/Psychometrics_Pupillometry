# Motor-buffered cognitive pupil metrics (Chapter 2 Tier-1 sensitivity).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

COG_WIN_START_REL <- 4.85
COG_WIN_END_PRIMARY_REL <- 6.05
TARGET_ONSET_REL <- 4.35
RESP_START_DEFAULT <- 4.70
B0_WIN <- c(-0.5, 0.0)
B1_WIN <- c(-0.5, 0.0)
MOTOR_BUFFER_SEC <- 0.15
MIN_MOTORBUFFER_DURATION_SEC <- 0.6
MIN_BASELINE_SAMPLES <- 10L
MIN_MOTORBUFFER_SAMPLES <- 150L
DEFAULT_PUPIL_PROCESSED_DIR <- "/Users/mohdasti/Documents/LC-BAP/BAP/BAP_Pupillometry/BAP/BAP_processed"

#' Resolve directory containing *_flat.csv pupil files.
resolve_pupil_processed_dir <- function() {
  env <- Sys.getenv("PUPIL_PROCESSED_DIR", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(env)
  }
  config_file <- file.path(here::here(), "config", "data_paths.yaml")
  if (file.exists(config_file) && requireNamespace("yaml", quietly = TRUE)) {
    cfg <- yaml::read_yaml(config_file)
    if (!is.null(cfg$processed_dir) && dir.exists(cfg$processed_dir)) {
      return(cfg$processed_dir)
    }
  }
  if (dir.exists(DEFAULT_PUPIL_PROCESSED_DIR)) {
    return(DEFAULT_PUPIL_PROCESSED_DIR)
  }
  stop(
    "Pupil flat-file directory not found. Set PUPIL_PROCESSED_DIR or config/data_paths.yaml$processed_dir."
  )
}

compute_trapezoid_auc <- function(time, value) {
  valid <- !is.na(time) & !is.na(value) & is.finite(value)
  if (sum(valid) < 2L) {
    return(NA_real_)
  }
  time_clean <- time[valid]
  value_clean <- value[valid]
  ord <- order(time_clean)
  time_clean <- time_clean[ord]
  value_clean <- value_clean[ord]
  n <- length(time_clean)
  if (n < 2L) {
    return(NA_real_)
  }
  dt <- diff(time_clean)
  means <- (value_clean[-n] + value_clean[-1L]) / 2
  sum(dt * means)
}

build_t_rel <- function(n_samples, dt_median = 0.004) {
  if (n_samples < 2L) {
    return(numeric())
  }
  dt_used <- if (is.na(dt_median) || !is.finite(dt_median)) 0.004 else dt_median
  t_rel <- seq(from = -3.0, by = dt_used, length.out = n_samples)
  t_rel_max <- max(t_rel, na.rm = TRUE)
  if (t_rel_max < 8 || t_rel_max > 15) {
    t_rel <- seq(from = -3.0, to = 10.7, length.out = n_samples)
  }
  t_rel
}

infer_time_unit <- function(time_vec) {
  time_diffs <- diff(sort(unique(time_vec)))
  time_diffs <- time_diffs[time_diffs > 0 & is.finite(time_diffs)]
  if (!length(time_diffs)) {
    return(list(unit = "sec", dt_median = 0.004))
  }
  dt_median <- stats::median(time_diffs, na.rm = TRUE)
  if (dt_median < 0.05) {
    list(unit = "sec", dt_median = dt_median)
  } else if (dt_median >= 1 && dt_median <= 10) {
    list(unit = "ms", dt_median = dt_median / 1000)
  } else {
    list(unit = "sec", dt_median = dt_median)
  }
}

#' Motor-buffered mean dilation for one trial waveform.
compute_trial_motorbuffer_metrics <- function(
    pupil_vals,
    t_rel,
    rt_sec,
    t_resp_start_rel = RESP_START_DEFAULT) {
  out <- list(
    cog_mean_motorbuffered = NA_real_,
    cog_auc_motorbuffered = NA_real_,
    cog_win_primary_end_motorbuffered = NA_real_,
    cog_win_motorbuffer_duration = NA_real_,
    cog_win_truncated_by_motor = NA,
    cog_win_motorbuffer_valid = FALSE,
    cog_win_motorbuffer_n_valid = 0L,
    baseline_b0_mean = NA_real_
  )

  if (length(pupil_vals) < 2L || !length(t_rel)) {
    return(out)
  }
  if (is.na(rt_sec) || !is.finite(rt_sec) || rt_sec <= 0) {
    return(out)
  }

  b1_start <- TARGET_ONSET_REL + B1_WIN[1]
  b1_end <- TARGET_ONSET_REL + B1_WIN[2]
  b1_mask <- t_rel >= b1_start & t_rel < b1_end
  b1_pupil <- pupil_vals[b1_mask]
  n_valid_b1 <- sum(!is.na(b1_pupil) & is.finite(b1_pupil))
  if (n_valid_b1 < MIN_BASELINE_SAMPLES) {
    return(out)
  }
  baseline_b0_mean <- mean(b1_pupil[!is.na(b1_pupil) & is.finite(b1_pupil)], na.rm = TRUE)
  pupil_partial <- pupil_vals - baseline_b0_mean

  t_press <- t_resp_start_rel + rt_sec
  win_end <- min(COG_WIN_END_PRIMARY_REL, t_press - MOTOR_BUFFER_SEC)
  win_start <- COG_WIN_START_REL
  duration <- win_end - win_start

  out$baseline_b0_mean <- baseline_b0_mean
  out$cog_win_primary_end_motorbuffered <- win_end
  out$cog_win_motorbuffer_duration <- duration
  out$cog_win_truncated_by_motor <- isTRUE(win_end < COG_WIN_END_PRIMARY_REL - 1e-9)

  if (!is.finite(duration) || duration < MIN_MOTORBUFFER_DURATION_SEC) {
    return(out)
  }

  win_mask <- t_rel >= win_start & t_rel <= win_end
  win_time <- t_rel[win_mask]
  win_pupil <- pupil_partial[win_mask]
  n_valid <- sum(!is.na(win_pupil) & is.finite(win_pupil))
  out$cog_win_motorbuffer_n_valid <- as.integer(n_valid)
  if (n_valid < MIN_MOTORBUFFER_SAMPLES) {
    return(out)
  }

  auc <- compute_trapezoid_auc(win_time, win_pupil)
  if (is.na(auc)) {
    return(out)
  }

  out$cog_auc_motorbuffered <- auc
  out$cog_mean_motorbuffered <- auc / duration
  out$cog_win_motorbuffer_valid <- TRUE
  out
}

process_flat_file_motorbuffer <- function(flat_path, rt_lookup = NULL) {
  df <- data.table::fread(flat_path, showProgress = FALSE, data.table = FALSE)
  if (!nrow(df)) {
    return(tibble::tibble())
  }

  needed <- c("sub", "task", "session_used", "run_used", "trial_index", "time", "pupil")
  miss <- setdiff(needed, names(df))
  if (length(miss)) {
    stop("Flat file missing columns (", basename(flat_path), "): ", paste(miss, collapse = ", "))
  }

  df <- df %>%
    dplyr::mutate(
      sub = as.character(sub),
      task = as.character(task),
      session_used = as.integer(session_used),
      run_used = as.integer(run_used),
      trial_index = as.integer(trial_index),
      time = as.numeric(time),
      pupil = as.numeric(pupil)
    ) %>%
    dplyr::filter(.data$session_used %in% c(2L, 3L))

  if ("trial_in_run_raw" %in% names(df)) {
    df$trial_in_run <- as.integer(df$trial_in_run_raw)
  } else {
    df$trial_in_run <- ((df$trial_index - 1L) %% 30L) + 1L
  }

  df <- df %>% dplyr::arrange(.data$run_used, .data$trial_in_run, .data$time)
  time_info <- infer_time_unit(df$time)
  if (time_info$unit == "ms") {
    df$time <- df$time / 1000
  }

  df %>%
    dplyr::group_by(.data$sub, .data$task, .data$session_used, .data$run_used, .data$trial_in_run) %>%
    dplyr::group_map(function(.x, keys) {
      n_samples <- nrow(.x)
      trial_index <- keys$trial_in_run[[1L]]
      rt_sec <- NA_real_
      t_resp <- RESP_START_DEFAULT
      if (!is.null(rt_lookup)) {
        rt_row <- rt_lookup %>%
          dplyr::filter(
            .data$sub == keys$sub[[1L]],
            .data$task == keys$task[[1L]],
            .data$session_used == keys$session_used[[1L]],
            .data$run_used == keys$run_used[[1L]],
            .data$trial_index == trial_index
          )
        if (nrow(rt_row)) {
          rt_sec <- rt_row$rt[1L]
          if ("t_resp_start_rel" %in% names(rt_row) && is.finite(rt_row$t_resp_start_rel[1L])) {
            t_resp <- rt_row$t_resp_start_rel[1L]
          }
        }
      }

      t_rel <- build_t_rel(n_samples, time_info$dt_median)
      met <- compute_trial_motorbuffer_metrics(.x$pupil, t_rel, rt_sec, t_resp)

      tibble::tibble(
        sub = keys$sub[[1L]],
        task = keys$task[[1L]],
        session_used = keys$session_used[[1L]],
        run_used = keys$run_used[[1L]],
        trial_index = trial_index,
        rt = rt_sec,
        cog_mean_motorbuffered = met$cog_mean_motorbuffered,
        cog_auc_motorbuffered = met$cog_auc_motorbuffered,
        cog_win_primary_end_motorbuffered = met$cog_win_primary_end_motorbuffered,
        cog_win_motorbuffer_duration = met$cog_win_motorbuffer_duration,
        cog_win_truncated_by_motor = met$cog_win_truncated_by_motor,
        cog_win_motorbuffer_valid = met$cog_win_motorbuffer_valid,
        cog_win_motorbuffer_n_valid = met$cog_win_motorbuffer_n_valid
      )
    }) %>%
    dplyr::bind_rows()
}

#' Within-subject center a motor-buffer pupil column.
add_motorbuffer_state_trait <- function(dat, value_col = "cog_mean_motorbuffered") {
  trait_col <- "pupil_cognitive_trait_motorbuffer"
  state_col <- "pupil_cognitive_state_motorbuffer"
  dat <- dat %>%
    dplyr::select(-dplyr::any_of(c(trait_col, state_col)))

  trait <- dat %>%
    dplyr::filter(!is.na(.data[[value_col]])) %>%
    dplyr::group_by(.data$sub) %>%
    dplyr::summarise(
      !!trait_col := mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  dat %>%
    dplyr::left_join(trait, by = "sub") %>%
    dplyr::mutate(
      !!state_col := .data[[value_col]] - .data[[trait_col]]
    )
}

CH2_MOTORBUFFER_INTERACTION_TERM <- "stimulus_intensity_scaled:pupil_cognitive_state_motorbuffer_scaled"

fit_motorbuffer_glmm <- function(dat) {
  md <- dat %>%
    dplyr::filter(
      .data$quality_primary == TRUE,
      .data$cog_win_motorbuffer_valid == TRUE,
      !is.na(.data$pupil_cognitive_state_motorbuffer),
      !is.na(.data$stimulus_intensity),
      !is.na(.data$choice_num)
    ) %>%
    dplyr::mutate(
      effort_factor = factor(.data$effort, levels = c("Low", "High")),
      task_factor = factor(.data$task)
    )

  md <- md %>%
    dplyr::group_by(.data$task_factor) %>%
    dplyr::mutate(stimulus_intensity_scaled = scale(.data$stimulus_intensity)[, 1L]) %>%
    dplyr::ungroup()
  md$pupil_cognitive_state_motorbuffer_scaled <- scale(md$pupil_cognitive_state_motorbuffer)[, 1L]
  md$pupil_cognitive_trait_motorbuffer_scaled <- scale(md$pupil_cognitive_trait_motorbuffer)[, 1L]

  mod <- lme4::glmer(
    choice_num ~ stimulus_intensity_scaled * pupil_cognitive_state_motorbuffer_scaled +
      effort_factor + task_factor +
      pupil_cognitive_trait_motorbuffer_scaled +
      (1 + stimulus_intensity_scaled | sub),
    data = md,
    family = stats::binomial(link = "probit"),
    control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )

  list(model = mod, data = md)
}

summarize_motorbuffer_glmm <- function(fit) {
  fe <- broom.mixed::tidy(fit$model, effects = "fixed")
  int <- fe[fe$term == CH2_MOTORBUFFER_INTERACTION_TERM, , drop = FALSE]
  tibble::tibble(
    n_trials = nrow(fit$data),
    n_sub = dplyr::n_distinct(fit$data$sub),
    interaction_beta = if (nrow(int)) int$estimate[1L] else NA_real_,
    interaction_se = if (nrow(int)) int$std.error[1L] else NA_real_,
    interaction_z = if (nrow(int)) int$statistic[1L] else NA_real_,
    interaction_p = if (nrow(int)) int$p.value[1L] else NA_real_
  )
}
