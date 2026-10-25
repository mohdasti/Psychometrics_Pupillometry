# Chapter 2 Phase 2b coding checks.
# Catch trials: x_c is 0. Levels 1-4: x_c is -1.5, -0.5, 0.5, 1.5.
# Do not code x_c as (stim_level_index - 1) - 1.5 on every row.

code_trials <- function(df) {
  if (!data.table::is.data.table(df)) {
    out <- data.table::as.data.table(df)
  } else {
    out <- data.table::copy(df)
  }
  need <- c("resp_is_diff", "stim_level_index", "is_high_grip", "task", "subject_id")
  missing <- need[!need %in% names(out)]
  if (length(missing)) stop("code_trials missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out[, diff := as.integer(resp_is_diff)]
  out[, catch := as.integer(stim_level_index == 0L)]
  out[, off_idx := NA_real_]
  out[catch == 0L, off_idx := as.numeric(stim_level_index) - 1]
  out[, x_c := ifelse(catch == 1L, 0, off_idx - 1.5)]
  out[, effort_c := ifelse(is_high_grip, 0.5, -0.5)]
  out[, task_c := ifelse(task == "VDT", 0.5, -0.5)]
  out[, subject := factor(subject_id)]
  out[]
}

make_coding_fixture <- function() {
  data.table::data.table(
    subject_id = "S1",
    task = rep(c("ADT", "VDT"), each = 5),
    stim_level_index = rep(0:4, times = 2),
    is_high_grip = rep(c(TRUE, FALSE), length.out = 10),
    resp_is_diff = rep(c(TRUE, FALSE), length.out = 10),
    stim_offset_abs = c(0, 8, 16, 32, 64, 0, 0.06, 0.12, 0.24, 0.48)
  )
}

assert_ch2_coding <- function(df) {
  if (!data.table::is.data.table(df)) df <- data.table::as.data.table(df)
  need <- c("stim_level_index", "catch", "x_c", "off_idx", "effort_c", "task_c",
            "task", "is_high_grip", "resp_is_diff", "diff", "stim_offset_abs")
  missing <- need[!need %in% names(df)]
  if (length(missing)) stop("coding test missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (nrow(df) < 1L) stop("coding test received 0 rows", call. = FALSE)

  xc_expected <- c(-1.5, -0.5, 0.5, 1.5)
  for (lv in 0:4) {
    rows <- df[stim_level_index == lv]
    if (nrow(rows) < 1L) stop("coding test has 0 rows at stim_level_index ", lv, call. = FALSE)
    if (lv == 0L) {
      if (any(rows$catch != 1L)) stop("level 0 catch is not 1", call. = FALSE)
      if (any(rows$x_c != 0)) stop("level 0 x_c is not 0", call. = FALSE)
      if (any(!is.na(rows$off_idx))) stop("level 0 off_idx is not NA", call. = FALSE)
    } else {
      if (any(rows$catch != 0L)) stop("level ", lv, " catch is not 0", call. = FALSE)
      if (any(abs(rows$x_c - xc_expected[[lv]]) > 0)) {
        stop("level ", lv, " x_c is not ", xc_expected[[lv]], call. = FALSE)
      }
      if (any(rows$off_idx != (lv - 1))) stop("level ", lv, " off_idx is not ", lv - 1L, call. = FALSE)
    }
  }

  rejected_on_catch <- df[stim_level_index == 0L, (stim_level_index - 1) - 1.5]
  if (any(df[stim_level_index == 0L, x_c] == rejected_on_catch)) {
    stop("level 0 x_c matched (level - 1 - 1.5)", call. = FALSE)
  }

  if (any(!df$effort_c %in% c(-0.5, 0.5))) stop("effort_c is not only -0.5 or 0.5", call. = FALSE)
  if (any(!df$task_c %in% c(-0.5, 0.5))) stop("task_c is not only -0.5 or 0.5", call. = FALSE)
  if (any(df[is_high_grip %in% TRUE, effort_c] != 0.5)) stop("High grip effort_c is not 0.5", call. = FALSE)
  if (any(df[is_high_grip %in% FALSE, effort_c] != -0.5)) stop("Low grip effort_c is not -0.5", call. = FALSE)
  if (any(df[task == "VDT", task_c] != 0.5)) stop("VDT task_c is not 0.5", call. = FALSE)
  if (any(df[task == "ADT", task_c] != -0.5)) stop("ADT task_c is not -0.5", call. = FALSE)
  if (any(df[resp_is_diff %in% TRUE, diff] != 1L)) stop("resp_is_diff TRUE is not diff 1", call. = FALSE)
  if (any(df[resp_is_diff %in% FALSE, diff] != 0L)) stop("resp_is_diff FALSE is not diff 0", call. = FALSE)

  expected_off <- list(ADT = c(8, 16, 32, 64), VDT = c(0.06, 0.12, 0.24, 0.48))
  for (tk in names(expected_off)) {
    sub <- df[task == tk]
    if (nrow(sub) < 1L) stop("coding test has 0 rows for task ", tk, call. = FALSE)
    got <- numeric(4)
    for (lv in 1:4) {
      offs <- unique(sub[stim_level_index == lv, stim_offset_abs])
      if (length(offs) != 1L) {
        stop("task ", tk, " level ", lv, " has ", length(offs), " physical offsets", call. = FALSE)
      }
      got[[lv]] <- offs[[1L]]
    }
    if (any(abs(got - expected_off[[tk]]) > 1e-8)) {
      stop("task ", tk, " offsets ", paste(got, collapse = ","),
           " expected ", paste(expected_off[[tk]], collapse = ","), call. = FALSE)
    }
    ratios <- got[-1L] / got[-length(got)]
    if (any(abs(ratios - 2) > 1e-8)) {
      stop("task ", tk, " offset ratios ", paste(ratios, collapse = ","), call. = FALSE)
    }
  }
  invisible(TRUE)
}

.ch2_coding_launched <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  files <- sub("^--file=", "", args[grepl("^--file=", args)])
  any(grepl("test_ch2_coding\\.R$", files))
}

if (isTRUE(.ch2_coding_launched())) {
  assert_ch2_coding(code_trials(make_coding_fixture()))
  cat("PASS\n")
}
