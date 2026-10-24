#!/usr/bin/env Rscript
# Chapter 2 Phase 2a.2. Rebuild pupil features on the fixed 250 Hz axis.
# Sample k is at t = -3 + 0.004 * (k - 1) s. Squeeze onset is sample 751.
# The retired whole-file interval (file_dt) is not called. A trial is accepted
# only when its own median increment is within 1e-6 of 0.004 and, for December
# 21, its sample count is 3425 or 3426.
#
# Feature definitions, keys, windows, gates, and waveform binning are the
# Phase 2a definitions, evaluated on this axis.
# compute_auc and window_mean are the Chapter 3 functions
# (modeling-pupil-DDM/scripts/r8/B_rebuild_pupil.R lines 25-50 and 76-93).
# Waveform binning follows scripts/r8/A_alignment.R: GRID is seq(-1, 8, by = 0.02).
# No model is fitted.

options(warn = 1, scipen = 999)
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) stop("Usage: Rscript ch2_rebuild_250hz.R RUN_ID")
RUN <- args[[1L]]

ca <- commandArgs(trailingOnly = FALSE)
fa <- sub("^--file=", "", ca[grepl("^--file=", ca)])
if (!length(fa)) stop("Cannot locate this script via Rscript --file=")
SCRIPT <- normalizePath(fa[[1L]])

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
DDM <- "/Users/mohdasti/Documents/GitHub/modeling-pupil-DDM"
FLAT <- "/Users/mohdasti/Documents/LC-BAP/BAP/BAP_Pupillometry/BAP/BAP_processed"
FLAT25 <- file.path(FLAT, "build_20251225_154443")
PFSHARE <- file.path(CH2, "data/external/lc_psychometric_share")
DATA <- file.path(CH2, "data/analysis_ready/ch2_r2", RUN)
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2a2", RUN)
R1_TRIALS <- file.path(CH2, "data/analysis_ready/ch2_r1/ch2r1_20261024_081618/ch2_pupil_trials_r1.csv")
P4_R1 <- file.path(CH2, "output/diagnostics/ch2_phase2a/ch2r1_20261024_081618/P4_gate_flow.csv")
P2_REFUSED <- file.path(CH2, "output/diagnostics/ch2_phase2a/ch2r1_20261024_081618/P2_refused_summary.csv")
HELPER <- file.path(CH2, "R/trial_time_axis_250hz.R")
TEST <- file.path(CH2, "tests/test_trial_time_axis_250hz.R")
OLD_AXIS <- file.path(CH2, "R/trial_time_axis.R")
DDM_HEAD_EXPECTED <- "c7966311bbd18a8bbd04bf6fce029115749634b4"
ID <- c("subject_id", "task", "session", "run", "trial")

dir.create(DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIAG, "checkpoints", "dec21"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIAG, "checkpoints", "dec25"), recursive = TRUE, showWarnings = FALSE)
setwd(CH2)

LOG <- file.path(DIAG, "run_log.txt")
log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
  flush(stdout())
}

package_zip <- function() {
  zip_path <- file.path(CH2, "output/diagnostics/ch2_phase2a2", paste0("ch2_phase2a2_", RUN, ".zip"))
  if (file.exists(zip_path)) unlink(zip_path)
  stage <- file.path(DIAG, "zip_stage")
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  copy_one <- function(src, dest_name = basename(src)) {
    if (file.exists(src) && !dir.exists(src)) {
      file.copy(src, file.path(stage, dest_name), overwrite = TRUE)
    }
  }
  diag_files <- list.files(DIAG, full.names = TRUE, recursive = FALSE)
  for (f in diag_files) copy_one(f)
  for (bn in c(
    "ch2_pupil_trials_r2.csv",
    "ch2_waveforms_subject_condition_r2.csv",
    "ch2_pupil_trials_dec25_r2.csv",
    "ch3_compatible_pupil_r2.csv",
    "ch3_compatible_gate_counts_r2.csv"
  )) copy_one(file.path(DATA, bn))
  copy_one(SCRIPT, "ch2_rebuild_250hz.R")
  copy_one(HELPER, "trial_time_axis_250hz.R")
  copy_one(TEST, "test_trial_time_axis_250hz.R")
  runlog <- file.path(DIAG, "run.log")
  copy_one(LOG, "run_log.txt")
  copy_one(runlog, "run.log")
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(stage)
  utils::zip(zip_path, list.files("."))
  log_line("Zip ", zip_path, " bytes ", file.info(zip_path)$size)
  zip_path
}

stop_md <- function(part, step, detail, doing, fatal = TRUE) {
  path <- file.path(DIAG, sprintf("STOPPED_%s.md", part))
  writeLines(c(
    sprintf("# STOPPED_%s", part), "",
    paste("Part:", part), paste("Step:", step), paste("Fatal:", fatal), "",
    "## Error or count", "", detail, "",
    "## What was being computed", "", doing
  ), path)
  log_line("WROTE ", path, if (fatal) " FATAL" else " continue")
  report <- file.path(DIAG, "report.md")
  if (fatal && !file.exists(report)) {
    writeLines(c(
      "# Chapter 2 Phase 2a.2", "",
      paste("Part", part, "stopped the run."), "",
      detail, "",
      "Ready for Phase 2b? NO"
    ), report)
  }
  if (fatal) {
    try(package_zip(), silent = TRUE)
    quit(status = 2L)
  }
  invisible(path)
}

# --- Chapter 3 feature functions (verbatim) ---------------------------------

compute_auc <- function(time, value, gap_max_ms = 250) {
  valid <- is.finite(time) & is.finite(value)
  if (sum(valid) < 2L) return(NA_real_)
  time <- time[valid]
  value <- value[valid]
  ord <- order(time)
  time <- time[ord]
  value <- value[ord]
  n <- length(time)
  dt <- diff(time)
  gap <- dt > gap_max_ms / 1000
  if (!any(gap)) {
    return(sum(dt * (value[-n] + value[-1]) / 2))
  }
  seg <- cumsum(c(0, gap)) + 1L
  auc <- 0
  for (s in unique(seg)) {
    m <- seg == s
    if (sum(m) < 2L) next
    ts <- time[m]
    ys <- value[m]
    d <- diff(ts)
    auc <- auc + sum(d * (ys[-length(ys)] + ys[-1]) / 2)
  }
  auc
}

window_mean <- function(t, pupil, b0, start, end) {
  in_win <- t >= start & t < end
  n_expected <- sum(in_win)
  if (!is.finite(b0)) {
    return(list(mean = NA_real_, prop = NA_real_, n_valid = 0L, n_expected = n_expected, status = "no_baseline"))
  }
  if (n_expected < 2L) {
    return(list(mean = NA_real_, prop = NA_real_, n_valid = 0L, n_expected = n_expected, status = "window_outside_timecourse"))
  }
  y <- pupil[in_win]
  n_valid <- sum(is.finite(y))
  prop <- n_valid / n_expected
  if (prop < 0.50) {
    return(list(mean = NA_real_, prop = prop, n_valid = as.integer(n_valid), n_expected = n_expected, status = "below_coverage"))
  }
  list(mean = mean(y[is.finite(y)]) - b0, prop = prop, n_valid = as.integer(n_valid),
       n_expected = n_expected, status = "ok")
}

GRID <- seq(-1, 8, by = 0.02)
new_wave <- function() list(sums = numeric(length(GRID)), counts = integer(length(GRID)), n_trials = 0L)
add_trial <- function(t, pupil, wave) {
  ok <- is.finite(t) & is.finite(pupil)
  in_b0 <- ok & t >= -0.5 & t < 0
  if (sum(in_b0) < 10L) return(wave)
  y <- pupil[ok] - mean(pupil[in_b0])
  tt <- t[ok]
  idx <- findInterval(tt, GRID - 0.01)
  keep <- idx >= 1L & idx <= length(GRID)
  if (!any(keep)) return(wave)
  add <- rowsum(y[keep], idx[keep], reorder = FALSE)
  pos <- as.integer(rownames(add))
  wave$sums[pos] <- wave$sums[pos] + add[, 1]
  wave$counts[pos] <- wave$counts[pos] + tabulate(idx[keep], nbins = length(GRID))[pos]
  wave$n_trials <- wave$n_trials + 1L
  wave
}

as_true <- function(x) {
  if (is.logical(x)) return(x)
  toupper(trimws(as.character(x))) %in% c("TRUE", "T", "1")
}

win_stats <- function(t, pupil, lo, hi, right_open = FALSE) {
  inn <- if (right_open) t >= lo & t < hi else t >= lo & t <= hi
  if (!any(inn)) return(list(n = 0L, prop = NA_real_, vals = numeric()))
  vals <- pupil[inn]
  list(n = as.integer(sum(is.finite(vals))), prop = mean(is.finite(vals)), vals = vals)
}

empty_feat <- function(n = 1L) {
  data.table(
    matched = logical(n), time_rule = NA_character_, refuse_reason = NA_character_,
    refuse_class = NA_character_, axis_error = NA_character_,
    n = NA_integer_, trial_median_dt = NA_real_, dt_used = NA_real_,
    implied_end = NA_real_, end_250hz = NA_real_, native_span = NA_real_,
    seg_start_used = NA_real_, seg_end_used = NA_real_,
    n_seg_start = NA_integer_, n_seg_end = NA_integer_,
    b0_mean = NA_real_, n_b0 = NA_integer_, b0_quality = NA_real_,
    b1_mean = NA_real_, n_b1 = NA_integer_, b1_quality = NA_real_,
    tepr_auc = NA_real_, tepr_quality = NA_real_, tepr_mean_465_765 = NA_real_,
    tepr_mean_485_605 = NA_real_, q_485_605 = NA_real_,
    grip_mean = NA_real_, grip_quality = NA_real_,
    total_auc_b0 = NA_real_, total_quality = NA_real_,
    auc_b0 = NA_real_,
    generation = NA_character_,
    source_file = NA_character_, source_md5 = NA_character_
  )
}

reason_class <- function(x) {
  out <- rep("other", length(x))
  out[grepl("^median dt", x)] <- "median_dt_not_0.004"
  out[grepl("^n =", x)] <- "n_not_3425_or_3426"
  out[grepl("^dec25 segment", x)] <- "dec25_segment"
  out[is.na(x)] <- NA_character_
  out
}

require_columns <- function(hdr, need, path) {
  miss <- setdiff(need, hdr)
  if (length(miss)) {
    stop_md("0", "columns",
            paste("Flat file is missing column(s):", paste(miss, collapse = ", "), "in", path),
            "Reading a flat file.", TRUE)
  }
}

read_flat <- function(path, gen_label) {
  stopifnot(file.exists(path))
  hdr <- names(fread(path, nrows = 0L, showProgress = FALSE))
  need <- c("session_used", "run_used", "trial_in_run_raw", "time", "pupil")
  if (gen_label == "dec25") need <- c(need, "seg_start_rel_used", "seg_end_rel_used")
  require_columns(hdr, need, path)
  stopifnot(file.exists(path))
  dt <- fread(path, select = need, showProgress = FALSE)
  setnames(dt, c("session_used", "run_used", "trial_in_run_raw"), c("session", "run", "trial"))
  dt[, `:=`(
    session = as.integer(session), run = as.integer(run), trial = as.integer(trial),
    time = as.numeric(time), pupil = as.numeric(pupil)
  )]
  if (gen_label == "dec25") {
    dt[, `:=`(
      seg_start_rel_used = as.numeric(seg_start_rel_used),
      seg_end_rel_used = as.numeric(seg_end_rel_used)
    )]
  } else {
    dt[, `:=`(seg_start_rel_used = NA_real_, seg_end_rel_used = NA_real_)]
  }
  dt <- dt[session %in% c(2L, 3L)]
  dt[, file_row := .I]
  dt
}

features_of <- function(kept, gen_label, subj, tsk) {
  kept[, {
    n_s <- as.integer(.N)
    d <- diff(time)
    df <- d[is.finite(d)]
    mdt <- if (length(df)) as.numeric(median(df)) else NA_real_
    finite_time <- is.finite(time)
    span <- if (any(finite_time)) max(time[finite_time]) - min(time[finite_time]) else NA_real_
    seg_start <- NA_real_
    seg_end <- NA_real_
    nss <- NA_integer_
    nse <- NA_integer_
    if (gen_label == "dec25") {
      seg_start <- as.numeric(seg_start_rel_used[1])
      seg_end <- as.numeric(seg_end_rel_used[1])
      nss <- as.integer(uniqueN(seg_start_rel_used))
      nse <- as.integer(uniqueN(seg_end_rel_used))
    }
    ax <- build_trial_time_250hz(
      n_s, mdt, gen_label, seg_start, seg_end,
      subj, tsk, session[1], run[1], trial[1]
    )
    end_calc <- if (is.finite(n_s)) -3 + 0.004 * (n_s - 1) else NA_real_
    blank <- list(
      matched = TRUE, time_rule = "refused", refuse_reason = ax$reason,
      refuse_class = NA_character_, axis_error = ax$reason,
      n = n_s, trial_median_dt = mdt, dt_used = NA_real_,
      implied_end = NA_real_, end_250hz = end_calc, native_span = span,
      seg_start_used = seg_start, seg_end_used = seg_end,
      n_seg_start = nss, n_seg_end = nse,
      b0_mean = NA_real_, n_b0 = NA_integer_, b0_quality = NA_real_,
      b1_mean = NA_real_, n_b1 = NA_integer_, b1_quality = NA_real_,
      tepr_auc = NA_real_, tepr_quality = NA_real_, tepr_mean_465_765 = NA_real_,
      tepr_mean_485_605 = NA_real_, q_485_605 = NA_real_,
      grip_mean = NA_real_, grip_quality = NA_real_,
      total_auc_b0 = NA_real_, total_quality = NA_real_,
      auc_b0 = NA_real_, generation = gen_label
    )
    if (is.null(ax$t)) {
      blank$refuse_class <- reason_class(ax$reason)
      blank
    } else {
      tt <- ax$t
      b0s <- win_stats(tt, pupil, -0.5, 0, TRUE)
      b0 <- if (b0s$n >= 10L) mean(b0s$vals[is.finite(b0s$vals)]) else NA_real_
      b1s <- win_stats(tt, pupil, 3.85, 4.35, TRUE)
      b1 <- if (b1s$n >= 10L) mean(b1s$vals[is.finite(b1s$vals)]) else NA_real_
      te <- win_stats(tt, pupil, 4.65, 7.65, FALSE)
      w_te <- tt >= 4.65 & tt <= 7.65
      tepr <- NA_real_
      te_mean <- NA_real_
      if (is.finite(b1)) {
        tepr <- compute_auc(tt[w_te], pupil[w_te] - b1)
        if (te$n >= 1L) te_mean <- mean((te$vals - b1)[is.finite(te$vals)])
      }
      w485 <- win_stats(tt, pupil, 4.85, 6.05, FALSE)
      m485 <- NA_real_
      if (is.finite(b1)) {
        if (w485$n >= 1L) m485 <- mean((w485$vals - b1)[is.finite(w485$vals)])
      }
      grip <- window_mean(tt, pupil, b0, 1.0, 4.35)
      tot <- win_stats(tt, pupil, 0, 4.70, FALSE)
      tot_auc <- NA_real_
      if (is.finite(b0)) tot_auc <- compute_auc(tt[tt >= 0 & tt <= 4.70], pupil[tt >= 0 & tt <= 4.70] - b0)
      aucb <- NA_real_
      if (is.finite(b0)) aucb <- compute_auc(tt[w_te], pupil[w_te] - b0)
      list(
        matched = TRUE, time_rule = ax$rule, refuse_reason = NA_character_,
        refuse_class = NA_character_, axis_error = NA_character_,
        n = n_s, trial_median_dt = mdt, dt_used = 0.004,
        implied_end = as.numeric(tt[n_s]), end_250hz = end_calc, native_span = span,
        seg_start_used = seg_start, seg_end_used = seg_end,
        n_seg_start = nss, n_seg_end = nse,
        b0_mean = b0, n_b0 = b0s$n, b0_quality = b0s$prop,
        b1_mean = b1, n_b1 = b1s$n, b1_quality = b1s$prop,
        tepr_auc = tepr, tepr_quality = te$prop, tepr_mean_465_765 = te_mean,
        tepr_mean_485_605 = m485, q_485_605 = w485$prop,
        grip_mean = grip$mean, grip_quality = grip$prop,
        total_auc_b0 = tot_auc, total_quality = tot$prop,
        auc_b0 = aucb, generation = gen_label
      )
    }
  }, by = .(session, run, trial)]
}

apply_gates <- function(trials) {
  trials[, g_tepr_1 := (matched %in% TRUE) & (time_rule != "refused") & is.finite(tepr_auc)]
  trials[, g_tepr_2 := (g_tepr_1 %in% TRUE) & is.finite(b1_quality) & (b1_quality >= 0.50)]
  trials[, g_tepr_3 := (g_tepr_2 %in% TRUE) & is.finite(tepr_quality) & (tepr_quality >= 0.50)]
  tepr_cells <- trials[g_tepr_3 %in% TRUE, .(N = .N), by = .(subject_id, task)]
  tepr_keep <- tepr_cells[N >= 20]
  keep_tepr <- paste(tepr_keep$subject_id, tepr_keep$task)
  trials[, g_tepr_final := (g_tepr_3 %in% TRUE) & (paste(subject_id, task) %in% keep_tepr)]
  trials[, g_grip_1 := (matched %in% TRUE) & (time_rule != "refused") & is.finite(grip_mean)]
  trials[, g_grip_2 := (g_grip_1 %in% TRUE) & is.finite(b0_quality) & (b0_quality >= 0.50)]
  trials[, g_grip_3 := (g_grip_2 %in% TRUE) & is.finite(grip_quality) & (grip_quality >= 0.50)]
  grip_cells <- trials[g_grip_3 %in% TRUE, .(N = .N), by = .(subject_id, task)]
  grip_keep <- grip_cells[N >= 20]
  keep_grip <- paste(grip_keep$subject_id, grip_keep$task)
  trials[, g_grip_final := (g_grip_3 %in% TRUE) & (paste(subject_id, task) %in% keep_grip)]
  invisible(trials)
}

gate_cells <- function(trials) {
  rbindlist(lapply(c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final", "g_grip_1", "g_grip_2", "g_grip_3", "g_grip_final"), function(g) {
    trials[get(g) %in% TRUE, .(gate = g, n_trials = .N, n_subjects = uniqueN(subject_id)), by = .(task, effort)]
  }))
}

gate_by_task <- function(trials) {
  rbindlist(lapply(c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final", "g_grip_1", "g_grip_2", "g_grip_3", "g_grip_final"), function(g) {
    trials[get(g) %in% TRUE, .(gate = g, n_trials = .N, n_subjects = uniqueN(subject_id)), by = task]
  }))
}

agree_rows <- function(dt, metrics, left, right, split_name) {
  rbindlist(lapply(metrics, function(metric) {
    a <- dt[[paste0(metric, left)]]
    b <- dt[[paste0(metric, right)]]
    ok <- is.finite(a) & is.finite(b)
    n <- sum(ok)
    note <- ""
    r <- NA_real_
    if (n < 2L) {
      note <- "undefined: fewer than 2 finite pairs"
    } else if (sd(a[ok]) == 0 || sd(b[ok]) == 0) {
      note <- "undefined: zero variance"
    } else {
      r <- cor(a[ok], b[ok])
    }
    data.table(
      split = split_name, metric = metric,
      n_trials_accepted_both = nrow(dt),
      n_finite_pairs = n,
      pearson_r = r,
      median_abs_diff = if (n >= 1L) as.numeric(median(abs(a[ok] - b[ok]))) else NA_real_,
      r_note = note
    )
  }))
}

fmt_int <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_num <- function(x, digits = 6) if (!is.finite(x)) "NA" else sprintf(paste0("%.", digits, "f"), x)

# --- Part 0 -----------------------------------------------------------------

log_line("=== Phase 2a.2 ", RUN, " ===")
log_line("date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
log_line("R: ", R.version.string)
si <- capture.output(sessionInfo())
writeLines(si, file.path(DIAG, "sessionInfo.txt"))
cat(c("", "sessionInfo:", si, ""), sep = "\n", file = LOG, append = TRUE)

git_ro <- function(repo) {
  list(
    head = paste(system2("git", c("-C", repo, "--no-optional-locks", "rev-parse", "HEAD"), stdout = TRUE), collapse = ""),
    branch = paste(system2("git", c("-C", repo, "--no-optional-locks", "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE), collapse = ""),
    status = system2("git", c("-C", repo, "--no-optional-locks", "status", "--porcelain"), stdout = TRUE)
  )
}
g_ch2 <- git_ro(CH2)
g_ddm <- git_ro(DDM)
log_line("CH2 HEAD ", g_ch2$head, " branch ", g_ch2$branch)
log_line("DDM HEAD ", g_ddm$head, " branch ", g_ddm$branch)
writeLines(g_ch2$status, file.path(DIAG, "0_status_ch2_start.txt"))
writeLines(g_ddm$status, file.path(DIAG, "0_status_ddm_start.txt"))
if (!identical(g_ddm$head, DDM_HEAD_EXPECTED)) {
  stop_md("0", "0", paste("DDM HEAD", g_ddm$head, "expected", DDM_HEAD_EXPECTED),
          "Confirming the read-only Chapter 3 repository.", TRUE)
}
if (!identical(g_ch2$branch, "ch2-revision")) {
  stop_md("0", "0", paste("CH2 branch is", g_ch2$branch),
          "Confirming the Chapter 2 working branch.", TRUE)
}

stopifnot(file.exists(SCRIPT))
src <- readLines(SCRIPT, warn = FALSE)
needle <- paste0("file", "_dt")
code <- sub("#.*", "", src)
call_hit <- grepl(needle, code, fixed = TRUE)
if (any(call_hit)) {
  stop_md("0", "0", paste("A call is on line(s):", paste(which(call_hit), collapse = ", ")),
          "Scanning the Phase 2a.2 script for a call to the retired whole-file interval.", TRUE)
}
comment_hit <- grepl(needle, src, fixed = TRUE) & !call_hit
log_line("Retired whole-file interval token is only in comments on lines: ",
         if (any(comment_hit)) paste(which(comment_hit), collapse = ", ") else "none")

stopifnot(file.exists(HELPER))
source(HELPER)
stopifnot(file.exists(TEST))
test_out <- system2("/usr/local/bin/Rscript", TEST, stdout = TRUE, stderr = TRUE)
writeLines(test_out, file.path(DIAG, "0_axis_250hz_test.txt"))
log_line(paste(test_out, collapse = " | "))
if (!any(grepl("All 250 Hz axis tests passed", test_out))) {
  stop_md("0", "0", paste(tail(test_out, 40), collapse = "\n"),
          "Running tests/test_trial_time_axis_250hz.R.", TRUE)
}
log_line("Part 0 axis tests passed")
smoke_path <- file.path(FLAT, "BAP003_ADT_flat.csv")
stopifnot(file.exists(smoke_path))
sm <- read_flat(smoke_path, "dec21")
sm_n <- sm[, .(N = .N), by = .(session, run, trial)]
sm_key <- sm_n[N %in% c(3425L, 3426L)][1]
sm_one <- sm[session == sm_key$session & run == sm_key$run & trial == sm_key$trial]
sm_built <- features_of(sm_one, "dec21", "BAP003", "ADT")
log_line("Smoke accepted rule ", sm_built$time_rule[1], " n ", sm_built$n[1],
         " end ", sm_built$implied_end[1], " median dt ", sm_built$trial_median_dt[1])
if (nrow(sm_built) != 1L || sm_built$time_rule[1] != "fixed_250hz_from_m3") {
  stop_md("0", "0", "Smoke trial was not accepted on the 250 Hz axis.",
          "Checking one December 21 trial before the full build.", TRUE)
}
if (abs(sm_built$implied_end[1] - sm_built$end_250hz[1]) > 1e-6) {
  stop_md("0", "0", "Smoke trial end does not match -3 + 0.004 * (n - 1).",
          "Checking one December 21 trial before the full build.", TRUE)
}
sm_short <- sm_n[!N %in% c(3425L, 3426L)]
if (nrow(sm_short)) {
  sm_short <- sm_short[1]
  sh_one <- sm[session == sm_short$session & run == sm_short$run & trial == sm_short$trial]
  sh_built <- features_of(sh_one, "dec21", "BAP003", "ADT")
  log_line("Smoke short trial n ", sh_built$n[1], " rule ", sh_built$time_rule[1])
  if (sh_built$time_rule[1] != "refused") {
    stop_md("0", "0", paste("Short trial n", sh_built$n[1], "was accepted."),
            "Checking that a December 21 trial outside {3425, 3426} is refused.", TRUE)
  }
}
rm(sm, sm_n, sm_one, sm_built, sm_short)
log_line("Part 0 smoke trial passed")

for (p in c(HELPER, TEST, OLD_AXIS, R1_TRIALS, P4_R1, P2_REFUSED)) {
  if (!file.exists(p)) stop_md("0", "0", paste("Missing", p), "Checking Phase 2a.2 inputs.", TRUE)
}
if (!dir.exists(FLAT)) stop_md("0", "0", paste("Missing", FLAT), "Checking the December 21 flat directory.", TRUE)
if (!dir.exists(FLAT25)) stop_md("0", "0", paste("Missing", FLAT25), "Checking the December 25 flat directory.", TRUE)
stopifnot(file.exists(OLD_AXIS))
axis_md5_start <- unname(tools::md5sum(OLD_AXIS))
ddm_axis <- file.path(DDM, "R/trial_time_axis.R")
stopifnot(file.exists(ddm_axis))
log_line("R/trial_time_axis.R md5 ", axis_md5_start, " DDM md5 ", unname(tools::md5sum(ddm_axis)))

V2 <- file.path(PFSHARE, "data/aggregated/bap_beh_trialdata_v2.csv")
R5 <- file.path(DDM, "data/analysis_ready/r5/ddm_behavioral_r5_full17857.csv")
R10 <- file.path(DDM, "data/analysis_ready/r10/ddm_pupil_r10.csv")
for (p in c(V2, R5, R10)) {
  if (!file.exists(p)) stop_md("0", "0", paste("Missing", p), "Checking Phase 2a.2 inputs.", TRUE)
}

stopifnot(file.exists(V2))
beh <- fread(V2, showProgress = FALSE)
need <- c("subject_id", "task_modality", "session_num", "run_num", "trial_num", "is_high_grip", "stim_level_index")
miss <- setdiff(need, names(beh))
if (length(miss)) stop_md("0", "0", paste("v2 missing columns:", paste(miss, collapse = ", ")), "Checking the v2 trial file.", TRUE)
beh[, subject_id := as.character(subject_id)]
beh[, task := fifelse(task_modality == "aud", "ADT", fifelse(task_modality == "vis", "VDT", NA_character_))]
beh[, session := as.integer(session_num)]
beh[, run := as.integer(run_num)]
beh[, trial := as.integer(trial_num)]
beh[, stim_level_index := as.integer(stim_level_index)]
beh[, high_grip := as_true(is_high_grip)]
beh[, effort := fifelse(high_grip, "High", "Low")]
n_aud <- uniqueN(beh[task == "ADT"]$subject_id)
n_vis <- uniqueN(beh[task == "VDT"]$subject_id)
bad_140 <- beh[subject_id == "BAP140" & task == "ADT", .N]
bad_excl <- beh[subject_id %in% c("BAP109", "BAP144"), .N]
v2_ok <- nrow(beh) == 18266L && n_aud == 66L && n_vis == 66L &&
  identical(sort(unique(beh$session)), c(2L, 3L)) &&
  identical(sort(unique(beh$stim_level_index)), 0:4) &&
  max(beh$trial) == 30L && bad_140 == 0L && bad_excl == 0L &&
  !anyNA(beh$task)
if (!v2_ok) {
  stop_md("0", "0", paste(
    "v2 checks failed. rows", nrow(beh), "aud subjects", n_aud, "vis subjects", n_vis,
    "BAP140 ADT", bad_140, "BAP109/144", bad_excl
  ), "Checking the v2 trial file.", TRUE)
}
if (anyDuplicated(beh, by = ID) > 0L) {
  stop_md("0", "0", "v2 has duplicate subject x task x session x run x trial keys.", "Checking v2 keys.", TRUE)
}

flats <- list.files(FLAT, pattern = "^BAP[0-9]+_(ADT|VDT)_flat\\.csv$", full.names = TRUE, recursive = FALSE)
if (any(grepl("build_|archive", flats))) {
  stop_md("0", "0", paste(flats[grepl("build_|archive", flats)], collapse = "\n"),
          "Listing top-level December 21 flat files.", TRUE)
}
flats25 <- list.files(FLAT25, pattern = "^BAP[0-9]+_(ADT|VDT)_flat\\.csv$", full.names = TRUE, recursive = FALSE)
log_line("December 21 flat files: ", length(flats), ". December 25 flat files: ", length(flats25), ".")

input_md5 <- data.table(
  item = c("bap_beh_trialdata_v2.csv", "ddm_behavioral_r5", "ddm_pupil_r10", "ch2_pupil_trials_r1",
           "P4_gate_flow", "P2_refused_summary", "trial_time_axis_250hz.R", "test_trial_time_axis_250hz.R",
           "ch2_rebuild_250hz.R", "trial_time_axis.R"),
  path = c(V2, R5, R10, R1_TRIALS, P4_R1, P2_REFUSED, HELPER, TEST, SCRIPT, OLD_AXIS)
)
input_md5[, md5 := unname(tools::md5sum(path))]
fwrite(input_md5, file.path(DIAG, "manifest_inputs.csv"))
log_line("Input md5s:")
for (i in seq_len(nrow(input_md5))) log_line("  ", input_md5$item[i], " ", input_md5$md5[i])

# --- shared build -----------------------------------------------------------

process_generation <- function(gen_label, flat_dir) {
  pairs <- unique(beh[, .(subject_id, task)])
  setorder(pairs, subject_id, task)
  feat_rows <- vector("list", nrow(pairs))
  wave_rows <- vector("list", nrow(pairs))
  subj_rows <- vector("list", nrow(pairs))
  flat_md5 <- vector("list", nrow(pairs))
  ckpt <- file.path(DIAG, "checkpoints", gen_label)
  for (i in seq_len(nrow(pairs))) {
    subj <- pairs$subject_id[i]
    tsk <- pairs$task[i]
    stamp <- file.path(ckpt, paste0(subj, "_", tsk, ".rds"))
    if (file.exists(stamp)) {
      stopifnot(file.exists(stamp))
      prev <- readRDS(stamp)
      feat_rows[[i]] <- prev$feat
      wave_rows[[i]] <- prev$wave
      subj_rows[[i]] <- prev$subj
      flat_md5[[i]] <- prev$md5row
      next
    }
    path <- file.path(flat_dir, sprintf("%s_%s_flat.csv", subj, tsk))
    wanted <- beh[subject_id == subj & task == tsk,
                  .(subject_id, task, session, run, trial, effort, stim_level_index, is_high_grip = high_grip)]
    log_line(gen_label, " ", i, "/", nrow(pairs), " ", basename(path), " v2 trials ", nrow(wanted))
    if (!file.exists(path)) {
      feat <- cbind(wanted, empty_feat(nrow(wanted)))
      feat[, `:=`(matched = FALSE, generation = gen_label)]
      prev <- list(feat = feat, wave = NULL, subj = NULL, md5row = NULL)
      saveRDS(prev, stamp)
      feat_rows[[i]] <- feat
      next
    }
    dt <- read_flat(path, gen_label)
    md5 <- unname(tools::md5sum(path))
    flat_md5[[i]] <- data.table(generation = gen_label, subject_id = subj, task = tsk, path = path, md5 = md5)
    if (!nrow(dt)) {
      feat <- cbind(wanted, empty_feat(nrow(wanted)))
      feat[, `:=`(matched = FALSE, generation = gen_label, source_file = path, source_md5 = md5)]
      prev <- list(feat = feat, wave = NULL, subj = NULL, md5row = flat_md5[[i]])
      saveRDS(prev, stamp)
      feat_rows[[i]] <- feat
      next
    }
    kept <- dt[wanted, on = c("session", "run", "trial"), nomatch = NULL, allow.cartesian = FALSE]
    if (nrow(kept)) setorder(kept, file_row)
    if (nrow(kept)) {
      built <- features_of(kept, gen_label, subj, tsk)
      built[, `:=`(source_file = path, source_md5 = md5)]
      feat <- merge(wanted, built, by = c("session", "run", "trial"), all.x = TRUE, sort = FALSE)
      feat[is.na(matched), matched := FALSE]
      feat[is.na(generation), generation := gen_label]
    } else {
      feat <- cbind(wanted, empty_feat(nrow(wanted)))
      feat[, `:=`(matched = FALSE, generation = gen_label, source_file = path, source_md5 = md5)]
    }
    accepted <- feat[matched %in% TRUE & time_rule == "fixed_250hz_from_m3"]
    cell_wave <- NULL
    subj_wave <- NULL
    if (nrow(accepted)) {
      acc_dt <- dt[accepted[, .(session, run, trial, effort, stim_level_index, trial_median_dt)],
                   on = c("session", "run", "trial"), nomatch = NULL, allow.cartesian = FALSE]
      setorder(acc_dt, file_row)
      pieces <- split(seq_len(nrow(acc_dt)), paste(acc_dt$session, acc_dt$run, acc_dt$trial))
      cell_acc <- list()
      subj_wave <- new_wave()
      for (ix in pieces) {
        tr <- acc_dt[ix]
        ax <- build_trial_time_250hz(
          nrow(tr), tr$trial_median_dt[1], gen_label,
          tr$seg_start_rel_used[1], tr$seg_end_rel_used[1],
          subj, tsk, tr$session[1], tr$run[1], tr$trial[1]
        )
        if (is.null(ax$t)) next
        cell_key <- paste(tr$effort[1], tr$stim_level_index[1], sep = "|")
        if (is.null(cell_acc[[cell_key]])) cell_acc[[cell_key]] <- new_wave()
        cell_acc[[cell_key]] <- add_trial(ax$t, tr$pupil, cell_acc[[cell_key]])
        subj_wave <- add_trial(ax$t, tr$pupil, subj_wave)
      }
      if (length(cell_acc)) {
        cell_wave <- rbindlist(lapply(names(cell_acc), function(k) {
          w <- cell_acc[[k]]
          parts <- strsplit(k, "|", fixed = TRUE)[[1]]
          data.table(
            subject_id = subj, task = tsk, effort = parts[[1]],
            stim_level_index = as.integer(parts[[2]]),
            bin = seq_along(GRID), sum_y = w$sums, n_bin = w$counts, n_trials = w$n_trials
          )
        }))
      }
    }
    prev <- list(feat = feat, wave = cell_wave, subj = subj_wave, md5row = flat_md5[[i]])
    saveRDS(prev, stamp)
    feat_rows[[i]] <- feat
    wave_rows[[i]] <- cell_wave
    subj_rows[[i]] <- subj_wave
    rm(dt, kept)
    if (i %% 20L == 0L) {
      log_line("Checkpoint after ", i, " ", gen_label, " subject x task files")
      gc(verbose = FALSE)
    }
  }
  trials <- rbindlist(feat_rows, fill = TRUE)
  waves <- rbindlist(wave_rows, fill = TRUE)
  md5s <- rbindlist(flat_md5, fill = TRUE)
  list(trials = trials, waves = waves, subj = subj_rows, pairs = pairs, md5s = md5s)
}

partition_ok <- function(trials, label) {
  n_acc <- trials[time_rule == "fixed_250hz_from_m3", .N]
  n_ref <- trials[time_rule == "refused", .N]
  n_un <- trials[!matched %in% TRUE, .N]
  log_line(label, " accepted ", n_acc, " refused ", n_ref, " unmatched ", n_un, " rows ", nrow(trials))
  if (nrow(trials) != 18266L || n_acc + n_ref + n_un != 18266L) {
    stop_md("A", label, paste("rows", nrow(trials), "accepted", n_acc, "refused", n_ref, "unmatched", n_un),
            "Checking accepted + refused + unmatched = 18266.", TRUE)
  }
  list(accepted = n_acc, refused = n_ref, unmatched = n_un)
}

write_waves <- function(waves, dest) {
  if (!nrow(waves)) {
    stop_md("A", "waveforms", "No waveform rows were produced.", "Writing the subject x condition waveforms.", TRUE)
  }
  wave_out <- waves[, .(
    sum_y = sum(sum_y), n_bin = sum(n_bin), n_trials = max(n_trials)
  ), by = .(subject_id, task, effort, stim_level_index, bin)]
  wave_out[, t := GRID[bin]]
  wave_out[, mean_b0 := fifelse(n_bin > 0, sum_y / n_bin, NA_real_)]
  fwrite(wave_out[, .(subject_id, task, effort, stim_level_index, t, mean_b0, n = n_bin, n_trials)], dest)
  log_line("Wrote ", dest, " rows ", nrow(wave_out))
}

# --- Part A: December 21 ----------------------------------------------------

log_line("Part A December 21")
dec21 <- process_generation("dec21", FLAT)
trials <- dec21$trials
counts21 <- partition_ok(trials, "dec21")

acc21 <- trials[time_rule == "fixed_250hz_from_m3"]
if (nrow(acc21)) {
  delta <- abs(acc21$implied_end - acc21$end_250hz)
  bad_end <- !is.finite(delta) | delta > 1e-6
  bad_n <- !(acc21$n %in% c(3425L, 3426L))
  bad_dt <- !is.finite(acc21$trial_median_dt) | abs(acc21$trial_median_dt - 0.004) > 1e-6
  if (any(bad_end) || any(bad_n) || any(bad_dt)) {
    stop_md("A", "axis", paste("accepted trials fail the 250 Hz check. end", sum(bad_end), "n", sum(bad_n), "dt", sum(bad_dt)),
            "Checking accepted December 21 trials against D007.", TRUE)
  }
}

ref <- trials[time_rule == "refused"]
if (nrow(ref)) {
  fwrite(ref[, .(subject_id, task, session, run, trial, n, trial_median_dt, end_250hz, refuse_class, refuse_reason)],
         file.path(DIAG, "A_refusals.csv"))
} else {
  fwrite(data.table(n_refused = 0L, refuse_class = "none", refuse_reason = "no trial was refused"),
         file.path(DIAG, "A_refusals.csv"))
}
if (counts21$refused > 200L) {
  reason_tbl <- if (nrow(ref)) ref[, .(n_trials = .N), by = refuse_class] else data.table(refuse_class = "none", n_trials = 0L)
  stop_md("A", "A", paste(capture.output(print(reason_tbl)), collapse = "\n"),
          "December 21 refusals exceed 200.", FALSE)
}

# --- Part B: gates ----------------------------------------------------------

log_line("Part B gates")
apply_gates(trials)
fwrite(trials, file.path(DATA, "ch2_pupil_trials_r2.csv"))
write_waves(dec21$waves, file.path(DATA, "ch2_waveforms_subject_condition_r2.csv"))

stopifnot(file.exists(P4_R1))
p4 <- fread(P4_R1, showProgress = FALSE)
p4_need <- c("task", "effort", "gate", "n_trials", "n_subjects")
miss_p4 <- setdiff(p4_need, names(p4))
if (length(miss_p4)) stop("P4_gate_flow.csv is missing column(s): ", paste(miss_p4, collapse = ", "), call. = FALSE)
r2_cells <- gate_cells(trials)
setnames(p4, c("n_trials", "n_subjects"), c("n_trials_r1", "n_subjects_r1"))
setnames(r2_cells, c("n_trials", "n_subjects"), c("n_trials_r2", "n_subjects_r2"))
flow <- merge(p4, r2_cells, by = c("task", "effort", "gate"), all = TRUE)
for (col in c("n_trials_r1", "n_subjects_r1", "n_trials_r2", "n_subjects_r2")) {
  idx <- which(is.na(flow[[col]]))
  if (length(idx)) set(flow, idx, col, 0L)
}
flow[, grain := "task_effort"]

stopifnot(file.exists(R1_TRIALS))
r1_need <- c(ID, "effort", "time_rule", "b0_mean", "b1_mean", "tepr_mean_465_765", "grip_mean", "tepr_quality",
             "g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final", "g_grip_1", "g_grip_2", "g_grip_3", "g_grip_final")
stopifnot(file.exists(R1_TRIALS))
r1_hdr <- names(fread(R1_TRIALS, nrows = 0L))
miss_r1 <- setdiff(r1_need, r1_hdr)
if (length(miss_r1)) stop("ch2_pupil_trials_r1.csv is missing column(s): ", paste(miss_r1, collapse = ", "), call. = FALSE)
stopifnot(file.exists(R1_TRIALS))
r1 <- fread(R1_TRIALS, select = r1_need, showProgress = FALSE)
r1[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
          session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]

r1_task <- gate_by_task(r1)
r2_task <- gate_by_task(trials)
setnames(r1_task, c("n_trials", "n_subjects"), c("n_trials_r1", "n_subjects_r1"))
setnames(r2_task, c("n_trials", "n_subjects"), c("n_trials_r2", "n_subjects_r2"))
flow_task <- merge(r1_task, r2_task, by = c("task", "gate"), all = TRUE)
flow_task[, effort := "ALL"]
flow_task[, grain := "task"]
for (col in c("n_trials_r1", "n_subjects_r1", "n_trials_r2", "n_subjects_r2")) {
  idx <- which(is.na(flow_task[[col]]))
  if (length(idx)) set(flow_task, idx, col, 0L)
}
flow_all <- rbind(flow, flow_task, fill = TRUE)
setcolorder(flow_all, c("grain", "task", "effort", "gate", "n_trials_r1", "n_subjects_r1", "n_trials_r2", "n_subjects_r2"))
gate_order <- c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final", "g_grip_1", "g_grip_2", "g_grip_3", "g_grip_final")
flow_all[, gate := factor(gate, levels = gate_order)]
setorder(flow_all, grain, gate, task, effort)
flow_all[, gate := as.character(gate)]
fwrite(flow_all, file.path(DIAG, "B_gate_flow_r1_vs_r2.csv"))
log_line("Wrote B_gate_flow_r1_vs_r2.csv")

# --- recording recovery -----------------------------------------------------

stopifnot(file.exists(P2_REFUSED))
prev_ref <- fread(P2_REFUSED, showProgress = FALSE)
if (!all(c("subject_id", "task") %in% names(prev_ref))) {
  stop("P2_refused_summary.csv is missing column(s): ",
       paste(setdiff(c("subject_id", "task"), names(prev_ref)), collapse = ", "), call. = FALSE)
}
prev_ref[, `:=`(subject_id = as.character(subject_id), task = as.character(task))]
r1_ref_n <- r1[time_rule == "refused", .(n_r1_refused = .N), by = .(subject_id, task)]
r2_acc_n <- trials[time_rule == "fixed_250hz_from_m3", .(n_r2_accepted = .N), by = .(subject_id, task)]
r2_ref_n <- trials[time_rule == "refused", .(n_r2_refused = .N), by = .(subject_id, task)]
recovery <- merge(unique(prev_ref[, .(subject_id, task)]), r1_ref_n, by = c("subject_id", "task"), all.x = TRUE)
recovery <- merge(recovery, r2_acc_n, by = c("subject_id", "task"), all.x = TRUE)
recovery <- merge(recovery, r2_ref_n, by = c("subject_id", "task"), all.x = TRUE)
for (col in c("n_r1_refused", "n_r2_accepted", "n_r2_refused")) {
  idx <- which(is.na(recovery[[col]]))
  if (length(idx)) set(recovery, idx, col, 0L)
}
recovery[, recovered := n_r2_accepted > 0L]
setorder(recovery, task, subject_id)
fwrite(recovery, file.path(DIAG, "A_recording_recovery.csv"))
n_prev <- nrow(recovery)
n_recovered <- recovery[recovered == TRUE, .N]
log_line("Previously refused recordings ", n_prev, " recovered ", n_recovered)

r1_ref_keys <- r1[time_rule == "refused", ..ID]
r2_status <- trials[, c(ID, "time_rule"), with = FALSE]
rec_trials <- merge(r1_ref_keys, r2_status, by = ID, all.x = TRUE)
n_ref_trials_now_acc <- rec_trials[time_rule == "fixed_250hz_from_m3", .N]
n_ref_trials_still <- rec_trials[time_rule == "refused", .N]
log_line("r1 refused trials ", nrow(r1_ref_keys), " now accepted ", n_ref_trials_now_acc, " still refused ", n_ref_trials_still)

# --- Part C: landmarks ------------------------------------------------------

log_line("Part C landmarks")
grid_extreme <- function(y, lo, hi, pick) {
  m <- GRID >= lo & GRID <= hi & is.finite(y)
  if (!any(m)) return(list(t = NA_real_, y = NA_real_))
  k <- pick(y[m])
  list(t = GRID[m][k], y = y[m][k])
}
grand_by_task <- list()
landmark_rows <- list()
for (tsk in c("ADT", "VDT")) {
  ys <- list()
  ntr <- 0L
  for (i in which(dec21$pairs$task == tsk)) {
    w <- dec21$subj[[i]]
    if (is.null(w) || w$n_trials < 1L) next
    y <- ifelse(w$counts > 0, w$sums / w$counts, NA_real_)
    ys[[length(ys) + 1L]] <- y
    ntr <- ntr + w$n_trials
  }
  n_acc_task <- trials[task == tsk & time_rule == "fixed_250hz_from_m3", .N]
  if (!length(ys)) {
    grand_by_task[[tsk]] <- NULL
    landmark_rows[[tsk]] <- data.table(
      task = tsk, n_subjects = 0L, n_trials_in_trace = 0L, n_accepted_trials = n_acc_task,
      t_minimum_2.5_to_5.0 = NA_real_, y_minimum = NA_real_,
      t_maximum_4.35_to_7.70 = NA_real_, y_maximum = NA_real_
    )
    next
  }
  M <- do.call(rbind, ys)
  grand <- colMeans(M, na.rm = TRUE)
  grand[!is.finite(grand)] <- NA_real_
  grand_by_task[[tsk]] <- list(y = grand, n_subjects = length(ys), n_trials = ntr)
  mn <- grid_extreme(grand, 2.5, 5.0, which.min)
  mx <- grid_extreme(grand, 4.35, 7.70, which.max)
  landmark_rows[[tsk]] <- data.table(
    task = tsk, n_subjects = length(ys), n_trials_in_trace = ntr, n_accepted_trials = n_acc_task,
    t_minimum_2.5_to_5.0 = mn$t, y_minimum = mn$y,
    t_maximum_4.35_to_7.70 = mx$t, y_maximum = mx$y
  )
}
landmarks <- rbindlist(landmark_rows)
fwrite(landmarks, file.path(DIAG, "C_landmarks.csv"))

png_path <- file.path(DIAG, "C_grand_waveform_r2.png")
png(png_path, width = 1200, height = 700)
plot_ok <- TRUE
op <- par(mfrow = c(1, 2), mar = c(4.2, 4, 3, 1))
for (tsk in c("ADT", "VDT")) {
  g <- grand_by_task[[tsk]]
  if (is.null(g)) {
    plot.new()
    title(main = paste(tsk, "no trace"))
    next
  }
  plot(GRID, g$y, type = "l", xlab = "Time (s) from squeeze onset", ylab = "B0-corrected pupil",
       main = paste0(tsk, ": ", g$n_subjects, " subjects, ", g$n_trials, " trials"),
       lwd = 2, col = "black")
  usr <- par("usr")
  rect(-0.5, usr[3], 0, usr[4], col = rgb(0.45, 0.45, 0.45, 0.28), border = NA)
  rect(1.0, usr[3], 4.35, usr[4], col = rgb(0.15, 0.55, 0.30, 0.16), border = NA)
  rect(3.85, usr[3], 4.35, usr[4], col = rgb(0.75, 0.65, 0.15, 0.28), border = NA)
  rect(4.65, usr[3], 7.65, usr[4], col = rgb(0.20, 0.35, 0.75, 0.18), border = NA)
  lines(GRID, g$y, lwd = 2, col = "black")
  abline(v = c(0, 3.00, 3.25, 3.75, 4.35, 4.45, 4.70, 7.70), lty = 2, col = "grey20")
  legend("topright", legend = c("B0", "Grip", "B1", "TEPR"),
         fill = c(rgb(0.45, 0.45, 0.45, 0.45), rgb(0.15, 0.55, 0.30, 0.35),
                  rgb(0.75, 0.65, 0.15, 0.45), rgb(0.20, 0.35, 0.75, 0.35)),
         bty = "n", cex = 0.85)
}
par(op)
dev.off()
log_line("Wrote C_landmarks.csv and C_grand_waveform_r2.png")

# --- Part D: December 25 ----------------------------------------------------

log_line("Part D December 25")
dec25 <- process_generation("dec25", FLAT25)
trials25 <- dec25$trials
counts25 <- partition_ok(trials25, "dec25")
apply_gates(trials25)
fwrite(trials25, file.path(DATA, "ch2_pupil_trials_dec25_r2.csv"))
if (nrow(dec25$md5s) || nrow(dec21$md5s)) {
  fwrite(rbind(dec21$md5s, dec25$md5s, fill = TRUE), file.path(DIAG, "manifest_flats.csv"))
}

metrics_d <- c("b0_mean", "b1_mean", "tepr_mean_465_765", "tepr_quality", "grip_mean")
a21 <- trials[time_rule == "fixed_250hz_from_m3", c(ID, metrics_d), with = FALSE]
a25 <- trials25[time_rule == "fixed_250hz_from_m3", c(ID, metrics_d), with = FALSE]
setnames(a21, metrics_d, paste0(metrics_d, "_21"))
setnames(a25, metrics_d, paste0(metrics_d, "_25"))
both_gen <- merge(a21, a25, by = ID)
d2 <- rbind(
  agree_rows(both_gen, metrics_d, "_21", "_25", "overall"),
  agree_rows(both_gen[task == "ADT"], metrics_d, "_21", "_25", "ADT"),
  agree_rows(both_gen[task == "VDT"], metrics_d, "_21", "_25", "VDT")
)
fwrite(d2, file.path(DIAG, "D2_generation_agreement.csv"))
log_line("D2 shared accepted trials ", nrow(both_gen))

named <- data.table(
  subject_id = c("BAP122", "BAP139", "BAP186", "BAP191", "BAP194", "BAP195", "BAP101", "BAP103", "BAP114"),
  task = c(rep("ADT", 6), rep("VDT", 3)),
  record_type = "phase1_named_case"
)
gain_cells <- trials25[g_tepr_final %in% TRUE, .(n_trials = .N), by = .(subject_id, task, effort)]
gain_cells[, dec21_file := file.exists(file.path(FLAT, sprintf("%s_%s_flat.csv", subject_id, task)))]
gain_only <- gain_cells[dec21_file == FALSE]
gain_only[, `:=`(record_type = "dec25_tepr_final_no_dec21_file", passes_tepr_stream = TRUE, would_add = TRUE)]

named_rows <- rbindlist(lapply(seq_len(nrow(named)), function(i) {
  subj <- named$subject_id[i]
  tsk <- named$task[i]
  sub <- trials25[subject_id == subj & task == tsk]
  f21 <- file.exists(file.path(FLAT, sprintf("%s_%s_flat.csv", subj, tsk)))
  f25 <- file.exists(file.path(FLAT25, sprintf("%s_%s_flat.csv", subj, tsk)))
  passes <- any(sub$g_tepr_final %in% TRUE)
  by_eff <- sub[g_tepr_final %in% TRUE, .(n_trials = .N), by = effort]
  if (!nrow(by_eff)) by_eff <- data.table(effort = NA_character_, n_trials = 0L)
  by_eff[, `:=`(
    record_type = "phase1_named_case", subject_id = subj, task = tsk,
    dec21_file_present = f21, dec25_file_present = f25,
    n_accepted = sub[time_rule == "fixed_250hz_from_m3", .N],
    n_tepr_final = sub[g_tepr_final %in% TRUE, .N],
    passes_tepr_stream = passes,
    would_add = passes & !f21,
    note = ""
  )]
  by_eff
}))
if (nrow(gain_only)) {
  gain_out <- gain_only[, .(
    record_type, subject_id, task, effort, n_trials,
    dec21_file_present = FALSE, dec25_file_present = TRUE,
    n_accepted = NA_integer_, n_tepr_final = n_trials,
    passes_tepr_stream = TRUE, would_add = TRUE,
    note = "passes the December 25 TEPR stream and has no December 21 file"
  )]
} else {
  gain_out <- data.table(
    record_type = "dec25_tepr_final_no_dec21_file", subject_id = NA_character_, task = NA_character_,
    effort = NA_character_, n_trials = 0L, dec21_file_present = NA, dec25_file_present = NA,
    n_accepted = NA_integer_, n_tepr_final = 0L, passes_tepr_stream = FALSE, would_add = FALSE,
    note = "no subject x task passes the December 25 TEPR stream without a December 21 file"
  )
}
d3 <- rbind(gain_out, named_rows, fill = TRUE)
fwrite(d3, file.path(DIAG, "D3_dec25_only_gain.csv"))
log_line("D3 would-add rows ", gain_only[, .N], " named cases ", nrow(named))

# --- Part E -----------------------------------------------------------------

log_line("Part E r1 vs r2")
metrics_e <- c("b0_mean", "b1_mean", "tepr_mean_465_765", "grip_mean", "tepr_quality")
e1 <- r1[time_rule == "step_from_m3", c(ID, metrics_e), with = FALSE]
e2 <- trials[time_rule == "fixed_250hz_from_m3", c(ID, metrics_e), with = FALSE]
setnames(e1, metrics_e, paste0(metrics_e, "_r1"))
setnames(e2, metrics_e, paste0(metrics_e, "_r2"))
both_re <- merge(e1, e2, by = ID)
e_tab <- rbind(
  agree_rows(both_re, metrics_e, "_r1", "_r2", "overall"),
  agree_rows(both_re[task == "ADT"], metrics_e, "_r1", "_r2", "ADT"),
  agree_rows(both_re[task == "VDT"], metrics_e, "_r1", "_r2", "VDT")
)
fwrite(e_tab, file.path(DIAG, "E_r1_vs_r2.csv"))
log_line("E shared accepted trials ", nrow(both_re))

# --- Part F -----------------------------------------------------------------

log_line("Part F Chapter 3 export")
stopifnot(file.exists(R5))
r5_hdr <- names(fread(R5, nrows = 0L))
miss_r5 <- setdiff(ID, r5_hdr)
if (length(miss_r5)) stop("ddm_behavioral_r5_full17857.csv is missing column(s): ", paste(miss_r5, collapse = ", "), call. = FALSE)
stopifnot(file.exists(R5))
r5 <- fread(R5, select = ID, showProgress = FALSE)
r5[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
          session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
if (anyDuplicated(r5, by = ID) > 0L) {
  stop_md("F", "F", "r5 has duplicate keys.", "Building the Chapter 3-compatible export.", TRUE)
}
f_src <- trials[, .(
  subject_id, task, session, run, trial,
  baseline_raw = b0_mean, b1_mean, B1_quality = b1_quality,
  tepr_raw = tepr_auc, cog_quality = tepr_quality, auc_b0,
  grip_raw = grip_mean, grip_prop = grip_quality,
  tepr_mean_465_765, time_rule, in_v2 = TRUE
)]
ch3 <- merge(r5, f_src, by = ID, all.x = TRUE, sort = FALSE)
if (nrow(ch3) != nrow(r5)) {
  stop_md("F", "F", paste("Export rows", nrow(ch3), "r5 rows", nrow(r5)),
          "Joining December 21 features onto r5 keys.", TRUE)
}
n_r5_missing <- ch3[is.na(in_v2), .N]
ch3[, in_v2 := NULL]
ch3_cols <- c("subject_id", "task", "session", "run", "trial",
              "baseline_raw", "b1_mean", "B1_quality", "tepr_raw", "cog_quality",
              "auc_b0", "grip_raw", "grip_prop", "tepr_mean_465_765", "time_rule")
setcolorder(ch3, ch3_cols)
if (!identical(names(ch3), ch3_cols)) {
  stop_md("F", "F", paste(names(ch3), collapse = ", "), "Checking Chapter 3 column names.", TRUE)
}
fwrite(ch3, file.path(DATA, "ch3_compatible_pupil_r2.csv"))

ch3[, g1 := (time_rule %in% "fixed_250hz_from_m3") & is.finite(tepr_raw)]
ch3[, g2 := (g1 %in% TRUE) & is.finite(B1_quality) & (B1_quality >= 0.50)]
ch3[, g3 := (g2 %in% TRUE) & is.finite(cog_quality) & (cog_quality >= 0.50)]
cell_f <- ch3[g3 %in% TRUE, .(N = .N), by = .(subject_id, task)]
keep_f <- cell_f[N >= 20]
keep_f_id <- paste(keep_f$subject_id, keep_f$task)
ch3[, g4 := (g3 %in% TRUE) & (paste(subject_id, task) %in% keep_f_id)]

stopifnot(file.exists(R10))
r10_lines <- as.integer(system(paste("wc -l <", shQuote(R10)), intern = TRUE))
r10_n <- r10_lines - 1L
log_line("r10 data rows ", r10_n)

stage_flag <- list(
  "0_r5" = rep(TRUE, nrow(ch3)),
  "1_tepr_present" = ch3$g1 %in% TRUE,
  "2_b1_ge_0.50" = ch3$g2 %in% TRUE,
  "3_window_ge_0.50" = ch3$g3 %in% TRUE,
  "4_ge_20_per_subject_task" = ch3$g4 %in% TRUE
)
gate_f <- rbindlist(lapply(names(stage_flag), function(st) {
  flag <- stage_flag[[st]]
  rows <- rbindlist(lapply(c("ALL", "ADT", "VDT"), function(tsk) {
    use <- if (tsk == "ALL") flag else flag & ch3$task == tsk
    data.table(
      stage = st, task = tsk,
      n_trials = sum(use),
      n_participants = uniqueN(ch3$subject_id[use])
    )
  }))
  rows
}))
gate_f[, r10_trials := r10_n]
setcolorder(gate_f, c("stage", "task", "n_trials", "n_participants", "r10_trials"))
fwrite(gate_f, file.path(DATA, "ch3_compatible_gate_counts_r2.csv"))
# Gate columns stay off the exported pupil table.
ch3[, c("g1", "g2", "g3", "g4") := NULL]
log_line("Wrote Chapter 3 export rows ", nrow(ch3))

# --- report -----------------------------------------------------------------

deviations <- character()
undetermined <- character()
if (length(flats) != 116L) deviations <- c(deviations, paste("December 21 pattern matched", length(flats), "files; the procedure named 116."))
if (length(flats25) != 113L) deviations <- c(deviations, paste("December 25 pattern matched", length(flats25), "files; the procedure named 113."))
if (!identical(axis_md5_start, unname(tools::md5sum(OLD_AXIS)))) {
  deviations <- c(deviations, "R/trial_time_axis.R md5 changed during the run.")
}
if (!identical(unname(tools::md5sum(OLD_AXIS)), unname(tools::md5sum(ddm_axis)))) {
  deviations <- c(deviations, "R/trial_time_axis.R md5 differs from the Chapter 3 copy.")
}
seg_bad <- trials25[n_seg_start > 1L | n_seg_end > 1L, .N]
if (seg_bad > 0L) {
  undetermined <- c(undetermined, paste(
    "December 25 segment columns are not constant within", seg_bad,
    "trials. The first row was used, as the procedure specifies, so a single segment is UNDETERMINED for those trials."
  ))
}
if (n_r5_missing > 0L) {
  undetermined <- c(undetermined, paste(n_r5_missing, "r5 keys have no December 21 rebuild row."))
}
if (r10_n != 4935L) {
  undetermined <- c(undetermined, paste("ddm_pupil_r10.csv has", r10_n, "data rows; the procedure's comparison figure is 4935."))
}
if (!length(deviations)) deviations <- "None."
if (!length(undetermined)) undetermined <- "None."

final_txt <- function(tab, g, tsk) {
  row <- tab[grain == "task" & gate == g & task == tsk]
  if (!nrow(row)) return(paste0(tsk, " NA"))
  paste0(tsk, " r1 ", fmt_int(row$n_trials_r1), " trials / ", fmt_int(row$n_subjects_r1),
         " participants; r2 ", fmt_int(row$n_trials_r2), " trials / ", fmt_int(row$n_subjects_r2), " participants")
}

reason_counts <- if (nrow(ref)) ref[, .(n_trials = .N), by = refuse_class][order(-n_trials)] else data.table(refuse_class = "none", n_trials = 0L)
reason_txt <- paste(paste0(reason_counts$refuse_class, " ", reason_counts$n_trials), collapse = "; ")

land_txt <- function(tsk) {
  row <- landmarks[task == tsk]
  paste0(tsk, " minimum ", fmt_num(row$t_minimum_2.5_to_5.0, 2), " s, maximum ", fmt_num(row$t_maximum_4.35_to_7.70, 2),
         " s (", fmt_int(row$n_subjects), " subjects)")
}
d2_txt <- function(met, spl) {
  row <- d2[metric == met & split == spl]
  paste0(spl, " r ", fmt_num(row$pearson_r, 4), ", median |diff| ", fmt_num(row$median_abs_diff, 4), ", n ", fmt_int(row$n_finite_pairs))
}
e_txt <- function(met, spl) {
  row <- e_tab[metric == met & split == spl]
  paste0(spl, " r ", fmt_num(row$pearson_r, 4), ", median |diff| ", fmt_num(row$median_abs_diff, 4), ", n ", fmt_int(row$n_finite_pairs))
}

would <- named_rows[would_add == TRUE, unique(paste(subject_id, task))]
would_extra <- if (nrow(gain_only)) unique(paste(gain_only$subject_id, gain_only$task)) else character()
would_all <- sort(unique(c(would, would_extra)))
would_txt <- if (length(would_all)) paste(would_all, collapse = "; ") else "none"

f_final <- gate_f[stage == "4_ge_20_per_subject_task" & task == "ALL"]
f_adt <- gate_f[stage == "4_ge_20_per_subject_task" & task == "ADT"]
f_vdt <- gate_f[stage == "4_ge_20_per_subject_task" & task == "VDT"]
ready <- counts21$refused <= 200L
ready_txt <- if (ready) "YES" else "NO"

tepr_d2 <- d2[metric == "tepr_mean_465_765" & split == "overall"]
tepr_e <- e_tab[metric == "tepr_mean_465_765" & split == "overall"]

report <- c(
  "# Chapter 2 Phase 2a.2",
  "",
  paste("Run:", RUN),
  "",
  "## 1. Summary",
  "",
  "Part 0: PASS. The 250 Hz axis tests passed. The retired whole-file interval is not called.",
  paste0("Part A: ", fmt_int(counts21$accepted), " accepted, ", fmt_int(counts21$refused), " refused, ",
         fmt_int(counts21$unmatched), " unmatched (sum 18,266). Recovered ", n_recovered, " of ", n_prev,
         " previously refused recordings."),
  paste0("Part B: TEPR final ", final_txt(flow_all, "g_tepr_final", "ADT"), "; ",
         final_txt(flow_all, "g_tepr_final", "VDT"), ". Grip final ",
         final_txt(flow_all, "g_grip_final", "ADT"), "; ", final_txt(flow_all, "g_grip_final", "VDT"), "."),
  paste0("Part C: ", land_txt("ADT"), "; ", land_txt("VDT"), "."),
  paste0("Part D: shared accepted trials ", fmt_int(nrow(both_gen)),
         ". tepr_mean_465_765 ", d2_txt("tepr_mean_465_765", "overall"),
         ". December 25 TEPR stream would add: ", would_txt, "."),
  paste0("Part E: trials accepted on both axes ", fmt_int(nrow(both_re)),
         ". tepr_mean_465_765 ", e_txt("tepr_mean_465_765", "overall"), "."),
  paste0("Part F: r5 gate, at least 20 trials per subject x task: ",
         fmt_int(f_final$n_trials), " trials / ", fmt_int(f_final$n_participants),
         " participants (ADT ", fmt_int(f_adt$n_trials), ", VDT ", fmt_int(f_vdt$n_trials),
         "). r10 reference ", fmt_int(r10_n), "."),
  "",
  paste0("Ready for Phase 2b? ", ready_txt),
  "",
  "## 2. Headline answers",
  "",
  paste0("(i) Refusals (A_refusals.csv, A_recording_recovery.csv). December 21 refused ",
         fmt_int(counts21$refused), " trials. Reasons: ", reason_txt,
         ". Previously refused recordings in P2_refused_summary.csv: ", n_prev,
         ". Recordings with at least one accepted r2 trial: ", n_recovered,
         ". Of ", fmt_int(nrow(r1_ref_keys)), " r1-refused trials, ",
         fmt_int(n_ref_trials_now_acc), " are accepted on the 250 Hz axis and ",
         fmt_int(n_ref_trials_still), " are still refused."),
  paste0("(ii) Final streams (B_gate_flow_r1_vs_r2.csv). TEPR: ",
         final_txt(flow_all, "g_tepr_final", "ADT"), "; ", final_txt(flow_all, "g_tepr_final", "VDT"),
         ". Grip: ", final_txt(flow_all, "g_grip_final", "ADT"), "; ",
         final_txt(flow_all, "g_grip_final", "VDT"),
         ". Task-level participant counts are unique subjects. The task x effort rows match Phase 2a P4."),
  paste0("(iii) Landmarks (C_landmarks.csv, C_grand_waveform_r2.png). ",
         land_txt("ADT"), "; ", land_txt("VDT"),
         ". Each subject mean pools that subject's accepted trials; the grand mean weights subjects equally. Search windows are [2.5, 5.0] for the minimum and [4.35, 7.70] for the maximum."),
  paste0("(iv) Generations (D2_generation_agreement.csv, D3_dec25_only_gain.csv). Trials accepted in both: ",
         fmt_int(nrow(both_gen)), ". ",
         "b0_mean ", d2_txt("b0_mean", "overall"), "; ",
         "b1_mean ", d2_txt("b1_mean", "overall"), "; ",
         "tepr_mean_465_765 ", d2_txt("tepr_mean_465_765", "overall"), "; ",
         "tepr_quality ", d2_txt("tepr_quality", "overall"), "; ",
         "grip_mean ", d2_txt("grip_mean", "overall"),
         ". By task, tepr_mean_465_765 ", d2_txt("tepr_mean_465_765", "ADT"), "; ",
         d2_txt("tepr_mean_465_765", "VDT"),
         ". December 25 accepted ", fmt_int(counts25$accepted), ", refused ", fmt_int(counts25$refused),
         ", unmatched ", fmt_int(counts25$unmatched),
         ". Subject x task that pass the December 25 TEPR stream with no December 21 file: ", would_txt, "."),
  paste0("(v) r1 versus r2 (E_r1_vs_r2.csv), trials accepted on both axes (", fmt_int(nrow(both_re)), "). ",
         "b0_mean ", e_txt("b0_mean", "overall"), "; ",
         "b1_mean ", e_txt("b1_mean", "overall"), "; ",
         "tepr_mean_465_765 ", e_txt("tepr_mean_465_765", "overall"), "; ",
         "grip_mean ", e_txt("grip_mean", "overall"), "; ",
         "tepr_quality ", e_txt("tepr_quality", "overall"), "."),
  paste0("(vi) Chapter 3 export (ch3_compatible_pupil_r2.csv, ch3_compatible_gate_counts_r2.csv). ",
         "Columns are subject_id, task, session, run, trial, baseline_raw, b1_mean, B1_quality, tepr_raw, cog_quality, auc_b0, grip_raw, grip_prop, tepr_mean_465_765, time_rule. ",
         "Rows: ", fmt_int(nrow(r5)), " r5 trials. ",
         "Gate counts, ALL: ",
         paste(gate_f[task == "ALL", paste0(stage, " ", fmt_int(n_trials), " trials / ", fmt_int(n_participants), " participants")], collapse = "; "),
         ". Final gate ", fmt_int(f_final$n_trials), " beside r10 ", fmt_int(r10_n), "."),
  "",
  "## 3. Deviations",
  "",
  deviations,
  "",
  "## 4. UNDETERMINED",
  "",
  undetermined,
  "",
  "Primary measure on this axis: tepr_mean_465_765. Gates follow Phase 2a: a TEPR value is present when tepr_auc is finite, B1 quality is at least 0.50, window quality is at least 0.50, and the subject x task has at least 20 such trials. auc_b0 is the gap-respecting AUC of pupil minus b0_mean on [4.65, 7.65]. December 25 is not in the primary table."
)
writeLines(report, file.path(DIAG, "report.md"))
log_line("Wrote report.md. Ready for Phase 2b? ", ready_txt)

# --- self-check and zip -----------------------------------------------------

g_ddm_end <- git_ro(DDM)
g_ch2_end <- git_ro(CH2)
writeLines(g_ch2_end$status, file.path(DIAG, "0_status_ch2_end.txt"))
writeLines(g_ddm_end$status, file.path(DIAG, "0_status_ddm_end.txt"))
log_line("CH2 HEAD end ", g_ch2_end$head)
log_line("DDM HEAD end ", g_ddm_end$head)
same_ddm <- identical(g_ddm$status, g_ddm_end$status) && identical(g_ddm$head, g_ddm_end$head)
log_line("DDM status unchanged: ", same_ddm)
if (!identical(unname(tools::md5sum(OLD_AXIS)), axis_md5_start)) {
  log_line("R/trial_time_axis.R md5 changed")
}

required <- c(
  "report.md", "A_refusals.csv", "A_recording_recovery.csv", "B_gate_flow_r1_vs_r2.csv",
  "C_landmarks.csv", "C_grand_waveform_r2.png", "D2_generation_agreement.csv", "D3_dec25_only_gain.csv",
  "E_r1_vs_r2.csv", "ch2_pupil_trials_r2.csv", "ch2_waveforms_subject_condition_r2.csv",
  "ch2_pupil_trials_dec25_r2.csv", "ch3_compatible_pupil_r2.csv", "ch3_compatible_gate_counts_r2.csv",
  "ch2_rebuild_250hz.R", "trial_time_axis_250hz.R", "test_trial_time_axis_250hz.R",
  "run_log.txt", "run.log"
)
stop_files <- list.files(DIAG, pattern = "^STOPPED_.*\\.md$")
required <- c(required, stop_files)

csv_check <- c(
  "A_refusals.csv", "A_recording_recovery.csv", "B_gate_flow_r1_vs_r2.csv", "C_landmarks.csv",
  "D2_generation_agreement.csv", "D3_dec25_only_gain.csv", "E_r1_vs_r2.csv",
  "ch2_pupil_trials_r2.csv", "ch2_waveforms_subject_condition_r2.csv",
  "ch2_pupil_trials_dec25_r2.csv", "ch3_compatible_pupil_r2.csv", "ch3_compatible_gate_counts_r2.csv"
)
header_only <- character()
for (bn in csv_check) {
  src_csv <- if (file.exists(file.path(DIAG, bn))) file.path(DIAG, bn) else file.path(DATA, bn)
  nlines <- length(readLines(src_csv, n = 2L, warn = FALSE))
  if (nlines < 2L) header_only <- c(header_only, bn)
}
if (length(header_only)) {
  stop_md("zip", "csv", paste("Header-only CSV:", paste(header_only, collapse = ", ")),
          "Checking that every CSV has a data row.", TRUE)
}

log_line("Packaging zip")
zip_path <- package_zip()
stopifnot(file.exists(zip_path))
uz <- system2("unzip", c("-t", zip_path), stdout = TRUE, stderr = TRUE)
writeLines(uz, file.path(DIAG, "zip_test.txt"))
zip_ok <- any(grepl("No errors detected", uz))
listed <- unzip(zip_path, list = TRUE)$Name
missing <- setdiff(required, basename(listed))
log_line("SELF-CHECK unzip -t passed: ", zip_ok)
log_line("SELF-CHECK cited files missing: ", if (length(missing)) paste(missing, collapse = ", ") else "none")
log_line("SELF-CHECK header-only CSV: none")
log_line("SELF-CHECK DDM unchanged: ", same_ddm)
log_line("Phase 2a.2 script finished")
stage <- file.path(DIAG, "zip_stage")
if (dir.exists(stage)) {
  file.copy(LOG, file.path(stage, "run_log.txt"), overwrite = TRUE)
  file.copy(file.path(DIAG, "run.log"), file.path(stage, "run.log"), overwrite = TRUE)
  file.copy(file.path(DIAG, "zip_test.txt"), file.path(stage, "zip_test.txt"), overwrite = TRUE)
  if (file.exists(zip_path)) unlink(zip_path)
  owd <- getwd()
  setwd(stage)
  utils::zip(zip_path, list.files("."))
  setwd(owd)
}
if (!zip_ok || length(missing) || !same_ddm) quit(status = 3L)
