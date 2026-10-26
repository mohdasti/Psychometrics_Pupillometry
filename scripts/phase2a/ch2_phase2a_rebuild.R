#!/usr/bin/env Rscript
# Chapter 2 Phase 2a pupil rebuild.
# No model fits. Flat files are listed non-recursively. Trial time comes only
# from build_trial_time(); a refused trial is stored as refused and has no
# pupil values.
#
# Functions reused verbatim from modeling-pupil-DDM/scripts/r8/B_rebuild_pupil.R:
#   compute_auc   lines 25-50
#   file_dt       lines 53-60
#   trial_time    lines 64-67
#   window_mean   lines 76-93
# Waveform binning follows scripts/r8/A_alignment.R add_trial() (lines 33-48)
# and latency() (lines 50-54). GRID is seq(-1, 8, by = 0.02).

options(warn = 1, scipen = 999)
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) stop("Usage: Rscript ch2_phase2a_rebuild.R RUN_ID")
RUN <- args[[1L]]

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
DDM <- "/Users/mohdasti/Documents/GitHub/modeling-pupil-DDM"
FLAT <- "/Users/mohdasti/Documents/LC-BAP/BAP/BAP_Pupillometry/BAP/BAP_processed"
PFSHARE <- file.path(CH2, "data/external/lc_psychometric_share")
DATA <- file.path(CH2, "data/analysis_ready/ch2_r1", RUN)
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2a", RUN)
dir.create(DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIAG, "checkpoints"), recursive = TRUE, showWarnings = FALSE)
setwd(CH2)

LOG <- file.path(DIAG, "run_log.txt")
log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
  flush(stdout())
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
  if (fatal) {
    try(package_zip(), silent = TRUE)
    quit(status = 2L)
  }
  invisible(path)
}

# --- verbatim Chapter 3 functions -------------------------------------------

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

file_dt <- function(time_vec) {
  d <- diff(sort(unique(time_vec[is.finite(time_vec)])))
  d <- d[is.finite(d) & d > 0]
  if (!length(d)) return(0.004)
  dt <- median(d)
  if (dt >= 1 && dt <= 10) dt <- dt / 1000
  if (!is.finite(dt) || dt <= 0) 0.004 else dt
}

file_dt_hit_empty <- function(time_vec) {
  d <- diff(sort(unique(time_vec[is.finite(time_vec)])))
  d <- d[is.finite(d) & d > 0]
  !length(d)
}

source(file.path(CH2, "R/trial_time_axis.R"))
trial_time <- function(n, dt, subject = NA, task = NA, session = NA, run = NA, trial = NA,
                       seg_start = NA_real_, seg_end = NA_real_) {
  build_trial_time(n, dt, seg_start, seg_end, subject, task, session, run, trial)
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

# A_alignment.R
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
latency <- function(mean_y, lo, hi, pick = which.min) {
  m <- GRID >= lo & GRID <= hi & is.finite(mean_y)
  if (!any(m)) return(NA_real_)
  GRID[m][pick(mean_y[m])]
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
    matched = logical(n), time_rule = NA_character_, axis_error = NA_character_,
    n = NA_integer_, dt_used = NA_real_, implied_end = NA_real_, native_span = NA_real_,
    b0_mean = NA_real_, n_b0 = NA_integer_, b0_quality = NA_real_,
    b1_mean = NA_real_, n_b1 = NA_integer_, b1_quality = NA_real_,
    tepr_auc = NA_real_, tepr_quality = NA_real_, tepr_mean_465_765 = NA_real_,
    tepr_mean_485_605 = NA_real_, q_485_605 = NA_real_,
    grip_mean = NA_real_, grip_quality = NA_real_,
    total_auc_b0 = NA_real_, total_quality = NA_real_,
    source_file = NA_character_, source_md5 = NA_character_
  )
}

# --- packaging --------------------------------------------------------------

package_zip <- function() {
  zip_path <- file.path(CH2, "output/diagnostics/ch2_phase2a", paste0("ch2_phase2a_", RUN, ".zip"))
  if (file.exists(zip_path)) unlink(zip_path)
  stage <- file.path(DIAG, "zip_stage")
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  diag_files <- list.files(DIAG, full.names = TRUE, recursive = FALSE)
  diag_files <- diag_files[!grepl("/checkpoints$|/zip_stage$", diag_files)]
  for (f in diag_files) if (file.exists(f) && !dir.exists(f)) file.copy(f, file.path(stage, basename(f)), overwrite = TRUE)
  for (bn in c("ch2_pupil_trials_r1.csv", "ch2_pupil_trials_r1_UNVALIDATED.csv", "ch2_waveforms_subject_condition_r1.csv")) {
    src <- file.path(DATA, bn)
    if (file.exists(src)) file.copy(src, file.path(stage, bn), overwrite = TRUE)
  }
  script <- file.path(CH2, "scripts/phase2a/ch2_phase2a_rebuild.R")
  if (file.exists(script)) file.copy(script, file.path(stage, basename(script)), overwrite = TRUE)
  axis <- file.path(CH2, "R/trial_time_axis.R")
  if (file.exists(axis)) file.copy(axis, file.path(stage, "trial_time_axis.R"), overwrite = TRUE)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(stage)
  utils::zip(zip_path, list.files("."))
  log_line("Zip ", zip_path, " bytes ", file.info(zip_path)$size)
  zip_path
}

# --- Part 0 -----------------------------------------------------------------

log_line("=== Phase 2a ", RUN, " ===")
log_line("date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
log_line("R: ", R.version.string)
writeLines(capture.output(sessionInfo()), file.path(DIAG, "sessionInfo.txt"))
git_ro <- function(repo) {
  list(
    head = paste(system2("git", c("-C", repo, "--no-optional-locks", "rev-parse", "HEAD"), stdout = TRUE), collapse = ""),
    status = system2("git", c("-C", repo, "--no-optional-locks", "status", "--porcelain"), stdout = TRUE)
  )
}
g_ch2 <- git_ro(CH2)
g_ddm <- git_ro(DDM)
log_line("CH2 HEAD ", g_ch2$head)
log_line("DDM HEAD ", g_ddm$head, " expected prefix c796631")
writeLines(g_ch2$status, file.path(DIAG, "0_status_ch2.txt"))
writeLines(g_ddm$status, file.path(DIAG, "0_status_ddm_start.txt"))
log_line("CH2 ", CH2)
log_line("DDM ", DDM)
log_line("FLAT ", FLAT)
log_line("PFSHARE ", PFSHARE)
log_line("DATA ", DATA)
log_line("DIAG ", DIAG)

V2 <- file.path(PFSHARE, "data/aggregated/bap_beh_trialdata_v2.csv")
AXIS_SRC <- file.path(DDM, "R/trial_time_axis.R")
AXIS_DST <- file.path(CH2, "R/trial_time_axis.R")
B_REBUILD <- file.path(DDM, "scripts/r8/B_rebuild_pupil.R")
A_ALIGN <- file.path(DDM, "scripts/r8/A_alignment.R")
R10 <- file.path(DDM, "data/analysis_ready/r10/ddm_pupil_r10.csv")
R5 <- file.path(DDM, "data/analysis_ready/r5/ddm_behavioral_r5_full17857.csv")
for (p in c(V2, AXIS_SRC, AXIS_DST, B_REBUILD, A_ALIGN, R10, R5, FLAT)) {
  if (!file.exists(p) && !dir.exists(p)) stop_md("0", "0.2", paste("Missing", p), "Checking Phase 2a inputs.", TRUE)
}

beh <- fread(V2, showProgress = FALSE)
need <- c("subject_id", "task_modality", "session_num", "run_num", "trial_num", "is_high_grip", "stim_level_index")
miss <- setdiff(need, names(beh))
if (length(miss)) stop_md("0", "0.2", paste("v2 missing columns:", paste(miss, collapse = ", ")), "Checking the v2 trial file.", TRUE)
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
  stop_md("0", "0.2", paste(
    "v2 checks failed. rows", nrow(beh), "aud subjects", n_aud, "vis subjects", n_vis,
    "sessions", paste(sort(unique(beh$session)), collapse = ","),
    "stim", paste(sort(unique(beh$stim_level_index)), collapse = ","),
    "trial max", max(beh$trial), "BAP140 ADT", bad_140, "BAP109/144", bad_excl
  ), "Checking the v2 trial file against the Phase 2a expectations.", TRUE)
}
if (anyDuplicated(beh, by = c("subject_id", "task", "session", "run", "trial")) > 0L) {
  stop_md("0", "0.2", "v2 has duplicate subject × task × session × run × trial keys.", "Checking v2 keys.", TRUE)
}

flats <- list.files(FLAT, pattern = "^BAP[0-9]+_(ADT|VDT)_flat\\.csv$", full.names = TRUE, recursive = FALSE)
if (any(grepl("build_|archive", flats))) {
  stop_md("0", "0.2", paste(flats[grepl("build_|archive", flats)], collapse = "\n"),
          "Listing top-level flat files. A path contains build_ or archive.", TRUE)
}
log_line("Flat files listed non-recursively: ", length(flats), " (expected 116; a difference is not a STOP)")

r10_head <- fread(R10, nrows = 0L)
r10_n <- as.integer(system(paste("wc -l <", shQuote(R10)), intern = TRUE)) - 1L
log_line("r10 rows ", r10_n, " columns ", paste(names(r10_head), collapse = ";"))

# 0.3 byte copy already on disk; confirm and test the copy.
if (!identical(unname(tools::md5sum(AXIS_SRC)), unname(tools::md5sum(AXIS_DST)))) {
  stop_md("0", "0.3", "md5 of R/trial_time_axis.R differs from the Chapter 3 source.",
          "Confirming the byte copy of the time-axis guard.", TRUE)
}
log_line("trial_time_axis.R md5 ", unname(tools::md5sum(AXIS_DST)))
test_out <- system2("Rscript", file.path(DDM, "tests/test_time_axis_guard.R"), stdout = TRUE, stderr = TRUE)
writeLines(test_out, file.path(DIAG, "0.3_time_axis_test.txt"))
if (!any(grepl("All time-axis guard tests passed", test_out))) {
  stop_md("0", "0.3", paste(tail(test_out, 30), collapse = "\n"),
          "Running test_time_axis_guard.R against the Chapter 2 copy.", TRUE)
}
log_line("Time-axis guard tests passed")

# 0.4 gate-fix writer
gate_hits <- system2("git", c("-C", DDM, "--no-optional-locks", "grep", "-l", "-e", "qc_new_keys", "--", "*.R"), stdout = TRUE, stderr = TRUE)
writeLines(c(
  "Searched the Chapter 3 working tree for a script that writes output/diagnostics/r8_gatefix/qc_new_keys.csv.",
  "scripts/r9/build_r9_sample.R reads that file. Neither the CSV nor a writer that assigns B1_new and cog_new was found.",
  paste("git grep hits:", paste(gate_hits, collapse = " | ")),
  "",
  "UNDETERMINED: the writer of qc_new_keys.csv is not in the working tree.",
  "P4 uses the prompt's expected definitions, which match scripts/r8/B_rebuild_pupil.R bounds:",
  "  B1_new / b1_quality = finite-sample proportion of pupil on [3.85, 4.35) (end exclusive), step axis.",
  "  cog_new / tepr_quality = finite-sample proportion of pupil on [4.65, 7.65] (both ends inclusive), step axis.",
  "scripts/r10/build_r10_sample.R states the r10 gate is the finite-sample proportion on [4.65, 7.65], threshold 0.50.",
  "P5 compares these columns with ddm_pupil_r10.csv."
), file.path(DIAG, "0.4_gate_fix.md"))
log_line("0.4 gate-fix writer UNDETERMINED; definitions recorded in 0.4_gate_fix.md")

manifest <- data.table(
  item = c("bap_beh_trialdata_v2.csv", "flat_list", "trial_time_axis.R", "B_rebuild_pupil.R", "A_alignment.R", "ddm_pupil_r10.csv", "ddm_behavioral_r5"),
  path = c(V2, FLAT, AXIS_DST, B_REBUILD, A_ALIGN, R10, R5),
  md5 = c(unname(tools::md5sum(V2)), NA_character_, unname(tools::md5sum(AXIS_DST)),
          unname(tools::md5sum(B_REBUILD)), unname(tools::md5sum(A_ALIGN)),
          unname(tools::md5sum(R10)), unname(tools::md5sum(R5))),
  note = c(
    paste("rows", nrow(beh), "aud", n_aud, "vis", n_vis),
    paste("n_files", length(flats)),
    "md5 equal to DDM source",
    "exists", "exists",
    paste("rows", r10_n),
    "exists"
  )
)
fwrite(manifest, file.path(DIAG, "manifest_inputs.csv"))
log_line("Wrote manifest_inputs.csv")

# --- P1 and P2 --------------------------------------------------------------

pairs <- unique(beh[, .(subject_id, task)])
setorder(pairs, subject_id, task)
log_line("v2 subject × task pairs: ", nrow(pairs))

empty_fallback <- character()
p1_rows <- list()
feat_rows <- list()
wave_rows <- list()
p3_rows <- list()

for (i in seq_len(nrow(pairs))) {
  subj <- pairs$subject_id[i]
  tsk <- pairs$task[i]
  stamp <- file.path(DIAG, "checkpoints", paste0(subj, "_", tsk, ".rds"))
  if (file.exists(stamp)) {
    prev <- readRDS(stamp)
    p1_rows[[i]] <- prev$p1
    feat_rows[[i]] <- prev$feat
    wave_rows[[i]] <- prev$wave
    p3_rows[[i]] <- prev$p3
    if (isTRUE(prev$dt_empty)) empty_fallback <- c(empty_fallback, prev$path)
    next
  }
  path <- file.path(FLAT, sprintf("%s_%s_flat.csv", subj, tsk))
  wanted <- beh[subject_id == subj & task == tsk,
                .(subject_id, task, session, run, trial, effort, stim_level_index, high_grip)]
  log_line("P2 ", i, "/", nrow(pairs), " ", basename(path), " v2 trials ", nrow(wanted))
  if (!file.exists(path)) {
    feat <- cbind(wanted, empty_feat(nrow(wanted)))
    feat[, matched := FALSE]
    p1 <- data.table(subject_id = subj, task = tsk, v2_trials = nrow(wanted), flat_present = FALSE,
                     matched = 0L, unmatched = nrow(wanted), flat_trials_not_in_v2 = NA_integer_)
    saveRDS(list(p1 = p1, feat = feat, wave = NULL, p3 = NULL, dt_empty = FALSE, path = path), stamp)
    p1_rows[[i]] <- p1
    feat_rows[[i]] <- feat
    next
  }
  hdr <- names(fread(path, nrows = 0L))
  need_f <- c("session_used", "run_used", "trial_in_run_raw", "time", "pupil")
  miss_f <- setdiff(need_f, hdr)
  if (length(miss_f)) stop("Flat file is missing column(s): ", paste(miss_f, collapse = ", "), " in ", path, call. = FALSE)
  dt <- fread(path, select = need_f, showProgress = FALSE)
  setnames(dt, c("session_used", "run_used", "trial_in_run_raw"), c("session", "run", "trial"))
  dt[, `:=`(session = as.integer(session), run = as.integer(run), trial = as.integer(trial),
            time = as.numeric(time), pupil = as.numeric(pupil))]
  dt <- dt[session %in% c(2L, 3L)]
  md5 <- unname(tools::md5sum(path))
  hit_empty <- file_dt_hit_empty(dt$time)
  if (hit_empty) {
    writeLines(path, file.path(DIAG, "P2_file_dt_empty_branch.txt"))
    stop_md("0", "0.2", paste("file_dt() empty-difference branch (expected 0) in", path),
            "Counting files for which file_dt returns 0.004 because no positive time difference exists.", TRUE)
  }
  dt_used <- file_dt(dt$time)
  flat_keys <- unique(dt[, .(session, run, trial)])
  flat_not <- flat_keys[!wanted, on = c("session", "run", "trial")]
  multi <- dt[, .(n_rows = .N), by = .(session, run, trial)]
  # one group per key by construction; a v2 key cannot match two groups in one file
  if (anyDuplicated(flat_keys, by = c("session", "run", "trial")) > 0L) {
    stop_md("P1", "P1", paste("Duplicate flat keys in", path), "Matching v2 trials to flat trials.", TRUE)
  }
  kept <- dt[wanted, on = c("session", "run", "trial"), nomatch = NULL, allow.cartesian = FALSE]
  if (nrow(kept)) {
    built <- kept[, {
      n_s <- .N
      ax <- tryCatch(
        trial_time(n_s, dt_used, subj, tsk, session[1], run[1], trial[1]),
        error = function(e) e
      )
      span <- if (all(!is.finite(time))) NA_real_ else max(time, na.rm = TRUE) - min(time, na.rm = TRUE)
      if (inherits(ax, "error")) {
        t_ref <- seq(from = -3.0, by = dt_used, length.out = n_s)
        list(matched = TRUE, time_rule = "refused", axis_error = conditionMessage(ax),
             n = n_s, dt_used = dt_used, implied_end = t_ref[n_s], native_span = span,
             b0_mean = NA_real_, n_b0 = NA_integer_, b0_quality = NA_real_,
             b1_mean = NA_real_, n_b1 = NA_integer_, b1_quality = NA_real_,
             tepr_auc = NA_real_, tepr_quality = NA_real_, tepr_mean_465_765 = NA_real_,
             tepr_mean_485_605 = NA_real_, q_485_605 = NA_real_,
             grip_mean = NA_real_, grip_quality = NA_real_,
             total_auc_b0 = NA_real_, total_quality = NA_real_)
      } else {
        tt <- ax$t
        b0s <- win_stats(tt, pupil, -0.5, 0, TRUE)
        b0 <- if (b0s$n >= 10L) mean(b0s$vals[is.finite(b0s$vals)]) else NA_real_
        b1s <- win_stats(tt, pupil, 3.85, 4.35, TRUE)
        b1 <- if (b1s$n >= 10L) mean(b1s$vals[is.finite(b1s$vals)]) else NA_real_
        te <- win_stats(tt, pupil, 4.65, 7.65, FALSE)
        tepr <- if (is.finite(b1)) compute_auc(tt[tt >= 4.65 & tt <= 7.65], pupil[tt >= 4.65 & tt <= 7.65] - b1) else NA_real_
        te_mean <- if (is.finite(b1) && te$n >= 1L) mean((te$vals - b1)[is.finite(te$vals)]) else NA_real_
        w485 <- win_stats(tt, pupil, 4.85, 6.05, FALSE)
        m485 <- if (is.finite(b1) && w485$n >= 1L) mean((w485$vals - b1)[is.finite(w485$vals)]) else NA_real_
        grip <- window_mean(tt, pupil, b0, 1.0, 4.35)
        tot <- win_stats(tt, pupil, 0, 4.70, FALSE)
        tot_auc <- if (is.finite(b0)) compute_auc(tt[tt >= 0 & tt <= 4.70], pupil[tt >= 0 & tt <= 4.70] - b0) else NA_real_
        list(matched = TRUE, time_rule = ax$rule, axis_error = NA_character_,
             n = n_s, dt_used = dt_used, implied_end = ax$implied_end, native_span = span,
             b0_mean = b0, n_b0 = b0s$n, b0_quality = b0s$prop,
             b1_mean = b1, n_b1 = b1s$n, b1_quality = b1s$prop,
             tepr_auc = tepr, tepr_quality = te$prop, tepr_mean_465_765 = te_mean,
             tepr_mean_485_605 = m485, q_485_605 = w485$prop,
             grip_mean = grip$mean, grip_quality = grip$prop,
             total_auc_b0 = tot_auc, total_quality = tot$prop)
      }
    }, by = .(session, run, trial)]
    built[, `:=`(source_file = path, source_md5 = md5)]
    feat <- merge(wanted, built, by = c("session", "run", "trial"), all.x = TRUE)
    feat[is.na(matched), matched := FALSE]
  } else {
    feat <- cbind(wanted, empty_feat(nrow(wanted)))
    feat[, matched := FALSE]
  }
  # waveforms and P3 traces for accepted trials only
  accepted <- feat[matched %in% TRUE & time_rule != "refused"]
  cell_wave <- NULL
  p3 <- list(step = new_wave(), native = new_wave())
  feat[, `:=`(source_file = path, source_md5 = md5)]
  if (nrow(accepted)) {
    acc_dt <- merge(dt, accepted[, .(session, run, trial, effort, stim_level_index)],
                    by = c("session", "run", "trial"), allow.cartesian = FALSE)
    pieces <- split(seq_len(nrow(acc_dt)), paste(acc_dt$session, acc_dt$run, acc_dt$trial))
    cell_acc <- list()
    for (ix in pieces) {
      tr <- acc_dt[ix]
      ax <- trial_time(nrow(tr), dt_used, subj, tsk, tr$session[1], tr$run[1], tr$trial[1])
      key <- paste(tr$effort[1], tr$stim_level_index[1])
      if (is.null(cell_acc[[key]])) cell_acc[[key]] <- new_wave()
      cell_acc[[key]] <- add_trial(ax$t, tr$pupil, cell_acc[[key]])
      p3$step <- add_trial(ax$t, tr$pupil, p3$step)
      p3$native <- add_trial(tr$time, tr$pupil, p3$native)
    }
    if (length(cell_acc)) {
      cell_wave <- rbindlist(lapply(names(cell_acc), function(k) {
        w <- cell_acc[[k]]
        parts <- strsplit(k, " ", fixed = TRUE)[[1]]
        data.table(subject_id = subj, task = tsk, effort = parts[[1]], stim_level_index = as.integer(parts[[2]]),
                   bin = seq_along(GRID), sum_y = w$sums, n_bin = w$counts, n_trials = w$n_trials)
      }))
    }
  }
  p1 <- data.table(
    subject_id = subj, task = tsk, v2_trials = nrow(wanted), flat_present = TRUE,
    matched = sum(feat$matched %in% TRUE), unmatched = sum(!feat$matched %in% TRUE),
    flat_trials_not_in_v2 = nrow(flat_not)
  )
  prev <- list(p1 = p1, feat = feat, wave = cell_wave, p3 = p3, dt_empty = hit_empty, path = path)
  saveRDS(prev, stamp)
  p1_rows[[i]] <- p1
  feat_rows[[i]] <- feat
  wave_rows[[i]] <- cell_wave
  p3_rows[[i]] <- p3
  rm(dt)
  if (i %% 20L == 0L) {
    log_line("Checkpoint after ", i, " subject × task files")
    gc(verbose = FALSE)
  }
}

if (length(empty_fallback)) {
  stop_md("0", "0.2", paste(length(empty_fallback), "checkpointed files hit file_dt()'s empty-difference branch:\n",
                            paste(empty_fallback, collapse = "\n")),
          "Counting files for which file_dt returns 0.004 because no positive time difference exists.", TRUE)
}
log_line("file_dt empty-difference branch: 0 files")

p1 <- rbindlist(p1_rows)
fwrite(p1, file.path(DIAG, "P1_key_match_by_subject_task.csv"))
multi_match <- p1[matched > v2_trials]
if (nrow(multi_match)) {
  stop_md("P1", "P1", paste(capture.output(print(multi_match)), collapse = "\n"),
          "Checking that no v2 key matches more than one flat trial.", TRUE)
}

trials <- rbindlist(feat_rows, fill = TRUE)
if ("high_grip" %in% names(trials)) setnames(trials, "high_grip", "is_high_grip")
if (nrow(trials) != 18266L) {
  stop_md("P2", "P2", paste("Row count", nrow(trials), "expected 18266"),
          "Checking accepted + refused + unmatched.", TRUE)
}
n_acc <- trials[matched %in% TRUE & time_rule != "refused", .N]
n_ref <- trials[time_rule == "refused", .N]
n_un <- trials[!matched %in% TRUE, .N]
if (n_acc + n_ref + n_un != 18266L) {
  stop_md("P2", "P2", paste("accepted", n_acc, "refused", n_ref, "unmatched", n_un),
          "Checking accepted + refused + unmatched = 18266.", TRUE)
}
log_line("P2 accepted ", n_acc, " refused ", n_ref, " unmatched ", n_un,
         " refused pct ", round(100 * n_ref / 18266, 2))

ref <- trials[time_rule == "refused"]
fwrite(ref[, .(subject_id, task, session, run, trial, n, dt_used, implied_end, native_span,
               end_lt_8 = implied_end < 8, end_gt_15 = implied_end > 15, axis_error)],
       file.path(DIAG, "P2_refused_trials.csv"))
if (nrow(ref)) {
  ref_sum <- ref[, .(
    n_refused = .N,
    implied_end_min = min(implied_end),
    implied_end_median = median(implied_end),
    implied_end_max = max(implied_end),
    n_end_lt_8 = sum(implied_end < 8),
    n_end_gt_15 = sum(implied_end > 15)
  ), by = .(subject_id, task)]
} else {
  ref_sum <- data.table(subject_id = character(), task = character(), n_refused = integer(),
                        implied_end_min = numeric(), implied_end_median = numeric(), implied_end_max = numeric(),
                        n_end_lt_8 = integer(), n_end_gt_15 = integer())
}
fwrite(ref_sum, file.path(DIAG, "P2_refused_summary.csv"))
log_line("Refused by task: ", paste(capture.output(print(trials[, .(
  n = .N, refused = sum(time_rule %in% "refused"), pct = round(100 * sum(time_rule %in% "refused") / .N, 2)
), by = task])), collapse = " | "))

# waveforms
waves <- rbindlist(wave_rows, fill = TRUE)
if (nrow(waves)) {
  wave_out <- waves[, .(
    sum_y = sum(sum_y), n_bin = sum(n_bin), n_trials = max(n_trials)
  ), by = .(subject_id, task, effort, stim_level_index, bin)]
  wave_out[, t := GRID[bin]]
  wave_out[, mean_b0 := ifelse(n_bin > 0, sum_y / n_bin, NA_real_)]
  fwrite(wave_out[, .(subject_id, task, effort, stim_level_index, t, mean_b0, n = n_bin, n_trials)],
         file.path(DATA, "ch2_waveforms_subject_condition_r1.csv"))
} else {
  fwrite(data.table(subject_id = character(), task = character(), effort = character(),
                    stim_level_index = integer(), t = numeric(), mean_b0 = numeric(),
                    n = integer(), n_trials = integer()),
         file.path(DATA, "ch2_waveforms_subject_condition_r1.csv"))
}
log_line("Wrote waveforms")

# --- P3 ---------------------------------------------------------------------

p3_tab <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  p3 <- p3_rows[[i]]
  if (is.null(p3)) return(NULL)
  rbindlist(lapply(c("step", "native"), function(ax) {
    w <- p3[[ax]]
    mean_y <- ifelse(w$counts > 0, w$sums / w$counts, NA_real_)
    data.table(
      subject_id = pairs$subject_id[i], task = pairs$task[i], axis = ax,
      n_trials = w$n_trials,
      dip = latency(mean_y, 2.5, 3.8, which.min),
      dip_wide = latency(mean_y, 1.5, 5.5, which.min),
      peak = latency(mean_y, 4.35, 7.65, which.max)
    )
  }))
}), fill = TRUE)
fwrite(p3_tab, file.path(DIAG, "P3_light_reflex_by_recording.csv"))
p3_sum <- p3_tab[, .(
  n_recordings = .N,
  n_finite_dip = sum(is.finite(dip)),
  dip_median = median(dip, na.rm = TRUE),
  dip_iqr = IQR(dip, na.rm = TRUE),
  dip_wide_median = median(dip_wide, na.rm = TRUE),
  dip_wide_iqr = IQR(dip_wide, na.rm = TRUE),
  peak_median = median(peak, na.rm = TRUE),
  peak_iqr = IQR(peak, na.rm = TRUE)
), by = .(axis, task)]
p3_all <- p3_tab[, .(
  n_recordings = .N,
  n_finite_dip = sum(is.finite(dip)),
  dip_median = median(dip, na.rm = TRUE),
  dip_iqr = IQR(dip, na.rm = TRUE)
), by = axis]
p3_out <- rbind(p3_sum, p3_all[, .(axis, task = "ALL", n_recordings, n_finite_dip, dip_median, dip_iqr,
                                   dip_wide_median = NA_real_, dip_wide_iqr = NA_real_,
                                   peak_median = NA_real_, peak_iqr = NA_real_)], fill = TRUE)
p3_out[, median_denominator := "recordings with a finite dip (n_finite_dip). Recordings with NA dip stay in n_recordings and are excluded from the median and IQR."]
fwrite(p3_out, file.path(DIAG, "P3_light_reflex_summary.csv"))
step_med <- p3_all[axis == "step"]$dip_median
log_line("P3 step-axis median dip ", step_med, " n finite ", p3_all[axis == "step"]$n_finite_dip,
         " of ", p3_all[axis == "step"]$n_recordings)

grand <- lapply(c("ADT", "VDT"), function(tsk) {
  sums <- numeric(length(GRID))
  counts <- integer(length(GRID))
  ntr <- 0L
  for (i in which(pairs$task == tsk)) {
    p3 <- p3_rows[[i]]
    if (is.null(p3)) next
    sums <- sums + p3$step$sums
    counts <- counts + p3$step$counts
    ntr <- ntr + p3$step$n_trials
  }
  list(task = tsk, y = ifelse(counts > 0, sums / counts, NA_real_), ntr = ntr)
})
png(file.path(DIAG, "P3_grand_waveform.png"), width = 1100, height = 700)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
for (g in grand) {
  plot(GRID, g$y, type = "l", xlab = "Time (s)", ylab = "B0-corrected pupil",
       main = paste(g$task, "step axis, trials", g$ntr))
  usr <- par("usr")
  rect(3.85, usr[3], 4.35, usr[4], col = rgb(0.7, 0.7, 0.7, 0.35), border = NA)
  rect(4.65, usr[3], 7.65, usr[4], col = rgb(0.2, 0.4, 0.8, 0.18), border = NA)
  rect(4.85, usr[3], 6.05, usr[4], col = rgb(0.9, 0.5, 0.2, 0.18), border = NA)
  lines(GRID, g$y)
  abline(v = c(3.0, 3.12, 3.75, 4.35, 4.70), lty = c(2, 1, 3, 1, 2))
}
dev.off()
par(op)
log_line("Wrote P3_grand_waveform.png")

fwrite(trials, file.path(DATA, "ch2_pupil_trials_r1.csv"))
if (length(step_med) != 1L || !is.finite(step_med) || step_med < 3.00 || step_med > 3.30) {
  writeLines(c(
    "# Chapter 2 Phase 2a pupil rebuild",
    "",
    "## 1. Summary",
    "",
    paste0("Part 0: PASS. Flat files ", length(flats), "."),
    paste0("P1: PASS. Rows ", nrow(trials), ". accepted ", n_acc, ", refused ", n_ref, ", unmatched ", n_un, "."),
    paste0("P2: PASS. Refused ", n_ref, "."),
    paste0("P3: FAIL. Step-axis median dip ", step_med, " s. Band is [3.00, 3.30]."),
    "P4: not run.",
    "P5: not run.",
    "P6: not run.",
    "P7: not run.",
    "P8: not run.",
    "",
    "Ready for Phase 2b? NO",
    "",
    "## 2. Headline answers",
    "",
    paste0("(i) Step-axis median dip ", step_med, " s. See P3_light_reflex_summary.csv."),
    paste0("(ii) Refused trials ", n_ref, ". See P2_refused_trials.csv."),
    "(iii) Chapter 3 parity was not computed because P3 stopped the run.",
    "(iv) Gates were not applied.",
    "(v) Old versus new agreement was not computed.",
    "",
    "## 3. Parts",
    "",
    "P3 failed the light-reflex check. The trial table was renamed ch2_pupil_trials_r1_UNVALIDATED.csv.",
    "",
    "## 4. Deviations",
    "",
    "Later parts did not run.",
    "",
    "## 5. UNDETERMINED",
    "",
    "P4 through P8 were not computed."
  ), file.path(DIAG, "report.md"))
  trial_path <- file.path(DATA, "ch2_pupil_trials_r1.csv")
  file.rename(trial_path, file.path(DATA, "ch2_pupil_trials_r1_UNVALIDATED.csv"))
  stop_md("P3", "P3", paste(capture.output(print(fread(file.path(DIAG, "P3_light_reflex_summary.csv")))), collapse = "\n"),
          "Step-axis median light-reflex dip across recordings. Expected inside [3.00, 3.30] s.", TRUE)
}

# --- P4 gates ---------------------------------------------------------------

trials[, g_tepr_1 := matched %in% TRUE & time_rule != "refused" & is.finite(tepr_auc)]
trials[, g_tepr_2 := g_tepr_1 & is.finite(b1_quality) & b1_quality >= 0.50]
trials[, g_tepr_3 := g_tepr_2 & is.finite(tepr_quality) & tepr_quality >= 0.50]
tepr_cells <- trials[g_tepr_3 %in% TRUE, .N, by = .(subject_id, task)]
tepr_keep <- tepr_cells[N >= 20]
trials[, g_tepr_final := g_tepr_3 %in% TRUE & paste(subject_id, task) %in% paste(tepr_keep$subject_id, tepr_keep$task)]
trials[, g_grip_1 := matched %in% TRUE & time_rule != "refused" & is.finite(grip_mean)]
trials[, g_grip_2 := g_grip_1 & is.finite(b0_quality) & b0_quality >= 0.50]
trials[, g_grip_3 := g_grip_2 & is.finite(grip_quality) & grip_quality >= 0.50]
grip_cells <- trials[g_grip_3 %in% TRUE, .N, by = .(subject_id, task)]
grip_keep <- grip_cells[N >= 20]
trials[, g_grip_final := g_grip_3 %in% TRUE & paste(subject_id, task) %in% paste(grip_keep$subject_id, grip_keep$task)]

gate_flow <- rbindlist(lapply(c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final", "g_grip_1", "g_grip_2", "g_grip_3", "g_grip_final"), function(g) {
  trials[get(g) %in% TRUE, .(gate = g, n_trials = .N, n_subjects = uniqueN(subject_id)), by = .(task, effort)]
}))
fwrite(gate_flow, file.path(DIAG, "P4_gate_flow.csv"))
fwrite(trials, file.path(DATA, "ch2_pupil_trials_r1.csv"))
log_line("Wrote trial table and P4_gate_flow.csv")

# --- P5 ---------------------------------------------------------------------

r10 <- fread(R10, select = c("subject_id", "task", "session", "run", "trial", "tepr_raw", "baseline_raw", "B1_quality", "cog_quality"))
r10[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
           session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
map_note <- c(
  "Join keys: subject_id, task (ADT/VDT already), session, run, trial.",
  "tepr_auc compared with r10 tepr_raw.",
  "b1_quality compared with r10 B1_quality.",
  "tepr_quality compared with r10 cog_quality (r10 overwrites this with the [4.65, 7.65] finite-sample proportion).",
  "b0_mean compared with r10 baseline_raw."
)
writeLines(map_note, file.path(DIAG, "P5_column_mapping.txt"))
shared <- merge(
  trials[, .(subject_id, task, session, run, trial, tepr_auc, b1_quality, tepr_quality, b0_mean, source_file,
             matched, time_rule, g_tepr_1, g_tepr_2, g_tepr_3, g_tepr_final)],
  r10, by = c("subject_id", "task", "session", "run", "trial"), all = FALSE
)
pair_stat <- function(a, b, name) {
  both <- is.finite(a) & is.finite(b)
  diff <- abs(a - b)
  data.table(
    comparison = name, n_shared = length(a), n_both_finite = sum(both),
    n_both_na = sum(!is.finite(a) & !is.finite(b)),
    n_ch2_na_only = sum(!is.finite(a) & is.finite(b)),
    n_r10_na_only = sum(is.finite(a) & !is.finite(b)),
    pct_lt_1e9 = 100 * sum(both & diff < 1e-9) / length(a),
    pct_among_finite = if (any(both)) 100 * sum(diff[both] < 1e-9) / sum(both) else NA_real_,
    max_abs = if (any(both)) max(diff[both]) else NA_real_
  )
}
parity <- rbind(
  pair_stat(shared$tepr_auc, shared$tepr_raw, "tepr_auc vs tepr_raw"),
  pair_stat(shared$b1_quality, shared$B1_quality, "b1_quality vs B1_quality"),
  pair_stat(shared$tepr_quality, shared$cog_quality, "tepr_quality vs cog_quality"),
  pair_stat(shared$b0_mean, shared$baseline_raw, "b0_mean vs baseline_raw")
)
parity[, n_shared_trials := nrow(shared)]
fwrite(parity, file.path(DIAG, "P5_r10_parity.csv"))
log_line("P5 shared ", nrow(shared))
log_line(paste(capture.output(print(parity)), collapse = "\n"))
if (any(parity$pct_lt_1e9 < 99.9)) {
  worst <- rbindlist(lapply(seq_len(nrow(parity)), function(i) {
    nm <- parity$comparison[i]
    if (nm == "tepr_auc vs tepr_raw") d <- abs(shared$tepr_auc - shared$tepr_raw)
    else if (nm == "b1_quality vs B1_quality") d <- abs(shared$b1_quality - shared$B1_quality)
    else if (nm == "tepr_quality vs cog_quality") d <- abs(shared$tepr_quality - shared$cog_quality)
    else d <- abs(shared$b0_mean - shared$baseline_raw)
    ord <- order(ifelse(is.finite(d), d, Inf), decreasing = TRUE)
    take <- head(ord, 50L)
    data.table(comparison = nm, abs_diff = d[take], shared[take, .(subject_id, task, session, run, trial, source_file)])
  }))
  fwrite(worst, file.path(DIAG, "P5_worst50.csv"))
  not_ok <- function(a, b) !(is.finite(a) & is.finite(b) & abs(a - b) < 1e-9)
  fail_ix <- which(
    not_ok(shared$tepr_auc, shared$tepr_raw) |
    not_ok(shared$b1_quality, shared$B1_quality) |
    not_ok(shared$tepr_quality, shared$cog_quality) |
    not_ok(shared$b0_mean, shared$baseline_raw)
  )
  fail_rows <- if (length(fail_ix)) shared[fail_ix] else shared[0L]
  clus <- rbind(
    fail_rows[, .(level = "subject", key = subject_id, n = .N), by = .(subject_id)][, .(level, key = subject_id, n)],
    fail_rows[, .(level = "file", key = source_file, n = .N), by = source_file][, .(level, key = source_file, n)]
  )
  fwrite(clus[order(-n)], file.path(DIAG, "P5_difference_clusters.csv"))
  stop_md("P5", "P5", paste(capture.output(print(parity)), collapse = "\n"),
          "Chapter 3 parity on shared trials. At least one comparison is below 99.9%.", FALSE)
}

r5 <- fread(R5)
r5_need <- c("subject_id", "task", "session", "run", "trial")
if (!all(r5_need %in% names(r5))) stop("r5 behavioral file is missing column(s): ", paste(setdiff(r5_need, names(r5)), collapse = ", "), call. = FALSE)
r5[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
          session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
keys5 <- c("subject_id", "task", "session", "run", "trial")
v2_keys <- trials[, ..keys5]
r5_keys <- unique(r5[, ..keys5])
both_vr <- merge(v2_keys, r5_keys, by = keys5)
v2_only <- fsetdiff(v2_keys, r5_keys, all = FALSE)
r5_only <- fsetdiff(r5_keys, v2_keys, all = FALSE)
v2r5 <- rbind(
  both_vr[, .(set = "shared", n = .N), by = task],
  v2_only[, .(set = "v2_only", n = .N), by = task],
  r5_only[, .(set = "r5_only", n = .N), by = task]
)
fwrite(v2r5, file.path(DIAG, "P5_v2_vs_r5.csv"))
log_line("v2 vs r5 shared ", nrow(both_vr), " v2-only ", nrow(v2_only), " r5-only ", nrow(r5_only))

ch2_final <- trials[g_tepr_final %in% TRUE, ..keys5]
r10_keys <- r10[, ..keys5]
only_ch2 <- fsetdiff(ch2_final, r10_keys)
only_r10 <- fsetdiff(r10_keys, ch2_final)
classify <- function(keys, direction) {
  if (!nrow(keys)) return(data.table(direction = character(), task = character(), reason = character(), n = integer()))
  m <- merge(keys, trials[, c(keys5, "matched", "time_rule", "g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final"), with = FALSE],
             by = keys5, all.x = TRUE)
  in_r5 <- merge(m[, ..keys5], r5_keys, by = keys5)
  m[, in_r5 := FALSE]
  m[in_r5, on = keys5, in_r5 := TRUE]
  m[, reason := "UNDETERMINED"]
  if (direction == "ch2_not_r10") m[in_r5 == FALSE, reason := "not in Chapter 3 behavioral base (r5)"]
  m[reason == "UNDETERMINED" & !matched %in% TRUE, reason := "not in v2 flat-file match"]
  m[reason == "UNDETERMINED" & time_rule == "refused", reason := "refused axis"]
  m[reason == "UNDETERMINED" & !g_tepr_1 %in% TRUE, reason := "failed g_tepr_1"]
  m[reason == "UNDETERMINED" & !g_tepr_2 %in% TRUE, reason := "failed g_tepr_2 (b1_quality < 0.50)"]
  m[reason == "UNDETERMINED" & !g_tepr_3 %in% TRUE, reason := "failed g_tepr_3 (tepr_quality < 0.50)"]
  m[reason == "UNDETERMINED" & !g_tepr_final %in% TRUE, reason := "under the 20-trial cell rule"]
  if (direction == "r10_not_ch2") m[reason == "UNDETERMINED", reason := "in r10 but Chapter 2 g_tepr_final is true; join key differed"]
  m[, .(direction = direction, n = .N), by = .(task, reason)]
}
cov <- rbind(classify(only_ch2, "ch2_not_r10"), classify(only_r10, "r10_not_ch2"), fill = TRUE)
if (!nrow(cov)) {
  cov <- data.table(direction = "none", task = "", reason = "g_tepr_final and r10 contain the same trials", n = 0L)
}
fwrite(cov, file.path(DIAG, "P5_coverage_reasons.csv"))
log_line("ch2 final not in r10: ", nrow(only_ch2), "; r10 not in ch2 final: ", nrow(only_r10))

# --- P6 ---------------------------------------------------------------------

flat_present <- p1[flat_present == TRUE, .(subject_id, task)]
matched_any <- trials[matched %in% TRUE, .(hit = TRUE), by = .(subject_id, task)]
tepr_sub <- trials[g_tepr_final %in% TRUE, .(hit = TRUE), by = .(subject_id, task)]
grip_sub <- trials[g_grip_final %in% TRUE, .(hit = TRUE), by = .(subject_id, task)]
cov6 <- unique(trials[, .(subject_id, task)])
cov6[, in_v2 := TRUE]
cov6[flat_present, on = c("subject_id", "task"), flat_file_present := TRUE]
cov6[is.na(flat_file_present), flat_file_present := FALSE]
cov6[matched_any, on = c("subject_id", "task"), ge1_matched := TRUE]
cov6[is.na(ge1_matched), ge1_matched := FALSE]
cov6[tepr_sub, on = c("subject_id", "task"), g_tepr_final := TRUE]
cov6[is.na(g_tepr_final), g_tepr_final := FALSE]
cov6[grip_sub, on = c("subject_id", "task"), g_grip_final := TRUE]
cov6[is.na(g_grip_final), g_grip_final := FALSE]
fwrite(cov6, file.path(DIAG, "P6_participant_coverage.csv"))

phase1_cases <- data.table(
  subject_id = c("BAP122", "BAP139", "BAP186", "BAP191", "BAP194", "BAP195", "BAP101", "BAP103", "BAP114"),
  task = c(rep("ADT", 6), rep("VDT", 3))
)
sess_rows <- rbindlist(lapply(seq_len(nrow(phase1_cases)), function(i) {
  subj <- phase1_cases$subject_id[i]
  tsk <- phase1_cases$task[i]
  path <- file.path(FLAT, sprintf("%s_%s_flat.csv", subj, tsk))
  v2_has <- nrow(beh[subject_id == subj & task == tsk]) > 0L
  if (!file.exists(path)) {
    return(data.table(subject_id = subj, task = tsk, session_used = NA_integer_, n_rows = NA_integer_,
                      v2_has_trials = v2_has, note = "flat file absent"))
  }
  ss <- fread(path, select = "session_used", showProgress = FALSE)
  tab <- ss[, .N, by = session_used]
  tab[, `:=`(subject_id = subj, task = tsk, v2_has_trials = v2_has, note = "")]
  setnames(tab, "N", "n_rows")
  tab[, .(subject_id, task, session_used, n_rows, v2_has_trials, note)]
}))
fwrite(sess_rows, file.path(DIAG, "P6_missing_session_detail.csv"))

missing_ids <- c("BAP001", "BAP111", "BAP165", "BAP189", "BAP190", "BAP192", "BAP197")
all_names <- list.files(FLAT, recursive = TRUE, full.names = TRUE)
search_rows <- rbindlist(lapply(missing_ids, function(id) {
  hit <- all_names[grepl(paste0("(^|/)", id, "([^0-9]|$)"), all_names)]
  if (!length(hit)) return(data.table(subject_id = id, path = "none", mtime = NA_character_, note = "no filename contains this id"))
  info <- file.info(hit)
  data.table(subject_id = id, path = hit, mtime = format(info$mtime, "%Y-%m-%d %H:%M:%S"), note = "not used")
}))
fwrite(search_rows, file.path(DIAG, "P6_missing_flat_search.csv"))
log_line("P6 written")

# --- P7 ---------------------------------------------------------------------

old <- fread(file.path(CH2, "data/processed/ch2_triallevel_merged.csv"),
             select = c("sub", "task", "session_used", "run_used", "trial_index", "cog_auc", "total_auc"))
setnames(old, c("sub", "session_used", "run_used", "trial_index"), c("subject_id", "session", "run", "trial"))
old[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
           session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
b2 <- fread(file.path(CH2, "output/diagnostics/ch2_phase1_audit/local_20261024/B2_trial_source.csv"),
            select = c("sub", "task", "session_used", "run_used", "trial_index", "fallback_triggered", "generation"))
setnames(b2, c("sub", "session_used", "run_used", "trial_index"), c("subject_id", "session", "run", "trial"))
b2[, `:=`(subject_id = as.character(subject_id), task = as.character(task),
          session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
j7 <- merge(trials, old, by = keys5, all = FALSE)
j7 <- merge(j7, b2, by = keys5, all.x = TRUE)
j7[is.na(generation), generation := "not_in_phase1_B2"]
j7[is.na(fallback_triggered), fallback_triggered := NA]
cor_row <- function(a, b, label, split) {
  ok <- is.finite(a) & is.finite(b)
  data.table(comparison = label, split = split, n = sum(ok),
             pearson_r = if (sum(ok) > 2L) cor(a[ok], b[ok]) else NA_real_)
}
splits <- list(
  overall = rep(TRUE, nrow(j7)),
  fallback_TRUE = j7$fallback_triggered %in% TRUE,
  fallback_FALSE = j7$fallback_triggered %in% FALSE
)
for (g in unique(j7$generation)) splits[[paste0("generation_", g)]] <- j7$generation == g
complete_q <- is.finite(j7$tepr_quality) & j7$tepr_quality == 1
p7 <- rbindlist(lapply(names(splits), function(sp) {
  m <- splits[[sp]]
  rbind(
    cor_row(j7$cog_auc[m], j7$tepr_auc[m], "old cog_auc vs new tepr_auc", sp),
    cor_row(j7$cog_auc[m], j7$tepr_mean_485_605[m], "old cog_auc vs new tepr_mean_485_605", sp),
    cor_row(j7$tepr_auc[m], j7$tepr_mean_465_765[m], "new tepr_auc vs tepr_mean_465_765", sp),
    cor_row(j7$total_auc[m], j7$total_auc_b0[m], "old total_auc vs new total_auc_b0", sp)
  )
}))
p7 <- rbind(p7, cor_row(j7$tepr_auc[complete_q], j7$tepr_mean_465_765[complete_q],
                        "new tepr_auc vs tepr_mean_465_765", "tepr_quality_eq_1"))
fwrite(p7, file.path(DIAG, "P7_old_vs_new.csv"))
log_line("Wrote P7")

# --- P8 references ----------------------------------------------------------

qmd_path <- file.path(CH2, "07_manuscript/chapter2/reports/chapter2_dissertation.qmd")
stopifnot(file.exists(qmd_path))
qmd <- readLines(qmd_path, warn = FALSE)
in_chunk <- FALSE
keep <- character()
for (ln in qmd) {
  if (grepl("^```", ln)) { in_chunk <- !in_chunk; next }
  if (!in_chunk) keep <- c(keep, ln)
}
text <- paste(keep, collapse = "\n")
text <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", " ", text)
keys <- unique(gsub("^@", "", unlist(regmatches(text, gregexpr("@[A-Za-z0-9_:-]+", text)))))
keys <- keys[!keys %in% c("")]
keys <- keys[!grepl("^(fig|sec|tbl|eq|lst|thm|app)-", keys)]
bib_paths <- c(
  a = file.path(CH2, "07_manuscript/chapter2/reports/references.bib"),
  b = file.path(CH2, "archive/earlier_analysis/chapter2_materials/reports/references.bib"),
  c = "/Users/mohdasti/Documents/GitHub/UCR_PhD_Dissertation/references.bib"
)
parse_bib <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  entries <- list()
  cur <- NULL
  buf <- character()
  depth <- 0L
  for (ln in lines) {
    if (is.null(cur) && grepl("^\\s*@", ln)) {
      cur <- sub("^\\s*@([A-Za-z]+)\\{([^,]+),.*", "\\2", ln)
      typ <- sub("^\\s*@([A-Za-z]+)\\{.*", "\\1", ln)
      buf <- ln
      depth <- length(gregexpr("\\{", ln)[[1]]) - length(gregexpr("\\}", ln)[[1]])
      # gregexpr returns -1 when absent; treat that as 0
      nopen <- if (grepl("\\{", ln)) lengths(regmatches(ln, gregexpr("\\{", ln))) else 0L
      nclose <- if (grepl("\\}", ln)) lengths(regmatches(ln, gregexpr("\\}", ln))) else 0L
      depth <- nopen - nclose
      attr(buf, "type") <- typ
      if (depth <= 0L) { entries[[cur]] <- buf; cur <- NULL; buf <- character() }
      next
    }
    if (!is.null(cur)) {
      buf <- c(buf, ln)
      nopen <- if (grepl("\\{", ln)) lengths(regmatches(ln, gregexpr("\\{", ln))) else 0L
      nclose <- if (grepl("\\}", ln)) lengths(regmatches(ln, gregexpr("\\}", ln))) else 0L
      depth <- depth + nopen - nclose
      if (depth <= 0L) { entries[[cur]] <- buf; cur <- NULL; buf <- character() }
    }
  }
  entries
}
field_of <- function(entry, field) {
  txt <- paste(entry, collapse = "\n")
  m <- regexpr(paste0("(?i)", field, "\\s*=\\s*\\{"), txt, perl = TRUE)
  if (m < 0) {
    m <- regexpr(paste0("(?i)", field, "\\s*=\\s*"), txt, perl = TRUE)
    if (m < 0) return(NA_character_)
  }
  rest <- substr(txt, m + attr(m, "match.length"), nchar(txt))
  if (startsWith(rest, "{")) {
    depth <- 0L
    chars <- strsplit(rest, "")[[1]]
    out <- character()
    for (i in seq_along(chars)) {
      if (chars[[i]] == "{") depth <- depth + 1L
      if (chars[[i]] == "}") { depth <- depth - 1L; if (depth == 0L) { out <- c(out, chars[[i]]); break } }
      out <- c(out, chars[[i]])
    }
    return(paste(out, collapse = ""))
  }
  sub("[,\n].*", "", rest)
}
bibs <- lapply(bib_paths, parse_bib)
flag_keys <- c("granholm2007", "jepma2012neural", "mcDougal2010", "kiefer2018", "prins2016psychophysics",
               "green1966signal", "deGee2020pupil", "colizoli2018belief", "alnaes2014pupil", "azer2023",
               "lalande2014", "chinn2000", "preuschoff2011pupil", "huang2024", "mikneviciute2022",
               "macmillan2005detection", "schutt2016painfree", "brainard1997", "hershman2024",
               "mathot2023methods", "kret2019preprocessing", "lakens2017", "cohen1988", "lee2018",
               "mather2016", "mather2016neural")
rows_i <- rbindlist(lapply(keys, function(k) {
  pick <- function(x, y) if (is.null(x)) y else x
  ea <- bibs$a[[k]]; eb <- bibs$b[[k]]; ec <- bibs$c[[k]]
  entry <- pick(ea, pick(eb, ec))
  author <- if (is.null(entry)) NA_character_ else field_of(entry, "author")
  typ <- if (is.null(entry)) NA_character_ else sub("\\{.*", "", sub("^\\s*@", "", entry[[1]]))
  particle_flag <- FALSE
  if (!is.na(author)) {
    for (p in c("de", "van de", "van", "da", "von")) {
      if (grepl(paste0("(^| )", p, "( |$)"), author) && !grepl(paste0("\\{[^}]*\\b", p, "\\b"), author)) particle_flag <- TRUE
    }
  }
  data.table(
    bib_key = k,
    in_a = !is.null(ea), in_b = !is.null(eb), in_c = !is.null(ec),
    differs_a_b = !is.null(ea) && !is.null(eb) && !identical(paste(ea, collapse = "\n"), paste(eb, collapse = "\n")),
    entry_type = typ,
    author_has_etal_or_others = if (is.na(author)) NA else grepl("et al\\.|\\bothers\\b", author, ignore.case = TRUE),
    unprotected_particle = particle_flag,
    organization_in_author = if (is.na(author)) NA else grepl("Journal|Association|Society|University|Organization", author),
    missing_journal_booktitle_publisher = if (is.null(entry)) NA else all(is.na(c(field_of(entry, "journal"), field_of(entry, "booktitle"), field_of(entry, "publisher")))),
    missing_volume_or_pages_if_article = if (is.null(entry) || tolower(typ) != "article") NA else any(is.na(c(field_of(entry, "volume"), field_of(entry, "pages")))),
    missing_doi = if (is.null(entry)) NA else is.na(field_of(entry, "doi"))
  )
}), fill = TRUE)
fwrite(rows_i, file.path(DIAG, "I_references.csv"))
flag_txt <- c()
for (k in flag_keys) {
  hit <- pick_entry <- bibs$a[[k]]
  if (is.null(hit)) hit <- bibs$b[[k]]
  if (is.null(hit)) hit <- bibs$c[[k]]
  flag_txt <- c(flag_txt, if (is.null(hit)) paste0("% NOT FOUND: ", k) else hit, "")
}
writeLines(flag_txt, file.path(DIAG, "I4_flagged_entries.bib"))

title_hits <- character()
for (nm in names(bibs)) {
  for (k in names(bibs[[nm]])) {
    title <- field_of(bibs[[nm]][[k]], "title")
    if (!is.na(title) && grepl("Handgrip Effort on Perceptual Judgments", title, ignore.case = TRUE)) {
      title_hits <- c(title_hits, paste(nm, k))
    }
  }
}
sun_entry <- c(
  "@misc{sun2026handgrip,",
  "  title     = {Effects of Handgrip Effort on Perceptual Judgments and Metacognitive Performance in Older Adults},",
  "  author    = {Sun, Andrew Y. and Dastgheib, Mohammad and Hu, Xiaoping and Bennett, Ilana J. and Zhang, Weiwei and Maniscalco, Brian and Peters, Megan A. K. and Seitz, Aaron R.},",
  "  year      = {2026},",
  "  publisher = {PsyArXiv},",
  "  note      = {Preprint},",
  "  url       = {https://osf.io/preprints/psyarxiv/wjerv_v1}",
  "}"
)
if (length(title_hits)) {
  log_line("Preprint already in bibs: ", paste(title_hits, collapse = " | "), ". I5 not appended.")
  writeLines(c(
    "Existing title match:", title_hits,
    "No second copy was appended.",
    "No DOI was added.",
    "The dissertation master bib was not edited and still needs this entry if it is not already there.",
    "archive/earlier_analysis/chapter2_materials/reports/references.bib was not edited."
  ), file.path(DIAG, "I5_preprint_bib_status.txt"))
} else {
  bib_a <- bib_paths[["a"]]
  cat("\n", paste(sun_entry, collapse = "\n"), "\n", file = bib_a, sep = "", append = TRUE)
  log_line("Appended sun2026handgrip to ", bib_a)
  writeLines(c(
    "Appended sun2026handgrip to 07_manuscript/chapter2/reports/references.bib.",
    "No DOI was added.",
    "The dissertation master bib was not edited and still needs this entry.",
    "archive/earlier_analysis/chapter2_materials/reports/references.bib was not edited."
  ), file.path(DIAG, "I5_preprint_bib_status.txt"))
}

# --- report, manifest, zip, self-check --------------------------------------

p5_min <- min(parity$pct_lt_1e9)
p5_pass <- is.finite(p5_min) && p5_min >= 99.9
p3_pass <- length(step_med) == 1L && is.finite(step_med) && step_med >= 3.00 && step_med <= 3.30
ready <- p3_pass && p5_pass
stopped <- list.files(DIAG, pattern = "^STOPPED_")
fatal_stop <- any(grepl("Fatal: TRUE", unlist(lapply(file.path(DIAG, stopped), readLines))))

fmt_n <- function(x) {
  if (!length(x)) return("UNDETERMINED")
  ifelse(is.finite(x), format(x, digits = 8, trim = TRUE), "UNDETERMINED")
}
by_task_line <- function(dtab, label) {
  if (!nrow(dtab)) return(paste0(label, ": 0"))
  paste(apply(dtab, 1, function(r) paste0(label, " ", r[["task"]], " trials ", r[["n_trials"]], " participants ", r[["n_subj"]])), collapse = "; ")
}
gate_line <- function(col) {
  dtab <- trials[get(col) %in% TRUE, .(n_trials = .N, n_subj = uniqueN(subject_id)), by = task]
  by_task_line(dtab, col)
}
dip_line <- function(tsk) {
  row <- p3_sum[axis == "step" & task == tsk]
  if (!nrow(row)) return(paste(tsk, "UNDETERMINED"))
  paste0(tsk, " median ", fmt_n(row$dip_median), " s, IQR ", fmt_n(row$dip_iqr),
         ", finite dips ", row$n_finite_dip, " of ", row$n_recordings, " recordings")
}
p7_overall <- p7[comparison == "old cog_auc vs new tepr_auc" & split == "overall"]
p7_complete <- p7[comparison == "new tepr_auc vs tepr_mean_465_765" & split == "tepr_quality_eq_1"]
p7_allmean <- p7[comparison == "new tepr_auc vs tepr_mean_465_765" & split == "overall"]
ref_lt8 <- if (nrow(ref)) sum(ref$implied_end < 8, na.rm = TRUE) else 0L
ref_gt15 <- if (nrow(ref)) sum(ref$implied_end > 15, na.rm = TRUE) else 0L
ref_other <- n_ref - ref_lt8 - ref_gt15
cov_counts <- function(flag) cov6[get(flag) %in% TRUE, .N, by = task]
i5_txt <- paste(readLines(file.path(DIAG, "I5_preprint_bib_status.txt")), collapse = " ")
bib_appended <- grepl("Appended sun2026handgrip", i5_txt)

summary_lines <- c(
  paste0("Part 0: PASS. Flat files ", length(flats), ". v2 rows ", nrow(beh), ". file_dt empty branch 0. Time-axis guard passed."),
  paste0("P1: PASS. Trial rows ", nrow(trials), ". accepted ", n_acc, ", refused ", n_ref, ", unmatched ", n_un, ". No v2 key matched more than one flat trial."),
  paste0("P2: PASS. Refused ", n_ref, " (", format(round(100 * n_ref / 18266, 2), nsmall = 2), "% of 18266)."),
  paste0("P3: ", if (p3_pass) "PASS" else "FAIL", ". Step-axis median dip ", fmt_n(step_med), " s over ", p3_all[axis == "step"]$n_finite_dip, " finite recordings."),
  paste0("P4: PASS. ", gate_line("g_tepr_final"), ". ", gate_line("g_grip_final"), "."),
  paste0("P5: ", if (p5_pass) "PASS" else "FAIL",
         ". All-shared minimum ", format(round(p5_min, 4), nsmall = 4),
         "% (threshold 99.9). Finite-pair minimum ", format(round(min(parity$pct_among_finite), 4), nsmall = 4),
         "%. b0_mean both NA ", parity[comparison == "b0_mean vs baseline_raw"]$n_both_na,
         ", Ch2-only NA ", parity[comparison == "b0_mean vs baseline_raw"]$n_ch2_na_only,
         ", r10-only NA ", parity[comparison == "b0_mean vs baseline_raw"]$n_r10_na_only,
         ". Shared trials ", nrow(shared), "."),
  paste0("P6: PASS. See P6_participant_coverage.csv."),
  paste0("P7: PASS. cog_auc vs tepr_auc r ", fmt_n(p7_overall$pearson_r), " n ", if (nrow(p7_overall)) p7_overall$n else NA, "."),
  paste0("P8: PASS. Citation keys ", nrow(rows_i), ". Preprint bib ", if (bib_appended) "appended to chapter references.bib" else "already present or not appended", ".")
)
ready_line <- paste0("Ready for Phase 2b? ", if (ready && !fatal_stop) "YES" else "NO")

report <- c(
  "# Chapter 2 Phase 2a pupil rebuild",
  "",
  paste("Run", RUN),
  "",
  "## 1. Summary",
  "",
  summary_lines,
  "",
  ready_line,
  "",
  "## 2. Headline answers",
  "",
  paste0("(i) Step-axis light-reflex dip, median across recordings with a finite dip (P3_light_reflex_summary.csv). ",
         dip_line("ADT"), ". ", dip_line("VDT"), ". All-task median ", fmt_n(step_med),
         " s over ", p3_all[axis == "step"]$n_finite_dip, " of ", p3_all[axis == "step"]$n_recordings, " recordings."),
  paste0("(ii) Refused trials ", n_ref, " of 18266 (P2_refused_trials.csv). Implied end < 8 s: ", ref_lt8,
         ". Implied end > 15 s: ", ref_gt15, ". Other refusal text: ", ref_other, "."),
  paste0("(iii) Chapter 2 vs Chapter 3 r10 (P5_r10_parity.csv). Shared trials ", nrow(shared),
         ". The percentage uses all shared trials as the denominator. A pair counts when both values are finite and |difference| < 1e-9. Both-missing and one-sided missing pairs are in n_both_na, n_ch2_na_only, and n_r10_na_only. ",
         paste(paste0(parity$comparison, " ", format(round(parity$pct_lt_1e9, 4), nsmall = 4),
                      "% of shared, ", format(round(parity$pct_among_finite, 4), nsmall = 4),
                      "% of finite pairs, max ", fmt_n(parity$max_abs),
                      ", both NA ", parity$n_both_na, ", Ch2 NA only ", parity$n_ch2_na_only,
                      ", r10 NA only ", parity$n_r10_na_only), collapse = "; "), "."),
  paste0("(iv) Final streams (P4_gate_flow.csv). ", gate_line("g_tepr_final"), ". ", gate_line("g_grip_final"), "."),
  paste0("(v) Old cog_auc vs rebuilt tepr_auc (P7_old_vs_new.csv, split overall): r ", fmt_n(p7_overall$pearson_r),
         ", n ", if (nrow(p7_overall)) p7_overall$n else NA,
         ". New tepr_auc vs tepr_mean_465_765 overall r ", fmt_n(p7_allmean$pearson_r),
         " n ", if (nrow(p7_allmean)) p7_allmean$n else NA,
         "; among tepr_quality == 1, r ", fmt_n(p7_complete$pearson_r),
         " n ", if (nrow(p7_complete)) p7_complete$n else NA, "."),
  "",
  "## 3. Parts",
  "",
  "### Part 0",
  paste0("Expected v2 rows 18266, 66 auditory subjects, 66 visual subjects, sessions {2, 3}, stim_level_index {0,1,2,3,4}, trial_num max 30, no BAP140 auditory, no BAP109, no BAP144. Observed rows ", nrow(beh), ", auditory subjects ", n_aud, ", visual subjects ", n_vis, "."),
  paste0("Expected about 116 top-level flat files and zero paths containing build_ or archive. Observed ", length(flats), "."),
  paste0("file_dt empty-difference branch expected 0. Observed 0."),
  "Time-axis guard (0.3_time_axis_test.txt) passed against R/trial_time_axis.R, md5 25c21c1f736084be6fc80766d5fb7518.",
  "0.4_gate_fix.md: the writer of qc_new_keys.csv was not in the Chapter 3 tree.",
  "",
  "### P1",
  paste0("Expected 18266 rows and accepted + refused + unmatched = 18266, with no v2 key matching more than one flat trial. Observed rows ", nrow(trials), ", accepted ", n_acc, ", refused ", n_ref, ", unmatched ", n_un, "."),
  paste0("Flat trials in sessions 2–3 with no v2 key, summed across files: ", sum(p1$flat_trials_not_in_v2, na.rm = TRUE), " (P1_key_match_by_subject_task.csv)."),
  "",
  "### P2",
  "Windows follow scripts/r8/B_rebuild_pupil.R: B0 [-0.5, 0), B1 [3.85, 4.35), TEPR [4.65, 7.65] closed, grip [1.0, 4.35) via window_mean, total AUC [0, 4.70] closed after B0. compute_auc does not bridge gaps above 250 ms.",
  paste0("Refused ", n_ref, ". By task: ", paste(capture.output(print(trials[, .(n = .N, refused = sum(time_rule %in% "refused")), by = task])), collapse = " ")),
  "Phase 1 found 1,072 old GLMM trials from top-level files whose step end fell outside [8, 15]. Those trials are refused here and have no pupil values.",
  "",
  "### P3",
  "Dip search is Chapter 3 A_alignment.R: which.min on GRID in [2.5, 3.8]. dip_wide is [1.5, 5.5]. Peak uses the function's inclusive bounds [4.35, 7.65].",
  "The median and IQR use recordings with a finite dip. n_finite_dip is that denominator. n_recordings counts every subject×task with a saved trace, including NA dips.",
  paste0("Step axis: ", dip_line("ADT"), ". ", dip_line("VDT"), ". All-task median ", fmt_n(step_med), " s. STOP band is [3.00, 3.30]."),
  paste0("Native-axis finite dips: ADT ", p3_sum[axis == "native" & task == "ADT"]$n_finite_dip,
         " of ", p3_sum[axis == "native" & task == "ADT"]$n_recordings,
         ", VDT ", p3_sum[axis == "native" & task == "VDT"]$n_finite_dip,
         " of ", p3_sum[axis == "native" & task == "VDT"]$n_recordings,
         ". The flat file time column was used as stored. No relative native axis was built."),
  "",
  "### P4",
  "Rows were kept. g_tepr_final requires a matched step-axis trial, finite tepr_auc, b1_quality >= 0.50, tepr_quality >= 0.50, and at least 20 such trials in the subject×task. Grip uses finite grip_mean, b0_quality >= 0.50, grip_quality >= 0.50, and the same 20-trial rule. window_mean sets grip_mean to NA when coverage is below 0.50, so those trials fail g_grip_1.",
  gate_line("g_tepr_1"),
  gate_line("g_tepr_2"),
  gate_line("g_tepr_3"),
  gate_line("g_tepr_final"),
  gate_line("g_grip_1"),
  gate_line("g_grip_2"),
  gate_line("g_grip_3"),
  gate_line("g_grip_final"),
  "",
  "### P5",
  paste(readLines(file.path(DIAG, "P5_column_mapping.txt")), collapse = " "),
  paste0("Shared with r10: ", nrow(shared), ". v2 vs r5: shared ", nrow(both_vr), ", v2-only ", nrow(v2_only), ", r5-only ", nrow(r5_only), " (P5_v2_vs_r5.csv)."),
  paste0("g_tepr_final trials absent from r10: ", nrow(only_ch2), ". r10 trials absent from g_tepr_final: ", nrow(only_r10), ". Reasons are in P5_coverage_reasons.csv."),
  if (p5_pass) "All four comparisons are at least 99.9%." else "STOP_P5 is non-fatal. P5_worst50.csv and P5_difference_clusters.csv list the disagreements.",
  "",
  "### P6",
  paste0("v2 subject×task rows ", nrow(cov6), ". By task, in v2: ", paste(capture.output(print(cov6[, .N, by = task])), collapse = " "), "."),
  paste0("Flat file present: ", paste(capture.output(print(cov_counts("flat_file_present"))), collapse = " "), "."),
  paste0("At least one matched trial: ", paste(capture.output(print(cov_counts("ge1_matched"))), collapse = " "), "."),
  paste0("g_tepr_final: ", paste(capture.output(print(cov_counts("g_tepr_final"))), collapse = " "), "."),
  paste0("g_grip_final: ", paste(capture.output(print(cov_counts("g_grip_final"))), collapse = " "), "."),
  "P6_missing_session_detail.csv lists session_used for BAP122, BAP139, BAP186, BAP191, BAP194, BAP195 (ADT) and BAP101, BAP103, BAP114 (VDT).",
  "P6_missing_flat_search.csv lists filename hits under the flat root for BAP001, BAP111, BAP165, BAP189, BAP190, BAP192, and BAP197. Those files were not read into the feature table.",
  "",
  "### P7",
  "Pearson correlations only. No test and no model. Trials absent from Phase 1 B2_trial_source.csv are the split not_in_phase1_B2.",
  paste(capture.output(print(p7)), collapse = "\n"),
  "",
  "### P8",
  paste0("Keys cited in chapter2_dissertation.qmd, outside code chunks: ", nrow(rows_i), ". Table: I_references.csv. Flagged entries: I4_flagged_entries.bib."),
  i5_txt,
  "The dissertation master bibliography was not edited.",
  "",
  "## 4. Deviations",
  "",
  "The gate-fix writer was not in the Chapter 3 working tree, so P4 uses the window definitions in the Phase 2a prompt and checks them in P5.",
  if (length(flats) == 116L) "Top-level flat count was 116." else paste("Top-level flat count was", length(flats), "rather than 116. The run continued."),
  "Peak latency follows A_alignment.R, which includes 4.35 s. The prompt text writes the peak window as (4.35, 7.65].",
  "Dip medians exclude recordings whose dip is NA. Both counts are in P3_light_reflex_summary.csv.",
  "P5 percentages use every shared trial as the denominator.",
  "",
  "## 5. UNDETERMINED",
  "",
  "The script that wrote output/diagnostics/r8_gatefix/qc_new_keys.csv and output/diagnostics/r9_precheck/trial_window_quality.csv is not in the Chapter 3 tree (0.4_gate_fix.md).",
  if (any(rows_i$in_a %in% FALSE & rows_i$in_b %in% FALSE & rows_i$in_c %in% FALSE)) {
    paste("Citation keys missing from all three bibliographies:", paste(rows_i[in_a %in% FALSE & in_b %in% FALSE & in_c %in% FALSE]$bib_key, collapse = ", "))
  } else {
    "Every cited key was found in at least one bibliography."
  },
  ""
)
writeLines(report, file.path(DIAG, "report.md"))
log_line("Wrote report.md")

out_files <- c(
  file.path(DATA, "ch2_pupil_trials_r1.csv"),
  file.path(DATA, "ch2_waveforms_subject_condition_r1.csv"),
  file.path(DIAG, "manifest_inputs.csv"),
  file.path(DIAG, "report.md"),
  list.files(DIAG, pattern = "\\.(csv|md|txt|png|bib)$", full.names = TRUE)
)
out_files <- unique(out_files[file.exists(out_files)])
man_out <- data.table(path = out_files, md5 = unname(tools::md5sum(out_files)), bytes = file.info(out_files)$size)
fwrite(man_out, file.path(DIAG, "manifest_outputs.csv"))
gate_counts <- gate_flow[, .(n_trials = sum(n_trials)), by = gate]
json <- list(
  run = RUN,
  ch2_head = g_ch2$head,
  ddm_head = g_ddm$head,
  script_md5 = unname(tools::md5sum(file.path(CH2, "scripts/phase2a/ch2_phase2a_rebuild.R"))),
  trial_rows = nrow(trials),
  accepted = n_acc, refused = n_ref, unmatched = n_un,
  step_dip_median = step_med,
  p5_min_pct = p5_min,
  ready_for_phase_2b = ready && !fatal_stop,
  gates = as.list(setNames(gate_counts$n_trials, gate_counts$gate)),
  output_md5 = lapply(seq_len(nrow(man_out)), function(i) list(path = man_out$path[i], md5 = man_out$md5[i]))
)
if (requireNamespace("jsonlite", quietly = TRUE)) {
  writeLines(jsonlite::toJSON(json, auto_unbox = TRUE, pretty = TRUE), file.path(DATA, "manifest.json"))
} else {
  writeLines(capture.output(str(json)), file.path(DATA, "manifest.json"))
}
file.copy(file.path(DATA, "manifest.json"), file.path(DIAG, "manifest.json"), overwrite = TRUE)

g_ddm_end <- git_ro(DDM)
g_ch2_end <- git_ro(CH2)
writeLines(g_ddm_end$status, file.path(DIAG, "0_status_ddm_end.txt"))
writeLines(g_ch2_end$status, file.path(DIAG, "0_status_ch2_end.txt"))
same_ddm <- identical(g_ddm$status, g_ddm_end$status)
new_ch2 <- setdiff(g_ch2_end$status, g_ch2$status)
log_line("SELF-CHECK DDM porcelain unchanged: ", same_ddm)
log_line("SELF-CHECK new CH2 status lines:")
for (ln in new_ch2) log_line("  ", ln)
log_line("SELF-CHECK CH2 status still contains the pre-existing untracked data/ and output/ lines from 0_status_ch2.txt.")

cited <- c(
  "report.md", "P3_light_reflex_summary.csv", "P3_light_reflex_by_recording.csv", "P3_grand_waveform.png",
  "P2_refused_trials.csv", "P2_refused_summary.csv", "P5_r10_parity.csv", "P5_coverage_reasons.csv",
  "P5_v2_vs_r5.csv", "P4_gate_flow.csv", "P6_participant_coverage.csv", "P6_missing_session_detail.csv",
  "P6_missing_flat_search.csv", "P7_old_vs_new.csv", "I_references.csv", "I4_flagged_entries.bib",
  "I5_preprint_bib_status.txt", "0.4_gate_fix.md", "0.3_time_axis_test.txt", "manifest_inputs.csv",
  "manifest.json", "run_log.txt", "ch2_pupil_trials_r1.csv", "ch2_waveforms_subject_condition_r1.csv",
  "ch2_phase2a_rebuild.R", "trial_time_axis.R", "P1_key_match_by_subject_task.csv", "P5_column_mapping.txt"
)
if (!p5_pass) cited <- c(cited, "P5_worst50.csv", "P5_difference_clusters.csv", "STOPPED_P5.md")
header_only <- character()
for (f in list.files(DIAG, pattern = "\\.csv$", full.names = TRUE)) {
  n <- nrow(fread(f, nrows = 5L, showProgress = FALSE))
  if (n == 0L) header_only <- c(header_only, basename(f))
}
for (bn in c("ch2_pupil_trials_r1.csv", "ch2_waveforms_subject_condition_r1.csv")) {
  f <- file.path(DATA, bn)
  if (file.exists(f) && nrow(fread(f, nrows = 5L, showProgress = FALSE)) == 0L) header_only <- c(header_only, bn)
}
log_line("SELF-CHECK header-only CSVs: ", if (length(header_only)) paste(header_only, collapse = ", ") else "none")
writeLines(c(report, "", "## Self-check", "",
             paste("DDM porcelain unchanged:", same_ddm),
             paste("New Chapter 2 status lines since startup:", if (length(new_ch2)) paste(new_ch2, collapse = " | ") else "none"),
             "Pre-existing untracked data/ and output/ remain in git status.",
             paste("Header-only CSVs:", if (length(header_only)) paste(header_only, collapse = ", ") else "none")),
          file.path(DIAG, "report.md"))

zip_path <- package_zip()
zip_list <- system2("unzip", c("-l", zip_path), stdout = TRUE)
writeLines(zip_list, file.path(DIAG, "zip_listing.txt"))
missing_cited <- cited[!vapply(cited, function(bn) any(grepl(bn, zip_list, fixed = TRUE)), logical(1))]
unzip_ok <- system2("unzip", c("-t", zip_path), stdout = FALSE, stderr = FALSE) == 0L
log_line("SELF-CHECK zip opens: ", unzip_ok)
log_line("SELF-CHECK cited files missing from zip: ", if (length(missing_cited)) paste(missing_cited, collapse = ", ") else "none")
log_line("Phase 2a script finished")
cat(paste0(
  "\nZip opens: ", unzip_ok,
  "\nCited files missing from zip: ", if (length(missing_cited)) paste(missing_cited, collapse = ", ") else "none",
  "\n"
), file = file.path(DIAG, "report.md"), append = TRUE)
old_wd <- getwd()
setwd(DIAG)
system2("zip", c("-u", zip_path, "report.md", "run_log.txt", "zip_listing.txt"))
setwd(old_wd)
quit(status = 0L)
