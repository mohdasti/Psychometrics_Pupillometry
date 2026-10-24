#!/usr/bin/env Rscript
# Chapter 2 Phase 2a.1: sampling interval and epoch, read-only.
# Reports evidence. Does not choose a time axis and does not fit a statistical model.
# file_dt() is the Phase 2a copy of modeling-pupil-DDM/scripts/r8/B_rebuild_pupil.R lines 53-60.

options(warn = 1, scipen = 999)
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) stop("Usage: Rscript ch2_axis_check.R RUN_ID")
RUN <- args[[1L]]

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
DDM <- "/Users/mohdasti/Documents/GitHub/modeling-pupil-DDM"
FLAT <- "/Users/mohdasti/Documents/LC-BAP/BAP/BAP_Pupillometry/BAP/BAP_processed"
FLAT25 <- file.path(FLAT, "build_20251225_154443")
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2a1_axis", RUN)
TRIALS <- file.path(CH2, "data/analysis_ready/ch2_r1/ch2r1_20261024_081618/ch2_pupil_trials_r1.csv")
REFUSED <- file.path(CH2, "output/diagnostics/ch2_phase2a/ch2r1_20261024_081618/P2_refused_summary.csv")
KEYS75 <- file.path(CH2, "output/diagnostics/ch2_phase2a/ch2r1_20261024_081618/I_references.csv")
BIB_MASTER <- "/Users/mohdasti/Documents/GitHub/UCR_PhD_Dissertation/references.bib"
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
LOG <- file.path(DIAG, "run_log.txt")

log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
}

stop_md <- function(part, step, detail) {
  path <- file.path(DIAG, sprintf("STOPPED_%s.md", part))
  writeLines(c(
    sprintf("# STOPPED_%s", part), "",
    paste("Part:", part), paste("Step:", step), "",
    "## Error", "", detail
  ), path)
  log_line("STOPPED ", part, " ", step)
  try(package_zip(), silent = TRUE)
  quit(status = 2L)
}

package_zip <- function() {
  zip_path <- file.path(CH2, "output/diagnostics/ch2_phase2a1_axis", paste0("ch2_phase2a1_", RUN, ".zip"))
  if (file.exists(zip_path)) unlink(zip_path)
  stage <- file.path(DIAG, "zip_stage")
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, showWarnings = FALSE)
  files <- list.files(DIAG, full.names = TRUE, recursive = FALSE)
  files <- files[!grepl("/zip_stage$", files)]
  for (f in files) if (file.exists(f) && !dir.exists(f)) file.copy(f, file.path(stage, basename(f)), overwrite = TRUE)
  script <- file.path(CH2, "scripts/phase2a1/ch2_axis_check.R")
  if (file.exists(script)) file.copy(script, file.path(stage, basename(script)), overwrite = TRUE)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(stage)
  utils::zip(zip_path, list.files("."))
  log_line("Zip ", zip_path, " bytes ", file.info(zip_path)$size)
  zip_path
}

file_dt <- function(time_vec) {
  d <- diff(sort(unique(time_vec[is.finite(time_vec)])))
  d <- d[is.finite(d) & d > 0]
  if (!length(d)) return(0.004)
  dt <- median(d)
  if (dt >= 1 && dt <= 10) dt <- dt / 1000
  if (!is.finite(dt) || dt <= 0) 0.004 else dt
}

file_dt_empty <- function(time_vec) {
  d <- diff(sort(unique(time_vec[is.finite(time_vec)])))
  d <- d[is.finite(d) & d > 0]
  !length(d)
}

mode_round <- function(d) {
  if (!length(d)) return(NA_real_)
  r <- round(d, 6)
  u <- unique(r)
  u[which.max(tabulate(match(r, u)))]
}

near <- function(d, v) if (!length(d)) NA_real_ else mean(abs(d - v) < 1e-6)

q5 <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, 5))
  as.numeric(quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE))
}

linfit <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 3L || !is.finite(sd(x)) || sd(x) == 0) {
    return(list(intercept = NA_real_, slope = NA_real_, resid_sd = NA_real_, n = n))
  }
  e <- cov(x, y) / var(x)
  c0 <- mean(y) - e * mean(x)
  list(intercept = c0, slope = e, resid_sd = sd(y - (c0 + e * x)), n = n)
}

# i21 = a + round(b * i25). This is the integer-lag grid in T3b.
# It differs from round(a + b * i25) by at most one sample.
grid_align <- function(x, y, b_grid = seq(0.70, 1.30, by = 0.01), a_min = -400L, a_max = 400L) {
  empty <- list(a = NA_integer_, b = NA_real_, r = NA_real_, n = 0L, n_tie = NA_integer_,
                r_b100 = NA_real_, r_b087 = NA_real_, r_b088 = NA_real_, r_b114 = NA_real_)
  n <- length(x)
  xf <- is.finite(x)
  ii <- which(is.finite(y))
  if (length(ii) < 50L) {
    empty$n <- length(ii)
    return(empty)
  }
  vv_all <- y[ii]
  ext_lo <- 1L - a_max
  ext_hi <- n - a_min
  len <- ext_hi - ext_lo + 1L
  N <- as.integer(2^ceiling(log2(4 * (len + n))))
  X <- numeric(N)
  X2 <- numeric(N)
  Mx <- numeric(N)
  ix <- seq_len(n) - ext_lo + 1L
  X[ix] <- ifelse(xf, x, 0)
  X2[ix] <- ifelse(xf, x * x, 0)
  Mx[ix] <- as.numeric(xf)
  fx <- fft(X)
  fx2 <- fft(X2)
  fmx <- fft(Mx)
  as_ <- a_min:a_max
  k <- ifelse(as_ >= 0L, as_ + 1L, N + as_ + 1L)
  best_r <- -Inf
  best <- empty
  for (b in b_grid) {
    pos <- as.integer(round(b * ii))
    keep <- pos >= ext_lo & pos <= ext_hi
    if (sum(keep) < 50L) next
    iy <- pos[keep] - ext_lo + 1L
    vv <- vv_all[keep]
    agg_n <- rowsum(rep(1, length(iy)), iy, reorder = FALSE)
    agg_s <- rowsum(vv, iy, reorder = FALSE)
    agg_s2 <- rowsum(vv * vv, iy, reorder = FALSE)
    idx <- as.integer(rownames(agg_n))
    Y <- numeric(N)
    Y2 <- numeric(N)
    My <- numeric(N)
    Y[idx] <- agg_s[, 1]
    Y2[idx] <- agg_s2[, 1]
    My[idx] <- agg_n[, 1]
    conv <- function(fA, B) Re(fft(fA * Conj(fft(B)), inverse = TRUE)) / N
    sn <- conv(fmx, My)
    sx <- conv(fx, My)
    sy <- conv(fmx, Y)
    sxx <- conv(fx2, My)
    syy <- conv(fmx, Y2)
    sxy <- conv(fx, Y)
    nn <- sn[k]
    num <- nn * sxy[k] - sx[k] * sy[k]
    den <- sqrt(pmax(0, nn * sxx[k] - sx[k]^2) * pmax(0, nn * syy[k] - sy[k]^2))
    r <- ifelse(nn >= 50 & den > 0, num / den, NA_real_)
    if (!any(is.finite(r))) next
    im <- which.max(r)
    if (abs(b - 1.00) < 1e-9) best$r_b100 <- r[im]
    if (abs(b - 0.87) < 1e-9) best$r_b087 <- r[im]
    if (abs(b - 0.88) < 1e-9) best$r_b088 <- r[im]
    if (abs(b - 1.14) < 1e-9) best$r_b114 <- r[im]
    if (is.finite(r[im]) && r[im] > best_r) {
      best_r <- r[im]
      best$a <- as_[im]
      best$b <- b
      best$r <- r[im]
      best$n <- as.integer(round(nn[im]))
      best$n_tie <- sum(is.finite(r) & r >= r[im] - 1e-4)
    }
  }
  best
}

new_wave <- function() list(sums = numeric(length(GRID)), counts = integer(length(GRID)), n_trials = 0L)
GRID <- seq(-1, 8, by = 0.02)
add_wave <- function(wave, t, pupil) {
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

log_line("=== Phase 2a.1 ", RUN, " ===")
log_line("date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
log_line("R: ", R.version.string)
writeLines(capture.output(sessionInfo()), file.path(DIAG, "sessionInfo.txt"))
git_head <- function(repo) paste(system2("git", c("-C", repo, "--no-optional-locks", "rev-parse", "HEAD"), stdout = TRUE), collapse = "")
log_line("CH2 HEAD ", git_head(CH2))
log_line("DDM HEAD ", git_head(DDM))
log_line("CH2 ", CH2)
log_line("DDM ", DDM)
log_line("FLAT ", FLAT)
log_line("FLAT25 ", FLAT25)
log_line("DIAG ", DIAG)

for (p in c(FLAT, FLAT25, TRIALS, REFUSED, KEYS75)) {
  if (!file.exists(p) && !dir.exists(p)) stop_md("T0", "paths", paste("Missing", p))
}

pat <- "^BAP[0-9]+_(ADT|VDT)_flat\\.csv$"
f21 <- list.files(FLAT, pattern = pat, full.names = TRUE, recursive = FALSE)
f25 <- list.files(FLAT25, pattern = pat, full.names = TRUE, recursive = FALSE)
log_line("December 21 files ", length(f21), "; December 25 files ", length(f25))
if (!length(f21) || !length(f25)) stop_md("T0", "list", "No flat files matched the December 21 or December 25 pattern.")

id_of <- function(path) {
  bn <- basename(path)
  data.table(
    path = path,
    subject_id = sub("_(ADT|VDT)_flat\\.csv$", "", bn),
    task = sub("^.*(ADT|VDT)_flat\\.csv$", "\\1", bn)
  )
}
id21 <- id_of(f21)
id25 <- id_of(f25)
setorder(id21, subject_id, task)
setorder(id25, subject_id, task)
both <- merge(id21, id25, by = c("subject_id", "task"), suffixes = c("_21", "_25"))
setorder(both, subject_id, task)
adt5 <- head(both[task == "ADT"], 5)
vdt5 <- head(both[task == "VDT"], 5)
extra <- both[subject_id %in% c("BAP003", "BAP108", "BAP124") &
              ((subject_id == "BAP003" & task == "ADT") |
               (subject_id == "BAP108" & task == "VDT") |
               (subject_id == "BAP124" & task == "ADT"))]
pairs <- unique(rbind(adt5, vdt5, extra, fill = TRUE))
setorder(pairs, subject_id, task)
fwrite(pairs[, .(subject_id, task)], file.path(DIAG, "T3_pairs_selected.csv"))
log_line("T3 pairs ", nrow(pairs), ": ", paste(pairs$subject_id, pairs$task, collapse = ", "))

ref <- fread(REFUSED)
need_ref <- c("subject_id", "task")
if (!all(need_ref %in% names(ref))) stop("P2_refused_summary.csv is missing column(s): ", paste(setdiff(need_ref, names(ref)), collapse = ", "), call. = FALSE)
ref[, subject_id := as.character(subject_id)]
ref[, task := as.character(task)]

acc <- fread(TRIALS, select = c("subject_id", "task", "session", "run", "trial", "time_rule"))
if (!all(c("subject_id", "task", "session", "run", "trial", "time_rule") %in% names(acc))) {
  stop("Trial table is missing a required column.", call. = FALSE)
}
acc <- acc[time_rule == "step_from_m3", .(subject_id = as.character(subject_id), task = as.character(task),
                                         session = as.integer(session), run = as.integer(run), trial = as.integer(trial))]
acc_keys <- acc[, .(hit = TRUE), by = .(subject_id, task, session, run, trial)]
log_line("Accepted step_from_m3 trials ", nrow(acc))

need_cols <- c("session_used", "run_used", "trial_in_run_raw", "time", "pupil", "trial_label")
t0_name_rx <- "seg|time|onset|event|ptb|phase"

trial_stats <- function(dt) {
  dt[, {
    d <- diff(time)
    df <- d[is.finite(d)]
    .(
      n = .N,
      median_d = if (length(df)) median(df) else NA_real_,
      mode_d = mode_round(df),
      frac_0005 = near(df, 0.0005),
      frac_0035 = near(df, 0.0035),
      frac_004 = near(df, 0.004),
      frac_005 = near(df, 0.005),
      n_d_le_0 = sum(df <= 0),
      n_d_gt_0.05 = sum(df > 0.05),
      n_d_nonfinite = sum(!is.finite(d)),
      span_core = time[.N] - time[1],
      n_finite_d = length(df)
    )
  }, by = .(session = session_used, run = run_used, trial = trial_in_run_raw)]
}

label_runs <- function(dt) {
  dt[, {
    rr <- rle(as.character(trial_label))
    end <- cumsum(rr$lengths)
    .(label = rr$values, first_index = end - rr$lengths + 1L, last_index = end, length_samples = rr$lengths)
  }, by = .(session = session_used, run = run_used, trial = trial_in_run_raw)]
}

t1_rows <- list()
t2_runs <- list()
t2_labels <- list()
waves <- list()
t0_done <- FALSE
jump_rows <- list()
ri <- 0L

read_flat <- function(path, cols) {
  stopifnot(file.exists(path))
  hdr <- names(fread(path, nrows = 0L))
  miss <- setdiff(cols, hdr)
  if (length(miss)) stop_md("read", "columns", paste("Flat file is missing column(s):", paste(miss, collapse = ", "), "in", path))
  fread(path, select = cols, showProgress = FALSE)
}

for (i in seq_len(nrow(id21))) {
  path <- id21$path[i]
  subj <- id21$subject_id[i]
  tsk <- id21$task[i]
  if (i %% 10L == 1L) log_line("T1/T2 December 21 ", i, "/", nrow(id21), " ", basename(path))
  dt <- read_flat(path, need_cols)
  dt <- dt[session_used %in% c(2L, 3L)]
  dt[, `:=`(session_used = as.integer(session_used), run_used = as.integer(run_used),
            trial_in_run_raw = as.integer(trial_in_run_raw), time = as.numeric(time), pupil = as.numeric(pupil))]
  if (!nrow(dt)) next
  st <- trial_stats(dt)
  st[, `:=`(generation = "dec21", subject_id = subj, task = tsk,
            file_dt = file_dt(dt$time), file_dt_empty = file_dt_empty(dt$time),
            refused_phase2a = paste(subj, tsk) %in% paste(ref$subject_id, ref$task))]
  ri <- ri + 1L
  t1_rows[[ri]] <- st
  runs <- label_runs(dt)
  runs[, `:=`(generation = "dec21", subject_id = subj, task = tsk)]
  t2_runs[[ri]] <- runs
  lab <- dt[, .N, by = trial_label]
  lab[, `:=`(subject_id = subj, task = tsk)]
  t2_labels[[ri]] <- lab

  # T4 waveforms for accepted trials
  st_acc <- merge(st, acc_keys, by.x = c("subject_id", "task", "session", "run", "trial"),
                  by.y = c("subject_id", "task", "session", "run", "trial"))
  if (nrow(st_acc)) {
    key <- paste(subj, tsk)
    if (is.null(waves[[key]])) waves[[key]] <- list(A = new_wave(), B = new_wave(), subject_id = subj, task = tsk)
    parts <- split(seq_len(nrow(dt)), paste(dt$session_used, dt$run_used, dt$trial_in_run_raw))
    want <- paste(st_acc$session, st_acc$run, st_acc$trial)
    for (nm in intersect(names(parts), want)) {
      ix <- parts[[nm]]
      pup <- dt$pupil[ix]
      n <- length(ix)
      if (n < 2L) next
      tA <- -3 + (seq_len(n) - 1) * 0.0035
      tB <- -3 + (seq_len(n) - 1) * 0.004
      waves[[key]]$A <- add_wave(waves[[key]]$A, tA, pup)
      waves[[key]]$B <- add_wave(waves[[key]]$B, tB, pup)
    }
  }

  if (!t0_done && subj == "BAP003" && tsk == "ADT") {
    t0_done <- TRUE
    hdr <- names(fread(path, nrows = 0L))
    extra <- hdr[grepl(t0_name_rx, hdr, ignore.case = TRUE)]
    keep <- unique(c("session_used", "run_used", "trial_in_run_raw", "time", "pupil", "trial_label", extra))
    full <- read_flat(path, keep)
    classes <- data.table(column = names(full), type = vapply(full, function(z) paste(class(z), collapse = ","), ""))
    fwrite(classes, file.path(DIAG, "T0_columns_dec21.csv"))
    full <- full[session_used %in% c(2L, 3L)]
    ord_trials <- unique(full[, .(session_used, run_used, trial_in_run_raw)])
    first <- ord_trials[1]
    block <- full[session_used == first$session_used & run_used == first$run_used & trial_in_run_raw == first$trial_in_run_raw]
    sink(file.path(DIAG, "T0_rows.txt"))
    cat("FILE December 21 ", path, "\n")
    cat("COLUMNS\n")
    print(classes)
    cat("\nFIRST TRIAL session", first$session_used, "run", first$run_used, "trial", first$trial_in_run_raw, "n", nrow(block), "\n")
    cat("FIRST 15 ROWS\n")
    print(head(block, 15), nrows = 15)
    cat("\nLAST 15 ROWS\n")
    print(tail(block, 15), nrows = 15)
    sink()
    take <- head(ord_trials, 3)
    for (j in seq_len(nrow(take))) {
      tr <- full[session_used == take$session_used[j] & run_used == take$run_used[j] & trial_in_run_raw == take$trial_in_run_raw[j]]
      d <- diff(tr$time)
      ord <- order(d, decreasing = TRUE, na.last = NA)
      top <- head(ord, 10)
      jump_rows[[length(jump_rows) + 1L]] <- data.table(
        generation = "dec21", subject_id = subj, task = tsk,
        session = take$session_used[j], run = take$run_used[j], trial = take$trial_in_run_raw[j],
        row_before = top, row_after = top + 1L,
        time_before = tr$time[top], time_after = tr$time[top + 1L], diff_time = d[top]
      )
    }
  }
}

log_line("December 21 pass done")

# December 25 T1, and T0 if dec21 T0 file was BAP003
t0_25 <- file.path(FLAT25, "BAP003_ADT_flat.csv")
if (!file.exists(t0_25)) t0_25 <- id25$path[1]
if (!is.na(t0_25) && file.exists(t0_25)) {
  hdr <- names(fread(t0_25, nrows = 0L))
  extra <- hdr[grepl(t0_name_rx, hdr, ignore.case = TRUE)]
  keep <- unique(c(need_cols, extra))
  keep <- intersect(keep, hdr)
  miss <- setdiff(c("session_used", "run_used", "trial_in_run_raw", "time", "pupil", "trial_label"), keep)
  if (length(miss)) stop_md("T0", "columns", paste("December 25 file is missing column(s):", paste(miss, collapse = ", "), "in", t0_25))
  full <- fread(t0_25, select = keep, showProgress = FALSE)
  classes <- data.table(column = names(full), type = vapply(full, function(z) paste(class(z), collapse = ","), ""))
  fwrite(classes, file.path(DIAG, "T0_columns_dec25.csv"))
  full_s <- full[session_used %in% c(2L, 3L)]
  ord_trials <- unique(full_s[, .(session_used, run_used, trial_in_run_raw)])
  first <- ord_trials[1]
  block <- full_s[session_used == first$session_used & run_used == first$run_used & trial_in_run_raw == first$trial_in_run_raw]
  sink(file.path(DIAG, "T0_rows.txt"), append = TRUE)
  cat("\n\nFILE December 25 ", t0_25, "\n")
  cat("COLUMNS\n")
  print(classes)
  cat("\nFIRST TRIAL session", first$session_used, "run", first$run_used, "trial", first$trial_in_run_raw, "n", nrow(block), "\n")
  cat("FIRST 15 ROWS\n")
  print(head(block, 15), nrows = 15)
  cat("\nLAST 15 ROWS\n")
  print(tail(block, 15), nrows = 15)
  sink()
  take <- head(ord_trials, 3)
  for (j in seq_len(nrow(take))) {
    tr <- full_s[session_used == take$session_used[j] & run_used == take$run_used[j] & trial_in_run_raw == take$trial_in_run_raw[j]]
    d <- diff(tr$time)
    ord <- order(d, decreasing = TRUE, na.last = NA)
    top <- head(ord, 10)
    jump_rows[[length(jump_rows) + 1L]] <- data.table(
      generation = "dec25", subject_id = sub("_(ADT|VDT)_flat\\.csv$", "", basename(t0_25)),
      task = sub("^.*(ADT|VDT)_flat\\.csv$", "\\1", basename(t0_25)),
      session = take$session_used[j], run = take$run_used[j], trial = take$trial_in_run_raw[j],
      row_before = top, row_after = top + 1L,
      time_before = tr$time[top], time_after = tr$time[top + 1L], diff_time = d[top]
    )
  }
}
if (length(jump_rows)) fwrite(rbindlist(jump_rows, fill = TRUE), file.path(DIAG, "T0_time_jumps.csv"))

# column list text
col_txt <- c(
  "December 21",
  if (file.exists(file.path(DIAG, "T0_columns_dec21.csv"))) readLines(file.path(DIAG, "T0_columns_dec21.csv")) else "T0_columns_dec21.csv missing",
  "",
  "December 25",
  if (file.exists(file.path(DIAG, "T0_columns_dec25.csv"))) readLines(file.path(DIAG, "T0_columns_dec25.csv")) else "T0_columns_dec25.csv missing",
  "",
  "Row dumps are in T0_rows.txt."
)
writeLines(col_txt, file.path(DIAG, "T0_columns.txt"))

for (i in seq_len(nrow(id25))) {
  path <- id25$path[i]
  subj <- id25$subject_id[i]
  tsk <- id25$task[i]
  if (i %% 10L == 1L) log_line("T1 December 25 ", i, "/", nrow(id25), " ", basename(path))
  dt <- read_flat(path, c("session_used", "run_used", "trial_in_run_raw", "time"))
  dt <- dt[session_used %in% c(2L, 3L)]
  if (!nrow(dt)) next
  dt[, `:=`(session_used = as.integer(session_used), run_used = as.integer(run_used),
            trial_in_run_raw = as.integer(trial_in_run_raw), time = as.numeric(time))]
  st <- trial_stats(dt)
  st[, `:=`(generation = "dec25", subject_id = subj, task = tsk,
            file_dt = file_dt(dt$time), file_dt_empty = file_dt_empty(dt$time),
            refused_phase2a = paste(subj, tsk) %in% paste(ref$subject_id, ref$task))]
  t1_rows[[length(t1_rows) + 1L]] <- st
}

t1 <- rbindlist(t1_rows, fill = TRUE)
fwrite(t1, file.path(DIAG, "T1_trial_increments.csv"))
log_line("T1 trials ", nrow(t1))

by_file <- t1[, .(
  n_trials = .N,
  median_of_trial_median_d = median(median_d, na.rm = TRUE),
  file_dt = file_dt[1],
  file_dt_empty = file_dt_empty[1],
  median_n = as.numeric(median(n, na.rm = TRUE)),
  median_span_core = median(span_core, na.rm = TRUE),
  frac_trials_median_near_0005 = mean(abs(median_d - 0.0005) < 1e-6),
  frac_trials_median_near_0035 = mean(abs(median_d - 0.0035) < 1e-6),
  frac_trials_median_near_004 = mean(abs(median_d - 0.004) < 1e-6),
  frac_trials_median_near_005 = mean(abs(median_d - 0.005) < 1e-6),
  weighted_frac_0005 = sum(frac_0005 * n_finite_d, na.rm = TRUE) / sum(n_finite_d),
  weighted_frac_0035 = sum(frac_0035 * n_finite_d, na.rm = TRUE) / sum(n_finite_d),
  weighted_frac_004 = sum(frac_004 * n_finite_d, na.rm = TRUE) / sum(n_finite_d),
  weighted_frac_005 = sum(frac_005 * n_finite_d, na.rm = TRUE) / sum(n_finite_d),
  refused_phase2a = refused_phase2a[1]
), by = .(generation, subject_id, task)]
fwrite(by_file, file.path(DIAG, "T1_by_file.csv"))

sum_gen <- function(dd, subset_name) {
  qd <- q5(dd$median_d)
  qn <- q5(dd$n)
  qs <- q5(dd$span_core)
  files_u <- unique(dd, by = c("subject_id", "task"))
  qf <- q5(files_u$file_dt)
  data.table(
    generation = dd$generation[1], subset = subset_name, n_trials = nrow(dd),
    n_files = uniqueN(dd, by = c("subject_id", "task")),
    median_d_min = qd[1], median_d_p25 = qd[2], median_d_p50 = qd[3], median_d_p75 = qd[4], median_d_max = qd[5],
    frac_trials_median_near_0005 = mean(abs(dd$median_d - 0.0005) < 1e-6),
    frac_trials_median_near_0035 = mean(abs(dd$median_d - 0.0035) < 1e-6),
    frac_trials_median_near_004 = mean(abs(dd$median_d - 0.004) < 1e-6),
    frac_trials_median_near_005 = mean(abs(dd$median_d - 0.005) < 1e-6),
    n_min = qn[1], n_p50 = qn[3], n_max = qn[5],
    n_trials_3425 = sum(dd$n == 3425), n_trials_3426 = sum(dd$n == 3426),
    span_p50 = qs[3], span_min = qs[1], span_max = qs[5],
    file_dt_p50 = qf[3], file_dt_min = qf[1], file_dt_max = qf[5],
    n_files_file_dt_near_0035 = uniqueN(dd[abs(file_dt - 0.0035) < 1e-6], by = c("subject_id", "task")),
    n_files_file_dt_near_004 = uniqueN(dd[abs(file_dt - 0.004) < 1e-6], by = c("subject_id", "task")),
    n_files_file_dt_le_0.001 = uniqueN(dd[file_dt <= 0.001], by = c("subject_id", "task"))
  )
}
gen_rows <- list()
for (g in c("dec21", "dec25")) {
  dd <- t1[generation == g]
  gen_rows[[length(gen_rows) + 1L]] <- sum_gen(dd, "all")
  if (g == "dec21") {
    gen_rows[[length(gen_rows) + 1L]] <- sum_gen(dd[refused_phase2a %in% TRUE], "refused_phase2a")
    gen_rows[[length(gen_rows) + 1L]] <- sum_gen(dd[refused_phase2a %in% FALSE], "not_refused_phase2a")
  }
}
fwrite(rbindlist(gen_rows, fill = TRUE), file.path(DIAG, "T1_by_generation.csv"))

# T2
runs_all <- rbindlist(t2_runs, fill = TRUE)
fwrite(runs_all, file.path(DIAG, "T2b_label_runs.csv"))
labs <- rbindlist(t2_labels, fill = TRUE)
t2a <- labs[, .(n_rows = sum(N), n_files = uniqueN(paste(subject_id, task))), by = trial_label]
t2a[, whole_trial_only := n_files == nrow(id21) & .N == 1L]
fwrite(t2a, file.path(DIAG, "T2a_labels.csv"))
log_line("T2 labels: ", paste(t2a$trial_label, t2a$n_rows, sep = "=", collapse = "; "))

# Map only when a label is unambiguous AND the run is not the entire trial.
phase_map <- c(
  grip = "grip_gauge", squeeze = "grip_gauge", grip_gauge = "grip_gauge", force = "grip_gauge",
  fixation = "fixation", fix = "fixation",
  isi = "isi", interstimulus = "isi",
  release = "release_blank",
  response = "response", resp = "response",
  confidence = "confidence", conf = "confidence"
)
phase_dur <- c(grip_gauge = 3.00, fixation = 0.50, isi = 0.50, release_blank = 0.25, response = 3.00, confidence = 3.00)
runs_all[, label_key := tolower(trimws(label))]
runs_all[, phase := phase_map[label_key]]
runs_all[is.na(phase) & label_key %in% names(phase_map), phase := phase_map[label_key]]
# whole-trial ITI_Baseline is not the pre-trial phase
n_by_trial <- runs_all[, .(n_samples = sum(length_samples), n_runs = .N), by = .(subject_id, task, session, run, trial)]
runs_all <- merge(runs_all, n_by_trial, by = c("subject_id", "task", "session", "run", "trial"))
runs_all[, whole_trial_run := n_runs == 1L & length_samples == n_samples]
unmapped <- unique(runs_all[is.na(phase) | whole_trial_run == TRUE, .(label, whole_trial_run)])
mapped <- runs_all[!is.na(phase) & whole_trial_run == FALSE]
mapped <- mapped[phase %in% names(phase_dur)]
mapped[, designed_duration := phase_dur[phase]]
mapped[, implied_dt := designed_duration / length_samples]
if (nrow(mapped)) {
  t2c <- mapped[, .(
    n_runs = .N,
    implied_dt_median = median(implied_dt),
    implied_dt_iqr = IQR(implied_dt),
    implied_dt_min = min(implied_dt),
    implied_dt_max = max(implied_dt),
    length_median = as.numeric(median(length_samples))
  ), by = .(generation, phase, designed_duration)]
} else {
  t2c <- data.table(
    generation = "dec21", phase = "UNDETERMINED", designed_duration = NA_real_, n_runs = 0L,
    implied_dt_median = NA_real_, implied_dt_iqr = NA_real_, implied_dt_min = NA_real_, implied_dt_max = NA_real_,
    length_median = NA_real_,
    reason = "No trial_label run could be mapped to a designed phase of at least 0.25 s. The observed labels are listed in T2a_labels.csv. A label that fills the whole trial was not treated as that phase."
  )
}
fwrite(t2c, file.path(DIAG, "T2c_implied_dt.csv"))
grip_runs <- runs_all[phase == "grip_gauge" & whole_trial_run == FALSE]
if (nrow(grip_runs)) {
  onset <- grip_runs[, .(squeeze_onset_index = min(first_index)), by = .(generation, subject_id, task, session, run, trial)]
  onset_sum <- onset[, .(
    n_trials = .N, median_index = as.numeric(median(squeeze_onset_index)), iqr = IQR(squeeze_onset_index),
    min_index = min(squeeze_onset_index), max_index = max(squeeze_onset_index)
  ), by = generation]
} else {
  onset <- data.table()
  onset_sum <- data.table(
    generation = "dec21", n_trials = 0L, median_index = NA_real_, iqr = NA_real_,
    min_index = NA_integer_, max_index = NA_integer_,
    reason = "UNDETERMINED: no sample is labeled as the grip gauge. trial_label does not identify squeeze onset."
  )
}
fwrite(if (nrow(onset)) onset else onset_sum, file.path(DIAG, "T2c_squeeze_onset_index.csv"))
writeLines(c(
  "Mapping used when the label is one of: grip/squeeze/grip_gauge/force -> grip gauge (3.00 s); fixation/fix -> fixation (0.50 s); isi/interstimulus -> ISI (0.50 s); release -> release blank (0.25 s); response/resp -> response (3.00 s); confidence/conf -> confidence (3.00 s).",
  "A run that is the only run in the trial and has length equal to the trial was not mapped. ITI_Baseline filling every sample is in that class.",
  "Unmapped labels:",
  if (nrow(unmapped)) paste(unmapped$label, "whole_trial_run", unmapped$whole_trial_run, collapse = " | ") else "none"
), file.path(DIAG, "T2_mapping.txt"))

# T3
log_line("T3 alignment on ", nrow(pairs), " pairs")
t3_rows <- list()
for (p in seq_len(nrow(pairs))) {
  subj <- pairs$subject_id[p]
  tsk <- pairs$task[p]
  p21 <- pairs$path_21[p]
  p25 <- pairs$path_25[p]
  log_line("T3 ", subj, " ", tsk)
  d21 <- read_flat(p21, c("session_used", "run_used", "trial_in_run_raw", "time", "pupil"))
  seg_cols <- c("seg_start_rel_used", "seg_end_rel_used", "seg_end_source")
  hdr25 <- names(fread(p25, nrows = 0L))
  miss_seg <- setdiff(seg_cols, hdr25)
  if (length(miss_seg)) stop_md("T3", "columns", paste("December 25 file is missing column(s):", paste(miss_seg, collapse = ", "), "in", p25))
  d25 <- read_flat(p25, c("session_used", "run_used", "trial_in_run_raw", "time", "pupil", seg_cols))
  d21 <- d21[session_used %in% c(2L, 3L)]
  d25 <- d25[session_used %in% c(2L, 3L)]
  d21[, `:=`(session_used = as.integer(session_used), run_used = as.integer(run_used), trial_in_run_raw = as.integer(trial_in_run_raw))]
  d25[, `:=`(session_used = as.integer(session_used), run_used = as.integer(run_used), trial_in_run_raw = as.integer(trial_in_run_raw))]
  keys <- merge(
    unique(d21[, .(session_used, run_used, trial_in_run_raw)]),
    unique(d25[, .(session_used, run_used, trial_in_run_raw)]),
    by = c("session_used", "run_used", "trial_in_run_raw")
  )
  sp21 <- split(seq_len(nrow(d21)), paste(d21$session_used, d21$run_used, d21$trial_in_run_raw))
  sp25 <- split(seq_len(nrow(d25)), paste(d25$session_used, d25$run_used, d25$trial_in_run_raw))
  rows_p <- vector("list", nrow(keys))
  for (j in seq_len(nrow(keys))) {
    nm <- paste(keys$session_used[j], keys$run_used[j], keys$trial_in_run_raw[j])
    a <- copy(d21[sp21[[nm]]])
    b <- copy(d25[sp25[[nm]]])
    n21 <- nrow(a)
    n25 <- nrow(b)
    dmed21 <- median(diff(a$time)[is.finite(diff(a$time))])
    dmed25 <- median(diff(b$time)[is.finite(diff(b$time))])
    seg_start <- b$seg_start_rel_used[1]
    seg_end <- b$seg_end_rel_used[1]
    seg_src <- as.character(b$seg_end_source[1])
    a_f <- a[, i21 := seq_len(.N)][is.finite(pupil)]
    b_f <- b[, i25 := seq_len(.N)][is.finite(pupil)]
    ua <- a_f[, .N, by = pupil][N == 1L]
    ub <- b_f[, .N, by = pupil][N == 1L]
    mm <- merge(a_f[ua, on = "pupil", .(pupil, i21)], b_f[ub, on = "pupil", .(pupil, i25)], by = "pupil")
    n_unique <- nrow(mm)
    if (n_unique >= 50L) {
      fit_i <- linfit(mm$i25, mm$i21)
      method <- "least_squares"
      aa <- fit_i$intercept
      bb <- fit_i$slope
      resid_i <- fit_i$resid_sd
      grid_r <- NA_real_
      grid_n <- n_unique
      n_tie <- NA_integer_
      r_b100 <- NA_real_
      r_b087 <- NA_real_
      r_b088 <- NA_real_
      r_b114 <- NA_real_
      fit_t <- linfit(b$time[mm$i25], a$time[mm$i21])
    } else {
      g <- grid_align(a$pupil, b$pupil)
      method <- "correlation_grid"
      aa <- g$a
      bb <- g$b
      grid_r <- g$r
      grid_n <- g$n
      n_tie <- g$n_tie
      r_b100 <- g$r_b100
      r_b087 <- g$r_b087
      r_b088 <- g$r_b088
      r_b114 <- g$r_b114
      if (is.finite(aa) && is.finite(bb)) {
        i25 <- seq_len(n25)
        i21 <- as.integer(aa + round(bb * i25))
        inr <- i21 >= 1L & i21 <= n21
        fit_i <- linfit(i25[inr], i21[inr])
        resid_i <- fit_i$resid_sd
        fit_t <- linfit(b$time[inr], a$time[i21[inr]])
      } else {
        resid_i <- NA_real_
        fit_t <- list(intercept = NA_real_, slope = NA_real_, resid_sd = NA_real_, n = 0L)
      }
    }
    rows_p[[j]] <- data.table(
      subject_id = subj, task = tsk, session = keys$session_used[j], run = keys$run_used[j],
      trial = keys$trial_in_run_raw[j], refused_phase2a = paste(subj, tsk) %in% paste(ref$subject_id, ref$task),
      n21 = n21, n25 = n25, median_d21 = dmed21, median_d25 = dmed25,
      seg_start_rel_used = seg_start, seg_end_rel_used = seg_end, seg_end_source = seg_src,
      n_unique_pupil_pairs = n_unique, method = method,
      a = aa, b = bb, index_resid_sd = resid_i, grid_r = grid_r, grid_n = grid_n, n_tie_lags = n_tie,
      r_at_b_1.00 = r_b100, r_at_b_0.87 = r_b087, r_at_b_0.88 = r_b088, r_at_b_1.14 = r_b114,
      time_c = fit_t$intercept, time_e = fit_t$slope, time_resid_sd = fit_t$resid_sd, time_n = fit_t$n
    )
  }
  t3_rows[[p]] <- rbindlist(rows_p, fill = TRUE)
  rm(d21, d25)
  gc(verbose = FALSE)
}
t3 <- rbindlist(t3_rows, fill = TRUE)
fwrite(t3, file.path(DIAG, "T3_trials.csv"))
t3_pair <- t3[, .(
  n_trials = .N,
  n21_median = as.numeric(median(n21)),
  n25_median = as.numeric(median(n25)),
  b_median = as.numeric(median(b, na.rm = TRUE)),
  e_median = as.numeric(median(time_e, na.rm = TRUE)),
  seg_start_median = as.numeric(median(seg_start_rel_used, na.rm = TRUE)),
  seg_end_median = as.numeric(median(seg_end_rel_used, na.rm = TRUE)),
  median_d25 = median(median_d25, na.rm = TRUE),
  median_d21 = median(median_d21, na.rm = TRUE),
  implied_dec21_step = median(median_d25 / b, na.rm = TRUE),
  grid_r_median = median(grid_r, na.rm = TRUE),
  r_at_b_1.00_median = median(r_at_b_1.00, na.rm = TRUE),
  r_at_b_1.14_median = median(r_at_b_1.14, na.rm = TRUE),
  frac_method_grid = mean(method == "correlation_grid"),
  refused_phase2a = refused_phase2a[1]
), by = .(subject_id, task)]
fwrite(t3_pair, file.path(DIAG, "T3_by_pair.csv"))
log_line("T3 done")

# T4 landmarks
task_mean <- function(axis_name) {
  out <- list()
  for (tsk in c("ADT", "VDT")) {
    mats <- lapply(waves, function(w) if (w$task == tsk) {
      ww <- w[[axis_name]]
      ifelse(ww$counts > 0, ww$sums / ww$counts, NA_real_)
    })
    mats <- mats[!vapply(mats, is.null, logical(1))]
    if (!length(mats)) {
      out[[tsk]] <- list(y = rep(NA_real_, length(GRID)), n_subj = 0L, n_trials = 0L)
      next
    }
    M <- do.call(cbind, mats)
    y <- rowMeans(M, na.rm = TRUE)
    y[!is.finite(y)] <- NA_real_
    ntr <- sum(vapply(waves, function(w) if (w$task == tsk) w[[axis_name]]$n_trials else 0L, integer(1)))
    out[[tsk]] <- list(y = y, n_subj = length(mats), n_trials = ntr)
  }
  out
}
smooth5 <- function(y) as.numeric(stats::filter(y, rep(1 / 5, 5), sides = 2))
two_mins <- function(y) {
  s <- smooth5(y)
  n <- length(s)
  is_min <- rep(FALSE, n)
  if (n >= 3L) {
    is_min[2:(n - 1L)] <- is.finite(s[2:(n - 1L)]) & s[2:(n - 1L)] < s[1:(n - 2L)] & s[2:(n - 1L)] < s[3:n]
  }
  cands <- which(is_min & GRID >= 3.6 & GRID <= 5.6)
  if (length(cands) < 2L) return(list(t1 = NA_real_, t2 = NA_real_, interval = NA_real_, n_local = length(cands)))
  ord <- cands[order(s[cands])]
  tt <- sort(GRID[ord[1:2]])
  list(t1 = tt[1], t2 = tt[2], interval = tt[2] - tt[1], n_local = length(cands))
}
global_min <- function(y) {
  w <- which(GRID >= 2 & GRID <= 5 & is.finite(y))
  if (!length(w)) return(NA_real_)
  GRID[w[which.min(y[w])]]
}
land <- list()
png(file.path(DIAG, "T4_difference_waves.png"), width = 1100, height = 800)
op <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
for (ax in c("A", "B")) {
  step <- if (ax == "A") 0.0035 else 0.004
  tm <- task_mean(ax)
  diffy <- tm$VDT$y - tm$ADT$y
  sm <- smooth5(diffy)
  mins <- two_mins(diffy)
  land[[ax]] <- data.table(
    axis = ax, step_s = step,
    n_subj_ADT = tm$ADT$n_subj, n_subj_VDT = tm$VDT$n_subj,
    n_trials_ADT = tm$ADT$n_trials, n_trials_VDT = tm$VDT$n_trials,
    diff_min_1 = mins$t1, diff_min_2 = mins$t2, diff_interval = mins$interval, n_local_minima = mins$n_local,
    adt_global_min_2_5 = global_min(tm$ADT$y),
    vdt_global_min_2_5 = global_min(tm$VDT$y)
  )
  plot(GRID, sm, type = "l", xlab = "Time (s)", ylab = "VDT - ADT, 0.10 s mean",
       main = paste0("Axis ", ax, " step ", step, " s"))
  abline(v = c(3.0, 3.25, 3.75, 4.35, 4.45, 4.70), lty = 2)
}
dev.off()
par(op)
land_dt <- rbindlist(land)
# Implied step if the observed interval were scaled onto the designed 0.60 s.
land_dt[, step_if_interval_were_0.60 := step_s * (0.60 / diff_interval)]
fwrite(land_dt, file.path(DIAG, "T4_landmarks.csv"))
log_line("T4 intervals ", paste(land_dt$axis, land_dt$diff_interval, collapse = " | "))

# T5
bib_rows <- list()
find_hits <- system2("find", c("/Users/mohdasti/Documents/GitHub", "-name", "references.bib", "-size", "+10k"), stdout = TRUE, stderr = TRUE)
writeLines(find_hits, file.path(DIAG, "T5_find_references_bib.txt"))
ch2_keys <- fread(KEYS75)$bib_key
check_bib <- function(path) {
  ex <- file.exists(path)
  if (!ex) {
    return(data.table(path = path, exists = FALSE, bytes = NA_real_, md5 = NA_character_,
                      n_at_entries = NA_integer_, example_keys = NA_character_,
                      n_ch2_keys = length(ch2_keys), n_exact_match = NA_integer_,
                      n_case_insensitive_match = NA_integer_, note = "file not found"))
  }
  info <- file.info(path)
  lines <- readLines(path, warn = FALSE)
  at <- grep("^@", lines, value = TRUE)
  ekeys <- sub(",.*$", "", sub("^@[A-Za-z]+\\{", "", at))
  ekeys <- trimws(ekeys)
  data.table(
    path = path, exists = TRUE, bytes = info$size, md5 = unname(tools::md5sum(path)),
    n_at_entries = length(at),
    example_keys = paste(head(ekeys, 10), collapse = " | "),
    n_ch2_keys = length(ch2_keys),
    n_exact_match = sum(ch2_keys %in% ekeys),
    n_case_insensitive_match = sum(tolower(ch2_keys) %in% tolower(ekeys)),
    note = ""
  )
}
bib_tab <- check_bib(BIB_MASTER)
fwrite(bib_tab, file.path(DIAG, "T5_master_bib.csv"))
log_line("T5 master bib exists ", bib_tab$exists, " exact matches ", bib_tab$n_exact_match, " of ", bib_tab$n_ch2_keys)

# report
g21 <- fread(file.path(DIAG, "T1_by_generation.csv"))
g_all <- g21[generation == "dec21" & subset == "all"]
g_ref <- g21[generation == "dec21" & subset == "refused_phase2a"]
g_ok <- g21[generation == "dec21" & subset == "not_refused_phase2a"]
g25 <- g21[generation == "dec25" & subset == "all"]
fmt <- function(x) if (!length(x) || !is.finite(x[1])) "UNDETERMINED" else format(x[1], digits = 8, trim = TRUE)
t1_step <- g_all$median_d_p50
t1_onset <- if (is.finite(t1_step)) 1 + 3 / t1_step else NA_real_
t1_same <- if (is.finite(g_ref$median_d_p50) && is.finite(g_ok$median_d_p50) && abs(g_ref$median_d_p50 - g_ok$median_d_p50) < 1e-6) "yes" else "no"
t2_step <- if (nrow(t2c) && "implied_dt_median" %in% names(t2c) && any(is.finite(t2c$implied_dt_median))) {
  fmt(median(t2c$implied_dt_median, na.rm = TRUE))
} else "UNDETERMINED"
t2_onset <- if (nrow(onset_sum) && any(is.finite(onset_sum$median_index))) fmt(onset_sum$median_index[1]) else "UNDETERMINED"
t3_step <- median(t3_pair$implied_dec21_step, na.rm = TRUE)
t3_ref <- t3_pair[refused_phase2a %in% TRUE]
t3_acc <- t3_pair[refused_phase2a %in% FALSE]
t3_same <- if (nrow(t3_ref) && nrow(t3_acc) && is.finite(median(t3_ref$b_median, na.rm = TRUE)) && is.finite(median(t3_acc$b_median, na.rm = TRUE))) {
  if (abs(median(t3_ref$b_median, na.rm = TRUE) - median(t3_acc$b_median, na.rm = TRUE)) <= 0.02) "yes" else "no"
} else "UNDETERMINED"
t3_onset <- if (is.finite(t3_step)) 1 + 3 / t3_step else NA_real_
t4_cell <- paste(vapply(seq_len(nrow(land_dt)), function(i) {
  paste0(land_dt$axis[i], " interval ", fmt(land_dt$diff_interval[i]), " s on step ", land_dt$step_s[i],
         " (step if interval were 0.60 s: ", fmt(land_dt$step_if_interval_were_0.60[i]), ")")
}, character(1)), collapse = "; ")

evidence <- c(
  "| Test | implied December 21 step (s) | implied squeeze-onset sample | consistent across refused and accepted files? | n | file |",
  "|---|---|---|---|---|---|",
  paste0("| T1 | ", fmt(t1_step), " | ", fmt(t1_onset), " if sample 1 is at -3 s | ", t1_same,
         " | ", g_all$n_trials, " December 21 trials | T1_by_generation.csv |"),
  paste0("| T2 | ", t2_step, " | ", t2_onset, " | UNDETERMINED | ", sum(t2a$n_rows), " labeled rows | T2c_implied_dt.csv |"),
  paste0("| T3 | ", fmt(t3_step), " | ", fmt(t3_onset), " if sample 1 is at -3 s | ", t3_same,
         " | ", nrow(t3), " matched trials | T3_by_pair.csv |"),
  paste0("| T4 | ", t4_cell, " | UNDETERMINED | not compared; accepted step_from_m3 trials only | ",
         sum(land_dt$n_trials_ADT + land_dt$n_trials_VDT) / 2, " trials per axis | T4_landmarks.csv |")
)

para_t1 <- paste0(
  "T1 computed, inside each session 2–3 trial and in file row order, the sample count, the median consecutive time difference, the mode of differences rounded to 6 decimals, the fraction of finite differences within 1e-6 of 0.0005, 0.0035, 0.004 and 0.005, the counts of non-positive and of >0.05 s differences, and span_core = time[n] - time[1]. ",
  "December 21, all trials: median of per-trial median(d) = ", fmt(g_all$median_d_p50),
  " s (IQR ", fmt(g_all$median_d_p25), " to ", fmt(g_all$median_d_p75),
  "); fraction of trials whose median is within 1e-6 of 0.004 = ", fmt(g_all$frac_trials_median_near_004),
  "; of 0.0035 = ", fmt(g_all$frac_trials_median_near_0035),
  "; n = ", g_all$n_trials, " trials in ", g_all$n_files, " files. ",
  "Sample count median ", fmt(g_all$n_p50), " (min ", fmt(g_all$n_min), ", max ", fmt(g_all$n_max),
  "); trials with 3,425 samples ", g_all$n_trials_3425, "; with 3,426 samples ", g_all$n_trials_3426, ". ",
  "Phase 2a refused recordings: median of per-trial median(d) = ", fmt(g_ref$median_d_p50),
  " s, fraction near 0.004 = ", fmt(g_ref$frac_trials_median_near_004),
  ", n = ", g_ref$n_trials, " trials in ", g_ref$n_files, " files. ",
  "Not refused: median ", fmt(g_ok$median_d_p50), " s, fraction near 0.004 = ", fmt(g_ok$frac_trials_median_near_004),
  ", n = ", g_ok$n_trials, ". ",
  "file_dt(), the whole-file median of positive differences of unique timestamps, has median ", fmt(g_all$file_dt_p50),
  " s on December 21 (min ", fmt(g_all$file_dt_min), ", max ", fmt(g_all$file_dt_max),
  "); ", g_all$n_files_file_dt_near_0035, " files are within 1e-6 of 0.0035 s and ",
  g_all$n_files_file_dt_le_0.001, " files are ≤ 0.001 s. ",
  "Among refused files the file_dt median is ", fmt(g_ref$file_dt_p50), " s (min ", fmt(g_ref$file_dt_min), ", max ", fmt(g_ref$file_dt_max),
  "); among files that were not refused it is ", fmt(g_ok$file_dt_p50), " s. ",
  "December 25, all trials: median of per-trial median(d) = ", fmt(g25$median_d_p50),
  " s, fraction near 0.004 = ", fmt(g25$frac_trials_median_near_004),
  ", fraction near 0.0035 = ", fmt(g25$frac_trials_median_near_0035),
  ", n = ", g25$n_trials, " trials, sample-count median ", fmt(g25$n_p50), "."
)
para_t2 <- paste0(
  "T2a counted trial_label on every December 21 row in sessions 2–3. Distinct labels: ",
  paste(paste0(t2a$trial_label, " (", t2a$n_rows, " rows, ", t2a$n_files, " files)"), collapse = "; "),
  ". T2b stored each run of identical labels. ",
  if (!nrow(mapped)) {
    "No run was mapped to a designed phase of at least 0.25 s, so implied_dt is UNDETERMINED and the squeeze-onset index is UNDETERMINED. "
  } else {
    paste0("Mapped runs give the implied_dt values in T2c_implied_dt.csv. ")
  },
  "Where a label fills every sample of a trial, the run length equals the trial length in every such trial. Those labels were not used as a phase clock. The mapping that was attempted is in T2_mapping.txt."
)
para_t3 <- paste0(
  "T3 matched ", nrow(t3), " trials on session_used, run_used and trial_in_run_raw across ", nrow(pairs),
  " subject × task pairs (T3_pairs_selected.csv). ",
  "Median n21 = ", fmt(median(t3$n21)), ", median n25 = ", fmt(median(t3$n25)),
  ". Median of per-trial median(diff(time)) is ", fmt(median(t3$median_d21, na.rm = TRUE)),
  " s in December 21 and ", fmt(median(t3$median_d25, na.rm = TRUE)), " s in December 25. ",
  "December 25 segment start median ", fmt(median(t3$seg_start_rel_used, na.rm = TRUE)),
  " s, segment end median ", fmt(median(t3$seg_end_rel_used, na.rm = TRUE)), " s. ",
  "Exact unique pupil matches numbered ≥ 50 in ", sum(t3$n_unique_pupil_pairs >= 50, na.rm = TRUE),
  " trials; the other ", sum(t3$method == "correlation_grid"), " trials used the correlation grid. ",
  "Median index scale b = ", fmt(median(t3$b, na.rm = TRUE)),
  ", median time slope e = ", fmt(median(t3$time_e, na.rm = TRUE)),
  ", median grid correlation = ", fmt(median(t3$grid_r, na.rm = TRUE)),
  ". Median correlation at the grid point b = 1.00 is ", fmt(median(t3$r_at_b_1.00, na.rm = TRUE)),
  "; at b = 1.14 it is ", fmt(median(t3$r_at_b_1.14, na.rm = TRUE)),
  "; at b = 0.87 it is ", fmt(median(t3$r_at_b_0.87, na.rm = TRUE)),
  "; at b = 0.88 it is ", fmt(median(t3$r_at_b_0.88, na.rm = TRUE)), ". ",
  "The December 21 step implied by median(median_d25 / b) across pairs is ", fmt(t3_step), " s. ",
  "Refused pairs (n = ", nrow(t3_ref), "): median b = ", fmt(median(t3_ref$b_median, na.rm = TRUE)),
  ", median implied step = ", fmt(median(t3_ref$implied_dec21_step, na.rm = TRUE)),
  " s. Other pairs (n = ", nrow(t3_acc), "): median b = ", fmt(median(t3_acc$b_median, na.rm = TRUE)),
  ", median implied step = ", fmt(median(t3_acc$implied_dec21_step, na.rm = TRUE)), " s."
)
para_t4 <- paste0(
  "T4 rebuilt the B0-corrected subject-equal mean on the ", nrow(acc),
  " trials with time_rule step_from_m3. Axis A steps 0.0035 s from -3. Axis B steps 0.004 s from -3. B0 is [-0.5, 0). Bins are 0.02 s. The VDT − ADT wave was smoothed with a 5-bin (0.10 s) running mean. ",
  paste(apply(land_dt, 1, function(r) paste0(
    "Axis ", r[["axis"]], " (step ", r[["step_s"]], " s): difference-wave minima at ", r[["diff_min_1"]],
    " s and ", r[["diff_min_2"]], " s, interval ", r[["diff_interval"]],
    " s (", r[["n_local_minima"]], " local minima in [3.6, 5.6]); ADT global minimum in [2, 5] at ",
    r[["adt_global_min_2_5"]], " s; VDT global minimum at ", r[["vdt_global_min_2_5"]],
    " s; subjects ", r[["n_subj_ADT"]], "/", r[["n_subj_VDT"]],
    "; trials ", r[["n_trials_ADT"]], "/", r[["n_trials_VDT"]], "."
  )), collapse = " ")
)
para_t5 <- paste0(
  "T5 checked ", BIB_MASTER, ". Exists: ", bib_tab$exists,
  ". Bytes: ", bib_tab$bytes, ". md5: ", bib_tab$md5,
  ". @ entries: ", bib_tab$n_at_entries,
  ". Example keys: ", bib_tab$example_keys,
  ". Of ", bib_tab$n_ch2_keys, " Chapter 2 keys, exact matches: ", bib_tab$n_exact_match,
  "; case-insensitive matches: ", bib_tab$n_case_insensitive_match, "."
)

report <- c(
  "# Chapter 2 Phase 2a.1 axis check",
  "",
  paste("Run", RUN),
  "",
  "This report states what the files show. It does not choose a sampling interval.",
  "",
  "## 1. Evidence",
  "",
  evidence,
  "",
  "## 2. Tests",
  "",
  para_t1,
  "",
  para_t2,
  "",
  para_t3,
  "",
  para_t4,
  "",
  para_t5,
  "",
  "## 3. Deviations",
  "",
  "T3 correlation grid evaluates i21 = a + round(b * i25). The prompt writes i21 = a + b * i25. With integer a, those two differ by at most one sample. The b grid is 0.70, 0.71, ..., 1.30, so 0.875 is not a grid point; the neighboring points 0.87 and 0.88 are reported.",
  "T3 least squares is the closed-form slope for the index or time pairs named in the prompt. No mixed model was fit.",
  "T4 bins with findInterval(t, GRID - 0.01) on GRID = seq(-1, 8, by = 0.02), the Chapter 3 waveform grid. A trial with fewer than 10 finite samples in [-0.5, 0) is left out of the mean.",
  "T2 did not map a label onto a designed phase when that label was the only label in the trial.",
  "",
  "## 4. UNDETERMINED",
  "",
  if (!nrow(mapped)) "T2 implied_dt is UNDETERMINED. trial_label does not separate the designed phases." else "T2 produced implied_dt for the mapped phases in T2c_implied_dt.csv.",
  if (!nrow(grip_runs)) "The squeeze-onset sample is UNDETERMINED from labels. No grip-gauge label is present." else "Squeeze-onset indexes are in T2c_squeeze_onset_index.csv.",
  "T4 does not identify squeeze onset, so that cell of the evidence table is UNDETERMINED.",
  "T0 shows why native_span reached thousands of seconds: the 10 largest within-trial time gaps are in T0_time_jumps.csv.",
  ""
)
writeLines(report, file.path(DIAG, "report.md"))
log_line("Wrote report.md")
zip_path <- package_zip()
unzip_ok <- system2("unzip", c("-t", zip_path), stdout = FALSE, stderr = FALSE) == 0L
log_line("SELF-CHECK zip opens: ", unzip_ok)
log_line("Phase 2a.1 finished")
owd <- getwd()
setwd(DIAG)
utils::zip(zip_path, "run_log.txt")
setwd(owd)
quit(status = 0L)
