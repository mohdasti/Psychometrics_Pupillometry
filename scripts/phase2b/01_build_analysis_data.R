#!/usr/bin/env Rscript
# Chapter 2 Phase 2b, part 1. Build analysis tables for the locked models.
# Sample sizes are gates. A mismatch writes STOPPED_1.md and ends.

options(warn = 1, scipen = 999)
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) {
  stop("Usage: Rscript scripts/phase2b/01_build_analysis_data.R RUN", call. = FALSE)
}
RUN <- args[[1L]]

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
BEH_PATH <- file.path(CH2, "data/external/lc_psychometric_share/data/aggregated/bap_beh_trialdata_v2.csv")
PUP_PATH <- file.path(CH2, "data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_pupil_trials_r2.csv")
TEST <- file.path(CH2, "tests/test_ch2_coding.R")
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2b", RUN)
MODELS <- file.path(CH2, "output/models/ch2_phase2b", RUN)
BEH_MD5_EXPECTED <- "3c1f98cce6c9f0812b621901c85817c7"
N_BEH <- 18266L
N_M2 <- 5240L
N_TEPR <- 6299L
N_M5 <- 12630L
ID <- c("subject_id", "task", "session", "run", "trial")

dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
dir.create(MODELS, recursive = TRUE, showWarnings = FALSE)

LOG <- file.path(DIAG, "run_log.txt")
log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
  flush(stdout())
}

check_rows <- list()
add_check <- function(check, expected, observed, pass, gate) {
  check_rows[[length(check_rows) + 1L]] <<- data.table(
    check = check,
    expected = as.character(expected),
    observed = as.character(observed),
    pass = isTRUE(pass),
    gate = isTRUE(gate)
  )
  fwrite(rbindlist(check_rows), file.path(DIAG, "P1_build_checks.csv"))
  invisible(pass)
}

write_stop <- function(step, detail) {
  path <- file.path(DIAG, "STOPPED_1.md")
  writeLines(c(
    "# STOPPED_1",
    "",
    "Part: 1",
    paste("Step:", step),
    "Fatal: TRUE",
    "",
    "## Error or count",
    "",
    detail
  ), path)
  log_line("WROTE ", path)
  path
}

package_zip <- function() {
  zip_path <- file.path(CH2, "output/diagnostics/ch2_phase2b", paste0("ch2_phase2b_", RUN, ".zip"))
  stage <- file.path(tempdir(), paste0("ch2_phase2b_stage_", RUN))
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  copy_dir <- function(dir) {
    if (!dir.exists(dir)) return(invisible())
    files <- list.files(dir, full.names = TRUE, recursive = FALSE)
    for (f in files) {
      if (file.exists(f) && !dir.exists(f)) file.copy(f, file.path(stage, basename(f)), overwrite = TRUE)
    }
  }
  copy_dir(DIAG)
  copy_dir(MODELS)
  file.copy(file.path(CH2, "scripts/phase2b/01_build_analysis_data.R"), stage, overwrite = TRUE)
  file.copy(file.path(CH2, "scripts/phase2b/02_fit_models.R"), stage, overwrite = TRUE)
  file.copy(TEST, stage, overwrite = TRUE)
  if (file.exists(zip_path)) unlink(zip_path)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(stage)
  utils::zip(zip_path, list.files("."))
  log_line("ZIP ", zip_path)
  zip_path
}

fail <- function(step, detail) {
  log_line("STOP_1 ", step, " ", detail)
  write_stop(step, detail)
  try(package_zip(), silent = TRUE)
  cat("EXIT: 1\n")
  flush(stdout())
  quit(save = "no", status = 1L)
}

gate_identical <- function(check, expected, observed) {
  pass <- identical(as.character(expected), as.character(observed))
  add_check(check, expected, observed, pass, TRUE)
  if (!pass) fail(check, paste0("expected ", expected, " observed ", observed))
}

state_trait <- function(df, col, prefix) {
  if (!is.data.table(df)) stop("state_trait expects a data.table", call. = FALSE)
  if (!col %in% names(df)) stop("state_trait missing column ", col, call. = FALSE)
  if (any(!is.finite(df[[col]]))) stop("state_trait non-finite values in ", col, call. = FALSE)
  values <- df[[col]]
  df[, st_v := values]
  cells <- df[, .(
    n = .N,
    trait_raw = mean(.SD[[1L]]),
    ss = sum((.SD[[1L]] - mean(.SD[[1L]]))^2)
  ), by = .(subject_id, task), .SDcols = col]
  den <- sum(as.numeric(cells$n) - 1)
  if (!is.finite(den) || den <= 0) stop("state_trait within-cell df is ", den, " for ", col, call. = FALSE)
  sd_within <- sqrt(sum(cells$ss) / den)
  if (!is.finite(sd_within) || sd_within == 0) stop("state_trait sd_within is ", sd_within, " for ", col, call. = FALSE)
  mu <- mean(cells$trait_raw)
  sdt <- stats::sd(cells$trait_raw)
  if (!is.finite(sdt) || sdt == 0) stop("state_trait trait sd is ", sdt, " for ", col, call. = FALSE)
  cells[, trait_z := (trait_raw - mu) / sdt]
  df[cells, on = .(subject_id, task), st_trait_raw := i.trait_raw]
  df[cells, on = .(subject_id, task), st_trait_z := i.trait_z]
  state_nm <- paste0(prefix, "_state")
  trait_nm <- paste0(prefix, "_trait")
  df[, (state_nm) := (st_v - st_trait_raw) / sd_within]
  df[, (trait_nm) := st_trait_z]
  df[, c("st_v", "st_trait_raw", "st_trait_z") := NULL]
  means <- df[, .(m = mean(.SD[[1L]])), by = .(subject_id, task), .SDcols = state_nm]
  max_abs <- max(abs(means$m))
  if (!is.finite(max_abs) || max_abs > 1e-10) {
    stop(prefix, " within-cell state mean max abs ", format(max_abs, scientific = TRUE), call. = FALSE)
  }
  list(prefix = prefix, max_abs_cell_mean = max_abs, sd_within = sd_within, n_cells = nrow(cells))
}

options(error = function() {
  msg <- geterrmessage()
  try(log_line("UNCAUGHT ", msg), silent = TRUE)
  if (!file.exists(file.path(DIAG, "STOPPED_1.md"))) {
    try(write_stop("uncaught", msg), silent = TRUE)
  }
  try(package_zip(), silent = TRUE)
  cat("EXIT: 1\n")
  flush(stdout())
  quit(save = "no", status = 1L)
})

log_line("R ", R.version.string)
log_line("lme4 ", as.character(packageVersion("lme4")))
log_line("lmerTest ", as.character(packageVersion("lmerTest")))
log_line("data.table ", as.character(packageVersion("data.table")))
log_line("RUN ", RUN)

stopifnot(file.exists(BEH_PATH))
stopifnot(file.exists(PUP_PATH))
stopifnot(file.exists(TEST))

beh_md5 <- unname(tools::md5sum(BEH_PATH))
pup_md5 <- unname(tools::md5sum(PUP_PATH))
log_line("beh md5 ", beh_md5)
log_line("pup md5 ", pup_md5)
gate_identical("beh_md5", BEH_MD5_EXPECTED, tolower(beh_md5))
add_check("pup_md5", pup_md5, pup_md5, TRUE, FALSE)

beh <- fread(BEH_PATH)
pup <- fread(PUP_PATH)
gate_identical("beh_nrows", N_BEH, nrow(beh))
gate_identical("pup_nrows", N_BEH, nrow(pup))

if (any(!beh$task_modality %in% c("aud", "vis"))) {
  fail("task_modality", "task_modality has a value other than aud or vis")
}
beh[, task := fifelse(task_modality == "aud", "ADT", "VDT")]
beh[, session := session_num]
beh[, run := run_num]
beh[, trial := trial_num]

beh_dups <- beh[, .N, by = ID][N > 1L, .N]
pup_dups <- pup[, .N, by = ID][N > 1L, .N]
gate_identical("beh_duplicate_keys", 0L, beh_dups)
gate_identical("pup_duplicate_keys", 0L, pup_dups)

source(TEST)
test_err <- tryCatch({
  assert_ch2_coding(code_trials(make_coding_fixture()))
  NULL
}, error = function(e) conditionMessage(e))
if (!is.null(test_err)) fail("unit_test_fixture", test_err)

d_m1 <- code_trials(beh)
test_err <- tryCatch({
  assert_ch2_coding(d_m1)
  NULL
}, error = function(e) conditionMessage(e))
if (!is.null(test_err)) fail("unit_test_m1", test_err)
add_check("unit_test", "PASS", "PASS", TRUE, TRUE)

gate_identical("m1_nrows", N_BEH, nrow(d_m1))
gate_identical("m1_adt_participants", 66L, uniqueN(d_m1[task == "ADT", subject_id]))
gate_identical("m1_vdt_participants", 66L, uniqueN(d_m1[task == "VDT", subject_id]))
na_coding <- d_m1[, sum(!is.finite(diff)) + sum(!is.finite(catch)) + sum(!is.finite(x_c)) +
                    sum(!is.finite(effort_c)) + sum(!is.finite(task_c))]
gate_identical("m1_na_coding", 0L, na_coding)

beh_join <- beh[, .(subject_id, task, session, run, trial, resp_is_diff,
                    beh_is_high_grip = is_high_grip,
                    beh_stim_level_index = stim_level_index)]
joined <- merge(pup, beh_join, by = ID, all = TRUE, sort = FALSE)
gate_identical("join_nrows", N_BEH, nrow(joined))
gate_identical("join_unmatched_resp", 0L, sum(is.na(joined$resp_is_diff)))
grip_eq <- joined$is_high_grip == joined$beh_is_high_grip
level_eq <- joined$stim_level_index == joined$beh_stim_level_index
if (anyNA(grip_eq)) fail("grip_agree", paste("NA comparisons", sum(is.na(grip_eq))))
if (anyNA(level_eq)) fail("level_agree", paste("NA comparisons", sum(is.na(level_eq))))
gate_identical("grip_agree_n", N_BEH, sum(grip_eq))
gate_identical("level_agree_n", N_BEH, sum(level_eq))
joined[, c("beh_is_high_grip", "beh_stim_level_index") := NULL]

d_pup <- code_trials(joined)
d_m2 <- d_pup[g_grip_final == TRUE]
d_tepr <- d_pup[g_tepr_final == TRUE]
d_m5 <- d_pup[matched == TRUE]
d_m5[, in_tepr_final := as.integer(g_tepr_final %in% TRUE)]

gate_identical("m2_nrows", N_M2, nrow(d_m2))
gate_identical("tepr_nrows", N_TEPR, nrow(d_tepr))
gate_identical("tepr_finite_b1", N_TEPR, sum(is.finite(d_tepr$b1_mean)))
gate_identical("tepr_finite_tepr_mean", N_TEPR, sum(is.finite(d_tepr$tepr_mean_465_765)))
gate_identical("tepr_finite_tepr_auc", N_TEPR, sum(is.finite(d_tepr$tepr_auc)))
gate_identical("m5_nrows", N_M5, nrow(d_m5))
gate_identical("m5_in_tepr_final_sum", N_TEPR, sum(d_m5$in_tepr_final))

st_b1 <- state_trait(d_tepr, "b1_mean", "b1")
st_tepr <- state_trait(d_tepr, "tepr_mean_465_765", "tepr")
st_auc <- state_trait(d_tepr, "tepr_auc", "teprauc")
for (st in list(st_b1, st_tepr, st_auc)) {
  pass <- is.finite(st$max_abs_cell_mean) && st$max_abs_cell_mean <= 1e-10
  add_check(paste0(st$prefix, "_state_max_abs_cell_mean"), 0, st$max_abs_cell_mean, pass, TRUE)
  add_check(paste0(st$prefix, "_sd_within"), st$sd_within, st$sd_within, TRUE, FALSE)
  add_check(paste0(st$prefix, "_n_cells"), st$n_cells, st$n_cells, TRUE, FALSE)
  if (!pass) fail(paste0(st$prefix, "_state_mean"), paste("max abs", st$max_abs_cell_mean))
}

need_tepr <- c("b1_state", "b1_trait", "tepr_state", "tepr_trait", "teprauc_state", "teprauc_trait")
if (any(!need_tepr %in% names(d_tepr))) {
  fail("state_columns", paste("missing", paste(need_tepr[!need_tepr %in% names(d_tepr)], collapse = ",")))
}
if (any(!is.finite(d_tepr$b1_state)) || any(!is.finite(d_tepr$b1_trait)) ||
    any(!is.finite(d_tepr$tepr_state)) || any(!is.finite(d_tepr$tepr_trait)) ||
    any(!is.finite(d_tepr$teprauc_state)) || any(!is.finite(d_tepr$teprauc_trait))) {
  fail("state_finite", "a state or trait column has a non-finite value")
}

fwrite(d_m1, file.path(DIAG, "analysis_data_m1.csv"))
fwrite(d_m2, file.path(DIAG, "analysis_data_m2.csv"))
fwrite(d_tepr, file.path(DIAG, "analysis_data_tepr.csv"))
fwrite(d_m5, file.path(DIAG, "analysis_data_m5.csv"))
add_check("build_complete", "TRUE", "TRUE", TRUE, TRUE)
log_line("WROTE analysis tables")
log_line("m1 ", nrow(d_m1), " m2 ", nrow(d_m2), " tepr ", nrow(d_tepr), " m5 ", nrow(d_m5))
cat("EXIT: 0\n")
flush(stdout())
quit(save = "no", status = 0L)
