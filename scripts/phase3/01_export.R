# Phase 3 export. Verifies the saved fits and writes chapter materials.
# Does not refit. The only model call is predict() on the saved M1 fit.
# Run: Rscript scripts/phase3/01_export.R

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
DDM <- "/Users/mohdasti/Documents/GitHub/modeling-pupil-DDM"
RUN <- "ch2fit_20261024_193248"
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2b", RUN)
MODELS <- file.path(CH2, "output/models/ch2_phase2b", RUN)
EXPORT <- file.path(CH2, "07_manuscript/chapter2/export", RUN)
PRIOR <- file.path(CH2, "07_manuscript/chapter2/export", paste0(RUN, "_pre_phase3"))
PUPIL <- file.path(CH2, "data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_pupil_trials_r2.csv")
WAVE <- file.path(CH2, "data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_waveforms_subject_condition_r2.csv")
PF <- file.path(CH2, "data/external/lc_psychometric_share/data/aggregated")
M1DATA <- file.path(DIAG, "analysis_data_m1.csv")
REDLOG <- file.path(DIAG, "P2_reduction_log.csv")
AXIS <- file.path(CH2, "output/diagnostics/ch2_phase2a1_axis/axis_20261024_131641")

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
})
source(file.path(CH2, "R/ch2_report_helpers.R"))
source(file.path(DDM, "R/colors_manuscript.R"))

dir.create(EXPORT, recursive = TRUE, showWarnings = FALSE)
need <- c(PRIOR, MODELS, DIAG, PUPIL, WAVE, M1DATA, REDLOG, AXIS,
          file.path(PF, "LC Aging Subject Data master spreadsheet - demographics.csv"),
          file.path(PF, "bap_beh_subjxtaskdata_v2.csv"),
          file.path(PF, "bap_beh_trialdata_v2.csv"))
miss <- need[!file.exists(need) & !dir.exists(need)]
if (length(miss)) stop("missing: ", paste(miss, collapse = " | "), call. = FALSE)

log_line <- function(...) cat(format(Sys.time(), "%H:%M:%S"), paste0(...), "\n")

coef_from_fit <- function(fit) {
  sm <- summary(fit)$coefficients
  cn <- colnames(sm)
  est <- sm[, "Estimate"]
  se <- sm[, "Std. Error"]
  if ("z value" %in% cn) {
    stat <- sm[, "z value"]
    df <- rep(NA_real_, length(est))
    p <- sm[, "Pr(>|z|)"]
  } else {
    stat <- sm[, "t value"]
    df <- sm[, "df"]
    p <- sm[, "Pr(>|t|)"]
  }
  zcrit <- stats::qnorm(0.975)
  data.table(
    term = rownames(sm),
    estimate = as.numeric(est),
    SE = as.numeric(se),
    statistic = as.numeric(stat),
    df = as.numeric(df),
    p = as.numeric(p),
    ci_low = as.numeric(est) - zcrit * as.numeric(se),
    ci_high = as.numeric(est) + zcrit * as.numeric(se)
  )
}

ids <- c("M1", "M2", "M3", "M4a", "M4b", "M5", "S1", "S2")
fits <- lapply(ids, function(id) readRDS(file.path(MODELS, paste0(id, "_fit.rds"))))
names(fits) <- ids
live <- rbindlist(lapply(ids, function(id) {
  coef_from_fit(fits[[id]])[, model := id]
}))

old <- fromJSON(file.path(PRIOR, "values.json"), simplifyVector = FALSE)
cmp <- rbindlist(lapply(ids, function(id) {
  keys <- old$models[[id]]$keys
  rbindlist(lapply(names(keys), function(tm) {
    saved <- keys[[tm]]
    hit <- live[model == id & term == tm]
    num <- function(a, b) {
      bnum <- suppressWarnings(as.numeric(b))
      if (nrow(hit) != 1L || length(a) != 1L || !is.finite(a) || !is.finite(bnum)) NA_real_ else abs(a - bnum)
    }
    data.table(
      model = id, term = tm,
      abs_estimate = num(hit$estimate, saved$estimate),
      abs_se = num(hit$SE, saved$se),
      abs_stat = num(hit$statistic, saved$statistic),
      abs_df = if (!is.finite(hit$df) && (is.null(saved$df) || saved$df %in% c("NA", NA))) 0 else num(hit$df, saved$df),
      abs_p = num(hit$p, saved$p),
      abs_lo = num(hit$ci_low, saved$ci_low),
      abs_hi = num(hit$ci_high, saved$ci_high)
    )
  }))
}))
fwrite(cmp, file.path(EXPORT, "A1_export_vs_rds.csv"))
cmp_num <- cmp[, .(abs_estimate, abs_se, abs_stat, abs_df, abs_p, abs_lo, abs_hi)]
if (anyNA(cmp_num)) stop("A1 comparison has NA; a saved coefficient could not be checked", call. = FALSE)
worst <- max(as.matrix(cmp_num))
log_line("A1 max abs diff ", signif(worst, 6))
if (!is.finite(worst) || worst >= 1e-10) {
  writeLines(c(
    "STOP_A fatal",
    paste("max absolute difference", worst),
    "Saved values.json does not match summary() of the saved fits at 1e-10."
  ), file.path(EXPORT, "STOPPED_A.md"))
  stop("STOP_A", call. = FALSE)
}

expected <- data.table(
  model = c("M1", "M1", "M2", "M3", "M4a", "M4b", "M5"),
  term = c("effort_c", "x_c:effort_c", "effort_c", "effort_c", "x_c:b1_state", "x_c:tepr_state", "effort_c"),
  estimate = c(-0.09618, 0.05814, 0.13817, -0.27583, -0.04399, 0.01160, 0.13033),
  p = c(.00069, .0435, .216, .00044, .0494, .613, .0063)
)
exp_chk <- merge(expected, live, by = c("model", "term"))
exp_chk[, est_ok := abs(estimate.x - round(estimate.y, 5)) < 1.5e-5]
exp_chk[, p_ok := abs(p.x - round(p.y, 5)) < 1.5e-4 | abs(p.x - round(p.y, 3)) < 1.5e-3]
if (!all(exp_chk$est_ok)) stop("expected estimates do not match the fits", call. = FALSE)

# --- keys -------------------------------------------------------------------
iv_store <- list()
contract <- list()
add_key <- function(key, value, description, source, how) {
  if (is.null(value) || (is.atomic(value) && anyNA(value))) stop("refusing NA key ", key, call. = FALSE)
  iv_store[[key]] <<- value
  contract[[length(contract) + 1L]] <<- data.frame(
    key = key, description = description, source_file = source,
    source_column = how, stringsAsFactors = FALSE
  )
}

slug <- function(term) gsub("[^A-Za-z0-9]+", "_", term)
model_name <- c(
  M1 = "Behavior", M2 = "Grip-window pupil", M3 = "Task-evoked pupil",
  M4a = "Pre-probe pupil", M4b = "TEPR pupil", M5 = "Missingness",
  S1 = "AUC supplement", S2 = "Pupil-subsample behavior"
)
link_kind <- c(M1 = "z", M2 = "t", M3 = "t", M4a = "z", M4b = "z", M5 = "z", S1 = "z", S2 = "z")
term_word <- c(
  "(Intercept)" = "Intercept", catch = "Catch", x_c = "Offset", effort_c = "Effort",
  task_c = "Task", b1_state = "Pre-probe state", b1_trait = "Pre-probe trait",
  tepr_state = "TEPR state", tepr_trait = "TEPR trait",
  teprauc_state = "AUC state", teprauc_trait = "AUC trait"
)
label_term <- function(term) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  paste(vapply(parts, function(p) {
    if (p %in% names(term_word)) unname(term_word[[p]]) else p
  }, character(1)), collapse = " \u00d7 ")
}

for (id in ids) {
  meta <- fromJSON(file.path(MODELS, paste0(id, "_meta.json")))
  add_key(paste0(tolower(id), "_nobs"), meta$nobs, paste(id, "trials"), paste0(id, "_meta.json"), "nobs")
  add_key(paste0(tolower(id), "_n_participants"), meta$n_participants, paste(id, "participants"), paste0(id, "_meta.json"), "n_participants")
  add_key(paste0(tolower(id), "_n_adt"), unname(meta$n_participants_by_task[["ADT"]]), paste(id, "ADT participants"), paste0(id, "_meta.json"), "n_participants_by_task")
  add_key(paste0(tolower(id), "_n_vdt"), unname(meta$n_participants_by_task[["VDT"]]), paste(id, "VDT participants"), paste0(id, "_meta.json"), "n_participants_by_task")
  rows <- live[model == id]
  for (i in seq_len(nrow(rows))) {
    tm <- rows$term[i]
    sl <- paste0(tolower(id), "_", slug(tm))
    add_key(paste0(sl, "_est"), fmt_est(rows$estimate[i]), paste(id, tm, "estimate"), paste0(id, "_fit.rds"), tm)
    add_key(paste0(sl, "_se"), fmt_est(rows$SE[i]), paste(id, tm, "SE"), paste0(id, "_fit.rds"), tm)
    add_key(paste0(sl, "_ci"), fmt_ci(rows$ci_low[i], rows$ci_high[i]), paste(id, tm, "CI"), paste0(id, "_fit.rds"), tm)
    add_key(paste0(sl, "_p"), fmt_p(rows$p[i]), paste(id, tm, "p"), paste0(id, "_fit.rds"), tm)
    add_key(paste0(sl, "_stat"), fmt_num(rows$statistic[i], 2), paste(id, tm, "statistic"), paste0(id, "_fit.rds"), tm)
    if (is.finite(rows$df[i])) {
      add_key(paste0(sl, "_df"), fmt_num(rows$df[i], if (rows$df[i] >= 100) 0 else 1), paste(id, tm, "df"), paste0(id, "_fit.rds"), tm)
    }
  }
}
m1_meta <- fromJSON(file.path(MODELS, "M1_meta.json"))
add_key("lme4_version", m1_meta$lme4_version, "lme4 version that fitted the models", "M1_meta.json", "lme4_version")
add_key("lmerTest_version", m1_meta$lmerTest_version, "lmerTest version that fitted the models", "M1_meta.json", "lmerTest_version")
log_line(
  "fit lme4 ", m1_meta$lme4_version, " lmerTest ", m1_meta$lmerTest_version,
  " | session lme4 ", as.character(packageVersion("lme4")),
  " lmerTest ", as.character(packageVersion("lmerTest"))
)
add_key("run_id", RUN, "fit run id", "path", RUN)
n_no_tepr <- iv_store$m1_n_participants - iv_store$m3_n_participants
if (n_no_tepr != 17L) stop("n_no_tepr is not 17", call. = FALSE)
add_key("n_no_tepr", n_no_tepr, "behavioral participants with no task-evoked trials", "M1_meta.json minus M3_meta.json", "m1_n_participants - m3_n_participants")

m2var <- as.data.frame(VarCorr(fits$M2))
m2_vc <- m2var$vcov[m2var$grp == "subject" & m2var$var1 == "effort_c" & (is.na(m2var$var2) | m2var$var2 == "")]
if (length(m2_vc) != 1L) stop("M2 effort slope variance not found", call. = FALSE)
add_key("m2_effort_slope_variance", fmt_est(m2_vc), "M2 by-participant effort-slope variance", "M2_fit.rds", "VarCorr subject effort_c")

red <- fread(REDLOG)
s1row <- red[model == "S1" & step == "same_as_M4b_3_drop_tepr_state"]
if (nrow(s1row) != 1L) stop("S1 first-step row missing", call. = FALSE)
grad <- sub(".*max\\|grad\\| = ([0-9.]+).*", "\\1", s1row$conv_messages)
if (!grepl("^[0-9.]+$", grad)) stop("could not parse S1 gradient", call. = FALSE)
add_key("s1_max_abs_grad", grad, "S1 first-step max absolute gradient", "P2_reduction_log.csv", "conv_messages")

# --- participants -----------------------------------------------------------
m1 <- fread(M1DATA, select = c("subject", "task", "stim_level_index", "effort_c", "diff"))
who <- unique(m1[, .(subject, task)])
n_adt <- uniqueN(who[task == "ADT"]$subject)
n_vdt <- uniqueN(who[task == "VDT"]$subject)
both <- intersect(who[task == "ADT"]$subject, who[task == "VDT"]$subject)
one <- setdiff(unique(who$subject), both)
add_key("n_participants_total", uniqueN(who$subject), "participants in v2 trials", "analysis_data_m1.csv", "unique subject")
add_key("n_adt", n_adt, "auditory participants", "analysis_data_m1.csv", "task == ADT")
add_key("n_vdt", n_vdt, "visual participants", "analysis_data_m1.csv", "task == VDT")
add_key("n_both_tasks", length(both), "participants in both tasks", "analysis_data_m1.csv", "intersect ADT and VDT")
add_key("n_one_task", length(one), "participants in one task", "analysis_data_m1.csv", "setdiff")
log_line("one-task IDs ", paste(sort(one), collapse = ","))

# --- descriptives -----------------------------------------------------------
m1[, effort := fifelse(effort_c > 0, "High", "Low")]
m1[, offset_label := fcase(
  task == "ADT" & stim_level_index == 0L, "0 (catch)",
  task == "ADT" & stim_level_index == 1L, "8 Hz",
  task == "ADT" & stim_level_index == 2L, "16 Hz",
  task == "ADT" & stim_level_index == 3L, "32 Hz",
  task == "ADT" & stim_level_index == 4L, "64 Hz",
  task == "VDT" & stim_level_index == 0L, "0 (catch)",
  task == "VDT" & stim_level_index == 1L, "0.06",
  task == "VDT" & stim_level_index == 2L, "0.12",
  task == "VDT" & stim_level_index == 3L, "0.24",
  task == "VDT" & stim_level_index == 4L, "0.48"
)]
subj_p <- m1[, .(p = mean(diff), n_trials = .N), by = .(subject, task, stim_level_index, offset_label, effort)]
obs <- rbindlist(lapply(c("ADT", "VDT"), function(tk) {
  sub <- subj_p[task == tk]
  nlev <- uniqueN(sub[, .(stim_level_index, effort)])
  complete <- sub[, .N, by = subject][N == nlev, subject]
  comp <- sub[subject %in% complete]
  comp[, subj_mean := mean(p), by = subject]
  grand <- mean(comp$p)
  comp[, p_n := p - subj_mean + grand]
  se <- comp[, .(
    se = stats::sd(p_n) / sqrt(.N) * sqrt(nlev / (nlev - 1)),
    n_complete = .N
  ), by = .(stim_level_index, effort)]
  means <- sub[, .(p_bar = mean(p), n_subj = .N, n_trials = sum(n_trials)), by = .(stim_level_index, offset_label, effort)]
  means[se, on = c("stim_level_index", "effort")][, task := tk]
}))
fwrite(obs, file.path(EXPORT, "p_diff_by_cell.csv"))
wide <- dcast(obs, task + stim_level_index + offset_label ~ effort, value.var = "p_bar")
wide[, diff_prop := High - Low]
trial_cells <- m1[, .(p = mean(diff)), by = .(task, stim_level_index, effort)]
trial_wide <- dcast(trial_cells, task + stim_level_index ~ effort, value.var = "p")
trial_wide[, diff_prop := High - Low]
fwrite(wide, file.path(EXPORT, "p_diff_participant_mean_diff.csv"))
fwrite(trial_wide, file.path(EXPORT, "p_diff_trial_mean_diff.csv"))
add_key("n_cells_high_lower_trial", sum(trial_wide$diff_prop < 0),
        "cells where the trial-mean high-effort proportion is lower", "analysis_data_m1.csv", "trial mean")
add_key("n_cells_high_lower_participant", sum(wide$diff_prop < 0),
        "cells where the participant-mean high-effort proportion is lower", "analysis_data_m1.csv", "mean of participant proportions")
add_key("n_low_gt_high_pooled", sum(trial_wide$diff_prop < 0),
        "task-by-offset cells with a lower trial-pooled proportion under high effort", "analysis_data_m1.csv", "trial mean diff_prop < 0")
add_key("n_low_gt_high_pmean", sum(wide$diff_prop < 0),
        "task-by-offset cells with a lower participant-mean proportion under high effort", "analysis_data_m1.csv", "participant mean diff_prop < 0")
exc <- wide[diff_prop >= 0]
if (nrow(exc) != 1L || exc$task[1] != "VDT" || exc$stim_level_index[1] != 3L) {
  stop("participant-mean exception is not the visual 0.24 cell", call. = FALSE)
}
add_key("pmean_exception", "visual, level 3 (0.24)",
        "the cell where the participant-mean proportion is not lower under high effort", "analysis_data_m1.csv", "diff_prop >= 0")
for (tk in c("ADT", "VDT")) {
  top <- obs[task == tk & stim_level_index == 4]
  add_key(paste0("top_", tolower(tk), "_p"), fmt_prop(mean(m1[task == tk & stim_level_index == 4, diff]), 2),
          paste(tk, "top-offset P(different), trial mean"), "analysis_data_m1.csv", "mean(diff)")
  add_key(paste0("top_", tolower(tk), "_n"), nrow(m1[task == tk & stim_level_index == 4]),
          paste(tk, "top-offset trials"), "analysis_data_m1.csv", "nrow")
  add_key(paste0("catch_", tolower(tk), "_p"), fmt_prop(mean(m1[task == tk & stim_level_index == 0, diff]), 3),
          paste(tk, "catch P(different)"), "analysis_data_m1.csv", "mean(diff)")
  dprop <- trial_wide[task == tk & stim_level_index == 4, diff_prop]
  dprop_digits <- if (abs(dprop) > 0 && abs(dprop) < 0.01) 3L else 2L
  add_key(paste0("level4_", tolower(tk), "_diff_prop"), fmt_prop(dprop, dprop_digits),
          paste(tk, "top-offset high minus low, trial-mean proportion"), "analysis_data_m1.csv", "diff_prop trial mean")
  dpart <- wide[task == tk & stim_level_index == 4, diff_prop]
  add_key(paste0("level4_", tolower(tk), "_diff_prop_participant"), fmt_est(dpart),
          paste(tk, "top-offset high minus low, participant-mean proportion"), "analysis_data_m1.csv", "diff_prop participant mean")
}
# Model-implied probability difference at the top offset, fixed effects only.
pred_grid <- CJ(
  task = c("ADT", "VDT"),
  effort = c("Low", "High"),
  stim_level_index = 0:4
)
pred_grid[, task_c := fifelse(task == "VDT", 0.5, -0.5)]
pred_grid[, effort_c := fifelse(effort == "High", 0.5, -0.5)]
pred_grid[, catch := as.numeric(stim_level_index == 0L)]
pred_grid[, x_c := fifelse(stim_level_index == 0L, 0, (stim_level_index - 1) - 1.5)]
pred_grid[, subject := m1$subject[1]]
pred_grid[, p_hat := as.numeric(predict(fits$M1, newdata = pred_grid, re.form = NA, type = "response"))]
for (tk in c("ADT", "VDT")) {
  hi <- pred_grid[task == tk & effort == "High" & stim_level_index == 4, p_hat]
  lo <- pred_grid[task == tk & effort == "Low" & stim_level_index == 4, p_hat]
  add_key(paste0("level4_", tolower(tk), "_diff_probit_response"), fmt_est(hi - lo),
          paste(tk, "top-offset high minus low on the response scale, M1 fixed effects"),
          "M1_fit.rds", "predict re.form=NA")
}
fwrite(pred_grid, file.path(EXPORT, "m1_predictions.csv"))

# --- demographics -----------------------------------------------------------
demo <- fread(file.path(PF, "LC Aging Subject Data master spreadsheet - demographics.csv"), fill = TRUE)
setnames(demo, 2, "subject_id_raw")
demo[, subject := trimws(subject_id_raw)]
demo[, age := as.numeric(get("AGE AT BAP SESSION 1"))]
demo[, sex := trimws(as.character(SEX))]
demo[, edu := as.numeric(get("EDU (years)"))]
demo <- unique(demo[, .(subject, age, sex, edu)], by = "subject")
neuro_path <- file.path(CH2, "data/raw/behavioral", "LC Aging Subject Data master spreadsheet - neuropsych.csv")
neuro <- fread(neuro_path, skip = 1)
moca_col <- grep("^MoCA \\(phone screen", names(neuro), value = TRUE)
if (length(moca_col) != 1L) stop("T-MoCA column not found in the neuropsych file", call. = FALSE)
neuro[, subject := trimws(get("SUBJECT NUMBER"))]
neuro[, tmoca := as.numeric(get(moca_col))]
neuro <- unique(neuro[, .(subject, tmoca)], by = "subject")
demo <- merge(demo, neuro, by = "subject", all.x = TRUE)
summ_people <- function(ids, label) {
  dd <- demo[subject %in% ids]
  ages <- dd$age[is.finite(dd$age)]
  edus <- dd$edu[is.finite(dd$edu)]
  tmocas <- dd$tmoca[is.finite(dd$tmoca)]
  nf <- sum(dd$sex == "Female", na.rm = TRUE)
  data.table(
    Group = label,
    N = length(ids),
    `Age M` = fmt_num(mean(ages), 1),
    `Age SD` = fmt_num(stats::sd(ages), 1),
    `Age range` = paste0(fmt_num(min(ages), 0), "\u2013", fmt_num(max(ages), 0)),
    `Female n` = nf,
    `Female %` = fmt_num(100 * nf / length(ids), 1),
    `Education M` = fmt_num(mean(edus), 1),
    `Education SD` = fmt_num(stats::sd(edus), 1),
    `Education n` = length(edus),
    `T-MoCA M` = fmt_num(mean(tmocas), 1),
    `T-MoCA SD` = fmt_num(stats::sd(tmocas), 1),
    `T-MoCA n` = length(tmocas)
  )
}
adt_ids <- who[task == "ADT", subject]
vdt_ids <- who[task == "VDT", subject]
pupil <- fread(PUPIL)
for (g in c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final")) {
  pupil[, (g) := as.character(get(g)) %in% c("TRUE", "true", "True")]
}
tepr_ids <- unique(pupil[g_tepr_final == TRUE, subject_id])
demo_tab <- rbind(
  summ_people(unique(who$subject), "All behavioral"),
  summ_people(adt_ids, "Auditory"),
  summ_people(vdt_ids, "Visual"),
  summ_people(tepr_ids, "TEPR sample")
)
fwrite(demo_tab, file.path(EXPORT, "tbl_demographics.csv"))
all_ages <- demo[subject %in% unique(who$subject) & is.finite(age), age]
add_key("age_m_all", fmt_num(mean(all_ages), 1), "age mean, all behavioral participants", "demographics.csv", "AGE AT BAP SESSION 1")
add_key("age_sd_all", fmt_num(stats::sd(all_ages), 1), "age SD, all behavioral participants", "demographics.csv", "AGE AT BAP SESSION 1")
nf_all <- demo[subject %in% unique(who$subject) & sex == "Female", .N]
add_key("n_female_all", nf_all, "n female, all behavioral participants", "demographics.csv", "SEX")
add_key("pct_female_all", fmt_num(100 * nf_all / uniqueN(who$subject), 1), "percent female", "demographics.csv", "SEX")
demo_note <- data.table(
  item = c("age_M", "age_SD", "pct_female"),
  expected = c(69.5, 6.4, 50),
  observed = c(mean(all_ages), stats::sd(all_ages), 100 * nf_all / uniqueN(who$subject))
)
demo_note[, difference := observed - expected]
fwrite(demo_note, file.path(EXPORT, "A3_demographics_vs_preprint.csv"))

# --- pupil attrition --------------------------------------------------------
pupil[, has_file := !is.na(source_file) & nzchar(source_file)]
attr_rows <- rbindlist(lapply(c("ADT", "VDT"), function(tk) {
  sub <- pupil[task == tk]
  people <- unique(sub$subject_id)
  with_file <- unique(sub[has_file == TRUE, subject_id])
  usable <- sub[, .(n_usable = sum(g_tepr_3 %in% TRUE)), by = subject_id]
  under <- usable[subject_id %in% with_file & n_usable < 20, subject_id]
  data.table(
    task = tk,
    n_v2_participants = uniqueN(people),
    n_v2_trials = nrow(sub),
    n_no_file = length(setdiff(people, with_file)),
    n_file_under_20 = length(under),
    n_axis_refused = sum(sub$time_rule == "refused"),
    n_tepr_1 = sum(sub$g_tepr_1 %in% TRUE),
    n_tepr_2 = sum(sub$g_tepr_2 %in% TRUE),
    n_tepr_3 = sum(sub$g_tepr_3 %in% TRUE),
    n_tepr_final = sum(sub$g_tepr_final %in% TRUE),
    n_tepr_final_participants = uniqueN(sub[g_tepr_final %in% TRUE, subject_id])
  )
}))
fwrite(attr_rows, file.path(EXPORT, "A3_pupil_attrition.csv"))
add_key("n_axis_refused", sum(attr_rows$n_axis_refused), "trials refused by the axis check", "ch2_pupil_trials_r2.csv", "time_rule == refused")
for (i in seq_len(nrow(attr_rows))) {
  tk <- tolower(attr_rows$task[i])
  add_key(paste0("n_no_file_", tk), attr_rows$n_no_file[i], paste(tk, "participants with no pupil file"), "ch2_pupil_trials_r2.csv", "source_file empty")
  add_key(paste0("n_file_under_20_", tk), attr_rows$n_file_under_20[i], paste(tk, "participants with a file and fewer than 20 gated trials"), "ch2_pupil_trials_r2.csv", "g_tepr_3 count < 20")
}

flow <- rbindlist(lapply(c("ADT", "VDT"), function(tk) {
  sub <- pupil[task == tk]
  stage <- function(name, rows) data.table(
    Task = tk, Stage = name,
    Participants = uniqueN(rows$subject_id),
    Trials = nrow(rows)
  )
  rbind(
    stage("v2 trials", sub),
    stage("Pupil file present", sub[has_file == TRUE]),
    stage("Axis accepted", sub[has_file == TRUE & time_rule != "refused"]),
    stage("B1 gate", sub[g_tepr_2 %in% TRUE]),
    stage("Window gate", sub[g_tepr_3 %in% TRUE]),
    stage("At least 20 trials", sub[g_tepr_final %in% TRUE])
  )
}))
fwrite(flow, file.path(EXPORT, "tbl_pupil_flow.csv"))

# --- axis evidence ----------------------------------------------------------
t1 <- fread(file.path(AXIS, "T1_by_generation.csv"))
t1d <- t1[generation == "dec21" & subset == "all"]
t2 <- fread(file.path(AXIS, "T2c_implied_dt.csv"))
t2g <- t2[generation == "dec21" & phase == "grip_gauge"]
t3 <- fread(file.path(AXIS, "T3_by_pair.csv"))
axis_tab <- data.table(
  Test = c("T1 within-trial step", "T2 phase labels", "T3 December 21 vs 25"),
  n = c(t1d$n_trials, t2g$n_runs, nrow(t3)),
  `Implied step` = c(
    sprintf("%.4f", t1d$median_d_p50),
    sprintf("%.4f", t2g$implied_dt_median),
    sprintf("%.4f", median(t3$b_median))
  ),
  Note = c(
    paste0("fraction of trials with median step 0.004 = ", t1d$frac_trials_median_near_004),
    paste0("squeeze-gauge duration ", t2g$designed_duration, " s; median length ", t2g$length_median, " samples"),
    paste0("median index scale b = ", sprintf("%.3f", median(t3$b_median)), "; median r at b = 1 = ", sprintf("%.3f", median(t3$r_at_b_1.00_median)))
  )
)
fwrite(axis_tab, file.path(EXPORT, "tbl_axis.csv"))

# --- coefficient and reduction tables ---------------------------------------
stat_cell <- function(id, stat, df) {
  if (link_kind[[id]] == "t" && is.finite(df)) {
    paste0("t(", fmt_num(df, if (df >= 100) 0 else 1), ") = ", fmt_num(stat, 2))
  } else {
    paste0("z = ", fmt_num(stat, 2))
  }
}
coef_out <- rbindlist(lapply(seq_len(nrow(live)), function(i) {
  id <- live$model[i]
  p_str <- fmt_p(live$p[i])
  data.table(
    Model = unname(model_name[[id]]),
    ID = id,
    Accepted = isTRUE(fromJSON(file.path(MODELS, paste0(id, "_meta.json")))$accepted),
    Term = label_term(live$term[i]),
    Estimate = fmt_est(live$estimate[i]),
    SE = fmt_est(live$SE[i]),
    Statistic = stat_cell(id, live$statistic[i], live$df[i]),
    `95% CI` = fmt_ci(live$ci_low[i], live$ci_high[i]),
    p = p_str,
    p_tab = sub("^= ", "", p_str)
  )
}))
fwrite(coef_out, file.path(EXPORT, "coefficients.csv"))
key_terms <- list(
  M1 = c("effort_c", "catch:effort_c", "x_c:effort_c"),
  M2 = "effort_c",
  M3 = c("effort_c", "x_c", "catch"),
  M4a = c("x_c:b1_state", "b1_state", "catch:b1_state"),
  M4b = c("x_c:tepr_state", "tepr_state", "catch:tepr_state"),
  M5 = "effort_c"
)
key_tab <- rbindlist(lapply(names(key_terms), function(id) {
  coef_out[ID == id & Term %in% vapply(key_terms[[id]], label_term, character(1))]
}))
# Preserve key-term order
key_tab <- rbindlist(lapply(names(key_terms), function(id) {
  rbindlist(lapply(key_terms[[id]], function(tm) coef_out[ID == id & Term == label_term(tm)]))
}))
fwrite(key_tab, file.path(EXPORT, "tbl_key_terms.csv"))
fwrite(coef_out[ID == "M5"], file.path(EXPORT, "tbl_m5.csv"))

re_only <- function(formula) {
  hits <- gregexpr("\\([^()]*\\|[^()]*\\)", formula, perl = TRUE)
  paste(regmatches(formula, hits)[[1]], collapse = " + ")
}
short_msg <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  if (!nzchar(x)) return("none")
  if (nchar(x) > 140) paste0(substr(x, 1, 137), "...") else x
}
red_show <- red[model %in% c("M1", "M2", "M3", "M4a", "M4b", "M5", "S1")]
red_tab <- red_show[, .(
  Model = model,
  Step = step,
  `Random effects` = vapply(formula, re_only, character(1)),
  Singular = singular,
  Convergence = vapply(conv_messages, short_msg, character(1)),
  Accepted = accepted
)]
fwrite(red_tab, file.path(EXPORT, "tbl_reduction.csv"))

# --- behavior figure --------------------------------------------------------
fig_dt <- merge(
  obs,
  pred_grid[, .(task, effort, stim_level_index, p_hat)],
  by = c("task", "effort", "stim_level_index")
)
fig_dt[, x_lab := factor(offset_label, levels = unique(offset_label))]
fig_dt[, task_label := fifelse(task == "ADT", "Auditory", "Visual")]
fig_dt[, effort := factor(effort, levels = c("Low", "High"))]
# Same effort colors and theme as Chapter 3 (R/colors_manuscript.R; theme_ms in F_figures.R).
theme_ms <- function() {
  theme_classic(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}
p_beh <- ggplot(fig_dt, aes(x = x_lab, y = p_bar, color = effort, group = effort)) +
  geom_line(aes(y = p_hat), linewidth = 0.6) +
  geom_errorbar(aes(ymin = p_bar - se, ymax = p_bar + se), width = 0.15, linewidth = 0.3) +
  geom_point(size = 2) +
  facet_wrap(~ task_label, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_color_manual(values = effort_colors, name = "Effort") +
  labs(x = "Offset (log\u2082 spacing; catch at the left)", y = "P(\"different\")") +
  theme_ms()
ggsave(file.path(EXPORT, "fig_p_diff.png"), p_beh, width = 7.2, height = 3.6, dpi = 300)

# --- pupil time course ------------------------------------------------------
# Same display as Chapter 3's waveform (F_figures.R): mean of the
# within-participant means, Morey SE with effort repeated inside task,
# grip band, task-evoked band, probe line, response-cue line.
# Values stay on the pre-squeeze baseline already stored in the waveforms.
# They are not re-anchored to zero at squeeze onset.
tepr_pairs <- unique(pupil[g_tepr_final %in% TRUE, .(subject_id, task)])
wave <- fread(WAVE)
wave <- wave[t >= -0.5 & t <= 8]
wave <- tepr_pairs[wave, on = c("subject_id", "task"), nomatch = NULL]
stopifnot(all(wave$effort %in% c("Low", "High")))
wave[, t_rel := round(t * 50) / 50]
subj_wave <- wave[, .(
  pupil = mean(mean_b0, na.rm = TRUE)
), by = .(subject_id, task, effort, t_rel)]
subj_wave <- subj_wave[is.finite(pupil)]
source(file.path(CH2, "R/tepr_waveform_helpers.R"))
suppressPackageStartupMessages(library(dplyr))
pre_df <- as.data.frame(subj_wave)
pre_df$mean_pupil_full <- pre_df$pupil
win_df <- winsorize_subject_waveforms(pre_df, value_col = "mean_pupil_full")
stopifnot(
  nrow(win_df) == nrow(pre_df),
  identical(as.character(win_df$subject_id), as.character(pre_df$subject_id)),
  identical(as.character(win_df$task), as.character(pre_df$task)),
  identical(as.character(win_df$effort), as.character(pre_df$effort)),
  all(abs(win_df$t_rel - pre_df$t_rel) < 1e-8)
)
delta <- win_df$mean_pupil_full - pre_df$pupil
win_sum <- data.table(
  task = pre_df$task,
  changed = abs(delta) > 1e-12,
  abs_change = abs(delta)
)[, .(
  n_values = .N,
  n_changed = sum(changed),
  max_abs_change = if (any(changed)) max(abs_change[changed]) else 0
), by = task]
dir.create(file.path(CH2, "output/diagnostics/ch2_phase5b"), recursive = TRUE, showWarnings = FALSE)
fwrite(win_sum, file.path(CH2, "output/diagnostics/ch2_phase5b/winsor_by_task.csv"))
subj_wave[, pupil := win_df$mean_pupil_full]
means <- subj_wave[, .(
  mean_pupil = mean(pupil),
  n_subjects = uniqueN(subject_id)
), by = .(task, effort, t_rel)]
wide <- dcast(subj_wave, subject_id + task + t_rel ~ effort, value.var = "pupil")
stopifnot(all(c("Low", "High") %in% names(wide)))
se_tbl <- wide[is.finite(Low) & is.finite(High), {
  subj_m <- (Low + High) / 2
  grand <- mean(subj_m)
  low_c <- Low - subj_m + grand
  high_c <- High - subj_m + grand
  n <- .N
  morey <- sqrt(2)
  if (n < 2L) stop("Morey SE needs two participants", call. = FALSE)
  .(
    se_Low = stats::sd(low_c) / sqrt(n) * morey,
    se_High = stats::sd(high_c) / sqrt(n) * morey,
    n_both = n
  )
}, by = .(task, t_rel)]
se_long <- melt(
  se_tbl,
  id.vars = c("task", "t_rel", "n_both"),
  measure.vars = c("se_Low", "se_High"),
  variable.name = "effort",
  value.name = "se"
)
se_long[, effort := fifelse(effort == "se_Low", "Low", "High")]
plot_tepr <- merge(means, se_long, by = c("task", "effort", "t_rel"), all.x = TRUE)
stopifnot(!anyNA(plot_tepr$se))
setorder(plot_tepr, task, effort, t_rel)
plot_tepr[, effort := factor(effort, levels = c("Low", "High"))]
plot_tepr[, task_label := factor(
  fifelse(task == "ADT", "Auditory", "Visual"),
  levels = c("Auditory", "Visual")
)]
p_wave <- ggplot(plot_tepr, aes(x = t_rel, y = mean_pupil, color = effort, fill = effort)) +
  annotate("rect", xmin = 1.0, xmax = 4.35, ymin = -Inf, ymax = Inf, fill = "#E5E7EB", alpha = 0.85) +
  annotate("rect", xmin = 4.65, xmax = 7.65, ymin = -Inf, ymax = Inf, fill = "#DBEAFE", alpha = 0.55) +
  annotate("rect", xmin = 3.85, xmax = 4.35, ymin = -Inf, ymax = Inf, fill = "#F6C945", alpha = 0.45) +
  geom_vline(xintercept = c(0, 3.75, 4.35, 4.70), linewidth = 0.35, color = "#111827") +
  geom_ribbon(aes(ymin = mean_pupil - se, ymax = mean_pupil + se), alpha = 0.22, color = NA) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ task_label, ncol = 1) +
  scale_color_manual(values = effort_colors, name = "Effort") +
  scale_fill_manual(values = effort_colors, name = "Effort") +
  scale_x_continuous(breaks = seq(0, 8, by = 1), limits = c(-0.5, 8)) +
  labs(
    x = "Time from squeeze onset (s)",
    y = "Pupil change from pre-squeeze baseline (a.u.)"
  ) +
  theme_ms() +
  theme(plot.margin = ggplot2::margin(6, 12, 8, 12))
ggsave(file.path(EXPORT, "fig_pupil_timecourse.png"), p_wave, width = 8.2, height = 7.5, dpi = 300)

n_wave <- pupil[g_tepr_final %in% TRUE, .(
  n_trials = .N,
  n_participants = uniqueN(subject_id)
), by = .(task, effort)]
n_wave <- merge(
  n_wave,
  subj_wave[, .(n_waveform_subjects = uniqueN(subject_id)), by = .(task, effort)],
  by = c("task", "effort")
)
fwrite(n_wave, file.path(EXPORT, "B3_timecourse_n.csv"))

allfit <- rbindlist(lapply(
  list.files(DIAG, pattern = "^allFit_M.*\\.csv$", full.names = TRUE),
  fread
))
if (!nrow(allfit)) stop("allFit tables missing", call. = FALSE)
add_key("allfit_optimizers", paste(sort(unique(allfit$optimizer)), collapse = ", "),
        "optimizers in the restricted allFit", "allFit_M1.csv", "optimizer")
add_key("allfit_max_abs_dev", fmt_num(max(allfit$max_abs_dev_key, na.rm = TRUE), 5),
        "largest absolute key-term deviation in allFit", "allFit_*.csv", "max_abs_dev_key")

# --- write values and contract ----------------------------------------------
write_json(list(run = RUN, iv = iv_store), file.path(EXPORT, "values.json"),
           pretty = TRUE, auto_unbox = TRUE, na = "null")
fwrite(rbindlist(lapply(contract, as.data.table)), file.path(EXPORT, "key_contract.csv"))
log_line("WROTE ", EXPORT)
log_line("keys ", length(iv_store))
