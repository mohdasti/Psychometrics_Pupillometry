# Phase 9 pupil figures.
# Does not refit. Reads the saved M3, M4a, and M4b fits and the same
# waveform rows as scripts/phase3/01_export.R. Writes only:
#   fig_pupil_dynamics.png, fig_pupil_diff.csv, fig_tepr_by_offset.csv,
#   fig_pupil_coupling.png, fig_pupil_coupling.csv
# Run: Rscript scripts/phase3/03_phase9_figures.R

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
RUN <- "ch2fit_20261024_193248"
MODELS <- file.path(CH2, "output/models/ch2_phase2b", RUN)
EXPORT <- file.path(CH2, "07_manuscript/chapter2/export", RUN)
PUPIL <- file.path(CH2, "data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_pupil_trials_r2.csv")
WAVE <- file.path(CH2, "data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_waveforms_subject_condition_r2.csv")
DIAG9 <- file.path(CH2, "output/diagnostics/ch2_phase9")
EXPORT_SCRIPT <- file.path(CH2, "scripts/phase3/01_export.R")

new_files <- c(
  "fig_pupil_dynamics.png",
  "fig_pupil_diff.csv",
  "fig_tepr_by_offset.csv",
  "fig_pupil_coupling.png",
  "fig_pupil_coupling.csv"
)

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(patchwork)
})
source(file.path(CH2, "R/ch2_report_helpers.R"))
source(file.path(CH2, "R/colors_manuscript.R"))
dir.create(DIAG9, recursive = TRUE, showWarnings = FALSE)

log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), " ", paste0(...), "\n")
  cat(msg)
  cat(msg, file = file.path(DIAG9, "compute.log"), append = TRUE)
}
cat("", file = file.path(DIAG9, "compute.log"))

protected <- setdiff(list.files(EXPORT), new_files)
protected_md5 <- tools::md5sum(file.path(EXPORT, protected))
on.exit({
  now <- tools::md5sum(file.path(EXPORT, protected))
  if (!identical(unname(protected_md5), unname(now))) {
    changed <- protected[unname(protected_md5) != unname(now)]
    stop("existing export file changed: ", paste(changed, collapse = ", "), call. = FALSE)
  }
}, add = TRUE)

theme_ms <- function() {
  theme_classic(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Same waveform block as 01_export.R, without the diagnostic fwrite or the
# ggsave that would overwrite fig_pupil_timecourse.png.
export_lines <- readLines(EXPORT_SCRIPT, warn = FALSE)
i0 <- grep("pupil time course", export_lines)
i1 <- grep("fig_pupil_timecourse.png", export_lines)
if (length(i0) != 1L || length(i1) != 1L || i1 <= i0) {
  stop("could not locate the fig_pupil_timecourse block", call. = FALSE)
}
block <- export_lines[(i0 + 1L):(i1 - 1L)]
block <- block[!grepl("ch2_phase5b|winsor_by_task", block)]
if (any(grepl("ggsave|write_json|values\\.json", block))) {
  stop("waveform block extractor included a writer", call. = FALSE)
}

pupil <- fread(PUPIL)
for (g in c("g_tepr_1", "g_tepr_2", "g_tepr_3", "g_tepr_final")) {
  pupil[, (g) := as.character(get(g)) %in% c("TRUE", "true", "True")]
}

wave_env <- function() {
  e <- new.env(parent = environment())
  eval(parse(text = block), envir = e)
  e
}
log_line("evaluating the original waveform block")
env_a <- wave_env()
env_b <- wave_env()
if (!identical(env_a$plot_tepr$t_rel, env_b$plot_tepr$t_rel) ||
    !identical(as.character(env_a$plot_tepr$task), as.character(env_b$plot_tepr$task)) ||
    !identical(as.character(env_a$plot_tepr$effort), as.character(env_b$plot_tepr$effort))) {
  stop("B1: the two original-path runs are not aligned", call. = FALSE)
}
b1_mean <- max(abs(env_a$plot_tepr$mean_pupil - env_b$plot_tepr$mean_pupil))
b1_se <- max(abs(env_a$plot_tepr$se - env_b$plot_tepr$se))
log_line("B1 max abs mean ", format(b1_mean, scientific = TRUE), " se ", format(b1_se, scientific = TRUE))
if (!is.finite(b1_mean) || !is.finite(b1_se) || b1_mean >= 1e-10 || b1_se >= 1e-10) {
  stop("B1 gate failed", call. = FALSE)
}

plot_tepr <- env_a$plot_tepr
subj_wave <- env_a$subj_wave
stopifnot(isTRUE(all.equal(env_a$plot_tepr$mean_pupil, plot_tepr$mean_pupil)))

t_step <- unique(round(diff(sort(unique(plot_tepr$t_rel))), 6))
log_line(
  "time grid step ", paste(t_step, collapse = ","),
  " range ", min(plot_tepr$t_rel), " to ", max(plot_tepr$t_rel),
  " n time ", uniqueN(plot_tepr$t_rel)
)
n_both <- unique(plot_tepr[, .(task, n_both)])
log_line("participants with both physical effort levels: ", paste(paste(n_both$task, n_both$n_both, sep = "="), collapse = ", "))

# --- row B: high minus low --------------------------------------------------
wide <- dcast(subj_wave, subject_id + task + t_rel ~ effort, value.var = "pupil")
stopifnot(all(c("Low", "High") %in% names(wide)))
diff_long <- wide[is.finite(Low) & is.finite(High), .(
  subject_id, task, t_rel, d = High - Low
)]
n_task <- diff_long[, .(n_participants = uniqueN(subject_id)), by = task]
n_time <- diff_long[, .(n = uniqueN(subject_id)), by = .(task, t_rel)]
n_span <- n_time[, .(n_min = min(n), n_max = max(n)), by = task]
log_line("row B n by task: ", paste(paste(n_task$task, n_task$n_participants, sep = "="), collapse = ", "))
log_line("row B n across time, min/max: ", paste(paste(n_span$task, n_span$n_min, n_span$n_max, sep = "="), collapse = ", "))

diff_sum <- diff_long[, {
  n <- .N
  m <- mean(d)
  s <- stats::sd(d)
  crit <- stats::qt(0.975, n - 1)
  se <- s / sqrt(n)
  .(n = n, mean_diff = m, sd_diff = s, lo = m - crit * se, hi = m + crit * se)
}, by = .(task, t_rel)]
setorder(diff_sum, task, t_rel)
diff_csv <- diff_sum[, .(
  task = fifelse(task == "ADT", "auditory", "visual"),
  time_s = t_rel,
  n, mean_diff, sd_diff, lo, hi
)]
stopifnot(diff_csv$task %in% c("auditory", "visual"))
fwrite(diff_csv, file.path(EXPORT, "fig_pupil_diff.csv"))

# --- row C: TEPR by offset --------------------------------------------------
fits <- list(
  M3 = readRDS(file.path(MODELS, "M3_fit.rds")),
  M4a = readRDS(file.path(MODELS, "M4a_fit.rds")),
  M4b = readRDS(file.path(MODELS, "M4b_fit.rds"))
)
iv <- jsonlite::fromJSON(file.path(EXPORT, "values.json"), simplifyVector = FALSE)$iv
mf <- as.data.table(model.frame(fits$M3))
log_line("M3 model.frame columns: ", paste(names(mf), collapse = ", "))
need_cols <- c("tepr_mean_465_765", "effort_c", "task_c", "catch", "x_c", "subject")
if (!all(need_cols %in% names(mf))) {
  stop("M3 model.frame lacks ", paste(setdiff(need_cols, names(mf)), collapse = ", "), call. = FALSE)
}
if (nrow(mf) != as.numeric(iv$m3_nobs) || uniqueN(mf$subject) != as.numeric(iv$m3_n_participants)) {
  stop(
    "B3 gate failed: nrow ", nrow(mf), " subjects ", uniqueN(mf$subject),
    " expected ", iv$m3_nobs, " and ", iv$m3_n_participants,
    call. = FALSE
  )
}
log_line("B3 gate pass: nrow ", nrow(mf), " participants ", uniqueN(mf$subject))

near <- function(a, b) abs(a - b) < 1e-8
mf[, position := fcase(
  near(catch, 1) & near(x_c, 0), "catch",
  near(catch, 0) & near(x_c, -1.5), "1",
  near(catch, 0) & near(x_c, -0.5), "2",
  near(catch, 0) & near(x_c, 0.5), "3",
  near(catch, 0) & near(x_c, 1.5), "4"
)]
mf[, task := fcase(near(task_c, -0.5), "auditory", near(task_c, 0.5), "visual")]
mf[, effort := fcase(near(effort_c, -0.5), "Low", near(effort_c, 0.5), "High")]
if (anyNA(mf$position) || anyNA(mf$task) || anyNA(mf$effort)) {
  stop("a trial did not match a task, physical effort, or offset position", call. = FALSE)
}

cell <- mf[, .(tepr = mean(tepr_mean_465_765)), by = .(subject, task, effort, position)]
pos_levels <- c("catch", "1", "2", "3", "4")
tepr_rows <- rbindlist(lapply(c("auditory", "visual"), function(tk) {
  sub <- cell[task == tk]
  nlev <- uniqueN(sub[, .(position, effort)])
  n_by_subj <- sub[, .N, by = subject]
  complete <- n_by_subj[N == nlev, subject]
  n_all <- uniqueN(sub$subject)
  n_dropped <- n_all - length(complete)
  comp <- sub[subject %in% complete]
  comp[, subj_mean := mean(tepr), by = subject]
  grand <- mean(comp$tepr)
  comp[, tepr_n := tepr - subj_mean + grand]
  se <- comp[, .(
    se_within = stats::sd(tepr_n) / sqrt(.N) * sqrt(nlev / (nlev - 1)),
    n_in_se = .N
  ), by = .(position, effort)]
  means <- sub[, .(mean = mean(tepr), n = .N), by = .(position, effort)]
  out <- means[se, on = c("position", "effort")]
  out[, task := tk]
  attr(out, "info") <- NULL
  out[, n_complete := length(complete)]
  out[, n_dropped := n_dropped]
  out[, n_all := n_all]
  out[, nlev := nlev]
  out
}))
b3_preview <- unique(tepr_rows[, .(task, n_complete, n_dropped, n_all, nlev)])
log_line("B3 complete participants:\n", paste(capture.output(print(b3_preview)), collapse = "\n"))
if (anyNA(tepr_rows$se_within)) stop("a cell has no within-participant SE", call. = FALSE)

pred_pos <- data.table(
  position = pos_levels,
  catch = c(1, 0, 0, 0, 0),
  x_c = c(0, -1.5, -0.5, 0.5, 1.5)
)
pred_grid <- CJ(task = c("auditory", "visual"), effort = c("Low", "High"), position = pos_levels)
pred_grid[pred_pos, on = "position", `:=`(catch = i.catch, x_c = i.x_c)]
pred_grid[, task_c := fifelse(task == "visual", 0.5, -0.5)]
pred_grid[, effort_c := fifelse(effort == "High", 0.5, -0.5)]
pred_grid[, subject := mf$subject[1]]
pred_grid[, pred := as.numeric(predict(fits$M3, newdata = as.data.frame(pred_grid), re.form = NA))]

tepr_csv <- merge(
  tepr_rows[, .(task, effort, position, n, mean, se_within)],
  pred_grid[, .(task, effort, position, pred)],
  by = c("task", "effort", "position"),
  sort = FALSE
)
tepr_csv[, position := factor(position, levels = pos_levels)]
tepr_csv[, effort := factor(effort, levels = c("Low", "High"))]
setorder(tepr_csv, task, effort, position)
tepr_csv[, position := as.character(position)]
tepr_csv[, effort := as.character(effort)]
fwrite(tepr_csv, file.path(EXPORT, "fig_tepr_by_offset.csv"))
b3_info <- unique(tepr_rows[, .(task, n_complete, n_dropped, n_all, nlev)])
fwrite(b3_info, file.path(DIAG9, "b3_complete.csv"))

# --- coupling coefficients --------------------------------------------------
coef_from_fit <- function(fit) {
  sm <- summary(fit)$coefficients
  cn <- colnames(sm)
  est <- sm[, "Estimate"]
  se <- sm[, "Std. Error"]
  stat <- if ("z value" %in% cn) sm[, "z value"] else sm[, "t value"]
  p <- if ("Pr(>|z|)" %in% cn) sm[, "Pr(>|z|)"] else sm[, "Pr(>|t|)"]
  zcrit <- stats::qnorm(0.975)
  data.table(
    term = rownames(sm),
    estimate = as.numeric(est),
    se = as.numeric(se),
    statistic = as.numeric(stat),
    p = as.numeric(p),
    lo = as.numeric(est) - zcrit * as.numeric(se),
    hi = as.numeric(est) + zcrit * as.numeric(se)
  )
}
term_map <- data.table(
  model = c("M4a", "M4a", "M4a", "M4b", "M4b", "M4b"),
  term_fit = c("b1_state", "x_c:b1_state", "catch:b1_state", "tepr_state", "x_c:tepr_state", "catch:tepr_state"),
  label = c(
    "Rate of \"different\" responses",
    "Slope (offset \u00d7 state)",
    "Catch trials (catch \u00d7 state)",
    "Rate of \"different\" responses",
    "Slope (offset \u00d7 state)",
    "Catch trials (catch \u00d7 state)"
  ),
  which = c("rate", "slope", "catch", "rate", "slope", "catch")
)
live <- rbindlist(lapply(c("M4a", "M4b"), function(id) coef_from_fit(fits[[id]])[, model := id]))
coup <- merge(term_map, live, by.x = c("model", "term_fit"), by.y = c("model", "term"))
if (nrow(coup) != 6L) stop("C1: did not match six coupling terms", call. = FALSE)
log_line("C1 terms: ", paste(paste(coup$model, coup$term_fit, sep = ":"), collapse = ", "))

expected <- data.table(
  model = c("M4a", "M4a", "M4a", "M4b", "M4b", "M4b"),
  which = c("slope", "rate", "catch", "slope", "rate", "catch"),
  est_fmt = c("\u22120.04", "\u22120.03", "0.07", "0.01", "0.000", "\u22120.01"),
  ci_fmt = c(
    "[\u22120.09, 0.000]", "[\u22120.07, 0.02]", "[\u22120.05, 0.19]",
    "[\u22120.03, 0.06]", "[\u22120.05, 0.05]", "[\u22120.14, 0.12]"
  ),
  p_fmt = c(".049", ".209", ".257", ".613", ".987", ".853")
)
coup[, est_fmt := vapply(estimate, fmt_est, character(1))]
coup[, ci_fmt := mapply(fmt_ci, lo, hi)]
coup[, p_fmt := sub("^= ", "", vapply(p, fmt_p, character(1)))]
chk <- merge(coup[, .(model, which, est_fmt, ci_fmt, p_fmt)], expected, by = c("model", "which"), suffixes = c("", "_exp"))
bad <- chk[est_fmt != est_fmt_exp | ci_fmt != ci_fmt_exp | p_fmt != p_fmt_exp]
if (nrow(bad)) {
  print(bad)
  stop("C1 gate failed", call. = FALSE)
}
log_line("C1 gate pass")

coef_csv <- fread(file.path(EXPORT, "coefficients.csv"))
coef_csv[, Estimate := vapply(Estimate, function(x) fmt_est(parse_est(x)), character(1))]
# The chapter table is the gate. Also require the rounded cells to match coefficients.csv.
label_in_csv <- c(
  "M4a|b1_state" = "Pre-probe state",
  "M4a|x_c:b1_state" = "Offset \u00d7 Pre-probe state",
  "M4a|catch:b1_state" = "Catch \u00d7 Pre-probe state",
  "M4b|tepr_state" = "TEPR state",
  "M4b|x_c:tepr_state" = "Offset \u00d7 TEPR state",
  "M4b|catch:tepr_state" = "Catch \u00d7 TEPR state"
)
for (i in seq_len(nrow(coup))) {
  key <- paste(coup$model[i], coup$term_fit[i], sep = "|")
  hit <- coef_csv[ID == coup$model[i] & Term == unname(label_in_csv[[key]])]
  if (nrow(hit) != 1L || hit$Estimate != coup$est_fmt[i]) {
    stop("rounded estimate does not match coefficients.csv for ", key, call. = FALSE)
  }
}

coup_csv <- coup[order(match(paste(model, which), paste(term_map$model, term_map$which))),
  .(model, term_fit, label, estimate, se, lo, hi, p)]
fwrite(coup_csv, file.path(EXPORT, "fig_pupil_coupling.csv"))

# --- B5 intervals -----------------------------------------------------------
b5_one <- function(task_code) {
  d <- diff_sum[task == task_code][order(t_rel)]
  d[, excl := lo > 0 | hi < 0]
  d[, sgn := fifelse(mean_diff > 0, "positive", "negative")]
  runs <- list()
  i <- 1L
  n <- nrow(d)
  while (i <= n) {
    if (!isTRUE(d$excl[i])) {
      i <- i + 1L
      next
    }
    j <- i
    while (j + 1L <= n && isTRUE(d$excl[j + 1L]) && d$sgn[j + 1L] == d$sgn[i] &&
           (d$t_rel[j + 1L] - d$t_rel[j]) <= 0.020001) {
      j <- j + 1L
    }
    dur <- d$t_rel[j] - d$t_rel[i]
    if (dur >= 0.10 - 1e-9) {
      runs[[length(runs) + 1L]] <- data.table(
        task = fifelse(task_code == "ADT", "auditory", "visual"),
        start_s = d$t_rel[i], end_s = d$t_rel[j], duration_s = dur,
        sign = d$sgn[i], n_points = j - i + 1L
      )
    }
    i <- j + 1L
  }
  runs <- if (length(runs)) rbindlist(runs) else data.table(
    task = character(), start_s = numeric(), end_s = numeric(),
    duration_s = numeric(), sign = character(), n_points = integer()
  )
  grip <- d[t_rel >= 1 & t_rel <= 4.35]
  pre <- d[t_rel >= 3.85 & t_rel <= 4.35]
  post <- d[t_rel >= 4.65 & t_rel <= 7.65]
  grip_excl <- grip[excl == TRUE]
  data.table(
    task = fifelse(task_code == "ADT", "auditory", "visual"),
    earliest_grip_excl_s = if (nrow(grip_excl)) min(grip_excl$t_rel) else NA_real_,
    n_grip = nrow(grip),
    n_grip_excl_first_half = grip[t_rel < 2.675 & excl == TRUE, .N],
    n_grip_excl_second_half = grip[t_rel >= 2.675 & excl == TRUE, .N],
    pre_n = nrow(pre),
    pre_frac_excl = mean(pre$excl),
    pre_mean = mean(pre$mean_diff),
    post_n = nrow(post),
    post_frac_excl = mean(post$excl),
    post_mean = mean(post$mean_diff)
  )[, intervals := list(runs)]
}
b5 <- rbindlist(lapply(c("ADT", "VDT"), b5_one))
b5_intervals <- rbindlist(b5$intervals)
fwrite(b5_intervals, file.path(DIAG9, "b5_intervals.csv"))
b5_summary <- copy(b5)
b5_summary[, intervals := NULL]
fwrite(b5_summary, file.path(DIAG9, "b5_summary.csv"))
log_line("B5 summary written")
print(b5_summary)
print(b5_intervals)

# --- plots ------------------------------------------------------------------
add_windows <- function(p) {
  p +
    annotate("rect", xmin = 1.0, xmax = 4.35, ymin = -Inf, ymax = Inf, fill = "#E5E7EB", alpha = 0.85) +
    annotate("rect", xmin = 4.65, xmax = 7.65, ymin = -Inf, ymax = Inf, fill = "#DBEAFE", alpha = 0.55) +
    annotate("rect", xmin = 3.85, xmax = 4.35, ymin = -Inf, ymax = Inf, fill = "#F6C945", alpha = 0.45) +
    geom_vline(xintercept = c(0, 3.75, 4.35, 4.70), linewidth = 0.35, color = "#111827")
}
scale_x_wave <- function() {
  scale_x_continuous(breaks = seq(0, 8, by = 1), limits = c(-0.5, 8))
}
hide_strips <- function(p) {
  p + theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )
}

p_wave_data <- copy(plot_tepr)
stopifnot(max(abs(p_wave_data$mean_pupil - env_b$plot_tepr$mean_pupil)) < 1e-10)
stopifnot(max(abs(p_wave_data$se - env_b$plot_tepr$se)) < 1e-10)

row_tag <- function(letter, x) {
  data.frame(
    task_label = factor("Auditory", levels = c("Auditory", "Visual")),
    x = x,
    y = Inf,
    lab = letter
  )
}
pA <- ggplot(p_wave_data, aes(x = t_rel, y = mean_pupil, color = effort, fill = effort))
pA <- add_windows(pA) +
  geom_ribbon(aes(ymin = mean_pupil - se, ymax = mean_pupil + se), alpha = 0.22, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_text(
    data = row_tag("A", -0.42),
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4,
    hjust = 0,
    vjust = 1.15
  ) +
  facet_wrap(~ task_label, ncol = 2) +
  scale_color_manual(
    values = effort_colors,
    name = "Physical effort",
    guide = guide_legend(override.aes = list(linewidth = 0.8, alpha = 1))
  ) +
  scale_fill_manual(values = effort_colors, name = "Physical effort") +
  scale_x_wave() +
  labs(
    x = "Time from squeeze onset (s)",
    y = "Pupil size (a.u.)"
  ) +
  theme_ms() +
  theme(plot.margin = ggplot2::margin(4, 8, 2, 6))

diff_plot <- copy(diff_sum)
diff_plot[, task_label := factor(fifelse(task == "ADT", "Auditory", "Visual"), levels = c("Auditory", "Visual"))]
pB <- ggplot(diff_plot, aes(x = t_rel, y = mean_diff))
pB <- add_windows(pB) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#3A3A3A", alpha = 0.25, color = NA) +
  geom_line(color = "#3A3A3A", linewidth = 0.6) +
  geom_text(
    data = row_tag("B", -0.42),
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4,
    hjust = 0,
    vjust = 1.15
  ) +
  facet_wrap(~ task_label, ncol = 2) +
  scale_x_wave() +
  labs(x = "Time from squeeze onset (s)", y = "High \u2212 low (a.u.)") +
  theme_ms() +
  theme(plot.margin = ggplot2::margin(2, 8, 2, 6), legend.position = "none")
pB <- hide_strips(pB)

eff_x <- c(catch = 1, "1" = 2.5, "2" = 3.5, "3" = 4.5, "4" = 5.5)
tepr_plot <- copy(tepr_csv)
tepr_plot[, x := eff_x[position] + fifelse(effort == "Low", -0.1, 0.1)]
tepr_plot[, task_label := factor(fifelse(task == "auditory", "Auditory", "Visual"), levels = c("Auditory", "Visual"))]
tepr_plot[, effort := factor(effort, levels = c("Low", "High"))]
tepr_line <- tepr_plot[position != "catch"]
tepr_catch <- tepr_plot[position == "catch"]
pC <- ggplot(tepr_plot, aes(x = x, y = mean, color = effort, group = effort)) +
  geom_vline(xintercept = 1.75, linewidth = 0.2, color = "grey70") +
  geom_segment(
    data = tepr_catch,
    aes(x = x - 0.15, xend = x + 0.15, y = pred, yend = pred),
    linewidth = 0.6
  ) +
  geom_line(data = tepr_line, aes(y = pred), linewidth = 0.6) +
  geom_errorbar(aes(ymin = mean - se_within, ymax = mean + se_within), width = 0.15, linewidth = 0.3) +
  geom_point(size = 2) +
  geom_text(
    data = row_tag("C", 0.68),
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4,
    hjust = 0,
    vjust = 1.15
  ) +
  facet_wrap(~ task_label, ncol = 2) +
  scale_color_manual(values = effort_colors, name = "Physical effort", guide = "none") +
  scale_x_continuous(breaks = unname(eff_x), labels = c("Catch", "1", "2", "3", "4"), limits = c(0.6, 5.9)) +
  labs(x = "Offset level (1 = smallest)", y = "TEPR (a.u.)") +
  theme_ms() +
  theme(plot.margin = ggplot2::margin(2, 8, 2, 6), legend.position = "none")
pC <- hide_strips(pC)

fig4 <- pA / pB / pC +
  plot_layout(heights = c(1, 1, 0.9), guides = "collect") &
  theme(legend.position = "bottom")
ggsave(file.path(EXPORT, "fig_pupil_dynamics.png"), fig4, width = 7.2, height = 7.6, dpi = 300, bg = "white")

y_levels <- c(
  "Catch trials (catch \u00d7 state)",
  "Slope (offset \u00d7 state)",
  "Rate of \"different\" responses"
)
coup_plot <- copy(coup_csv)
coup_plot[, label := factor(label, levels = y_levels)]
coup_plot[, facet := factor(
  fifelse(model == "M4a", "Pre-probe pupil (M4a)", "TEPR (M4b)"),
  levels = c("Pre-probe pupil (M4a)", "TEPR (M4b)")
)]
x_pad <- max(abs(c(coup_plot$lo, coup_plot$hi))) * 1.12
fig5 <- ggplot(coup_plot, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.18, linewidth = 0.45, color = "#3A3A3A") +
  geom_point(size = 2.1, color = "#3A3A3A") +
  facet_wrap(~ facet, ncol = 2) +
  scale_x_continuous(limits = c(-x_pad, x_pad)) +
  labs(x = "Coefficient (probit per SD of trial-to-trial pupil size)", y = NULL) +
  theme_ms() +
  theme(plot.margin = ggplot2::margin(6, 12, 4, 8))
ggsave(file.path(EXPORT, "fig_pupil_coupling.png"), fig5, width = 7.2, height = 2.8, dpi = 300, bg = "white")

wrote <- file.path(EXPORT, new_files)
missing <- wrote[!file.exists(wrote)]
if (length(missing)) stop("missing new file: ", paste(missing, collapse = ", "), call. = FALSE)
extra <- setdiff(list.files(EXPORT), c(protected, new_files))
if (length(extra)) stop("unexpected new export file: ", paste(extra, collapse = ", "), call. = FALSE)
md5 <- tools::md5sum(wrote)
log_line("md5\n", paste(paste(basename(names(md5)), md5, sep = " "), collapse = "\n"))
writeLines(
  c(
    paste("b1_mean", b1_mean),
    paste("b1_se", b1_se),
    paste("time_step", paste(t_step, collapse = ",")),
    paste("time_min", min(plot_tepr$t_rel)),
    paste("time_max", max(plot_tepr$t_rel))
  ),
  file.path(DIAG9, "gates.txt")
)
log_line("phase 9 figures written")
