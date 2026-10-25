# Phase 3 export. Reads the locked Phase 2b fits. Does not refit them.
# Run: Rscript scripts/phase3/01_export_chapter_values.R

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
RUN <- "ch2fit_20261024_193248"
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2b", RUN)
MODELS <- file.path(CH2, "output/models/ch2_phase2b", RUN)
OUT <- file.path(CH2, "07_manuscript/chapter2/export", RUN)

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(ggplot2)
})

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(DIAG), dir.exists(MODELS))

fmt_num <- function(x, digits = 3) {
  if (length(x) != 1L || !is.finite(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_signed <- function(x, digits) {
  if (length(x) != 1L || !is.finite(x)) return("NA")
  s <- fmt_num(x, digits)
  if (startsWith(s, "-")) paste0("\u2212", substring(s, 2)) else s
}

fmt_est <- function(x) {
  digits <- if (is.finite(x) && abs(x) > 0 && abs(x) < 0.001) 4L else 3L
  fmt_signed(x, digits)
}

fmt_p <- function(p) {
  if (length(p) != 1L || !is.finite(p)) return("NA")
  if (p < .001) return("< .001")
  sprintf("= .%03d", as.integer(round(p * 1000)))
}

fmt_ci <- function(lo, hi) {
  paste0("[", fmt_est(lo), ", ", fmt_est(hi), "]")
}

fmt_df <- function(df) {
  if (length(df) != 1L || !is.finite(df)) return("")
  if (df >= 100) return(sprintf("%.0f", df))
  sprintf("%.1f", df)
}

term_row <- function(fit, term_name) {
  hit <- fit[term == term_name]
  if (nrow(hit) != 1L) stop("term not found: ", term_name, call. = FALSE)
  list(
    term = term_name,
    estimate = hit$estimate,
    se = hit$SE,
    statistic = hit$statistic,
    df = if (is.finite(hit$df)) hit$df else NA_real_,
    p = hit$p,
    ci_low = hit$ci_low,
    ci_high = hit$ci_high,
    est_fmt = fmt_est(hit$estimate),
    se_fmt = fmt_num(hit$SE, 3),
    stat_fmt = fmt_signed(hit$statistic, 2),
    df_fmt = fmt_df(hit$df),
    p_fmt = fmt_p(hit$p),
    ci_fmt = fmt_ci(hit$ci_low, hit$ci_high)
  )
}

load_model <- function(id) {
  meta_path <- file.path(MODELS, paste0(id, "_meta.json"))
  fix_path <- file.path(MODELS, paste0(id, "_fixef.csv"))
  var_path <- file.path(MODELS, paste0(id, "_ranef_var.csv"))
  stopifnot(file.exists(meta_path), file.exists(fix_path), file.exists(var_path))
  meta <- fromJSON(meta_path)
  fit <- fread(fix_path)
  var <- fread(var_path)
  list(meta = meta, fixef = fit, ranef = var)
}

pack_model <- function(id, key_terms) {
  obj <- load_model(id)
  keys <- lapply(key_terms, function(tm) term_row(obj$fixef, tm))
  names(keys) <- key_terms
  by_task_n <- obj$meta$nobs_by_task
  by_task_p <- obj$meta$n_participants_by_task
  list(
    id = id,
    accepted = isTRUE(obj$meta$accepted),
    nobs = obj$meta$nobs,
    n_participants = obj$meta$n_participants,
    n_adt = unname(by_task_n[["ADT"]]),
    n_vdt = unname(by_task_n[["VDT"]]),
    p_adt = unname(by_task_p[["ADT"]]),
    p_vdt = unname(by_task_p[["VDT"]]),
    singular = isTRUE(obj$meta$singular),
    step = obj$meta$reduction_step,
    formula = obj$meta$formula,
    lme4 = obj$meta$lme4_version,
    lmerTest = obj$meta$lmerTest_version,
    keys = keys,
    fixef = obj$fixef,
    ranef = obj$ranef
  )
}

ids <- list(
  M1 = c("effort_c", "catch:effort_c", "x_c:effort_c"),
  M2 = c("effort_c"),
  M3 = c("effort_c", "x_c", "catch"),
  M4a = c("x_c:b1_state", "b1_state", "catch:b1_state"),
  M4b = c("x_c:tepr_state", "tepr_state", "catch:tepr_state"),
  M5 = c("effort_c"),
  S1 = c("x_c:teprauc_state", "teprauc_state", "catch:teprauc_state"),
  S2 = c("effort_c", "catch:effort_c", "x_c:effort_c")
)

packed <- lapply(names(ids), function(id) pack_model(id, ids[[id]]))
names(packed) <- names(ids)

stopifnot(packed$M1$accepted, packed$M1$nobs == 18266L, packed$M2$nobs == 5240L)
stopifnot(packed$M3$nobs == 6299L, packed$M4a$nobs == 6299L, packed$M4b$nobs == 6299L)
stopifnot(packed$M5$nobs == 12630L, packed$S1$accepted, !packed$S2$accepted)
stopifnot(packed$S2$singular)

m1_path <- file.path(DIAG, "analysis_data_m1.csv")
stopifnot(file.exists(m1_path))
m1 <- fread(m1_path, select = c("task", "stim_level_index", "effort_c", "diff"))
m1[, effort := fifelse(effort_c > 0, "High", "Low")]
m1[, offset_label := fcase(
  task == "ADT" & stim_level_index == 0L, "0 Hz",
  task == "ADT" & stim_level_index == 1L, "8 Hz",
  task == "ADT" & stim_level_index == 2L, "16 Hz",
  task == "ADT" & stim_level_index == 3L, "32 Hz",
  task == "ADT" & stim_level_index == 4L, "64 Hz",
  task == "VDT" & stim_level_index == 0L, "0",
  task == "VDT" & stim_level_index == 1L, "0.06",
  task == "VDT" & stim_level_index == 2L, "0.12",
  task == "VDT" & stim_level_index == 3L, "0.24",
  task == "VDT" & stim_level_index == 4L, "0.48"
)]

cells <- m1[, .(
  n = .N,
  n_diff = sum(diff),
  p_diff = mean(diff)
), by = .(task, stim_level_index, offset_label, effort)]
setorder(cells, task, stim_level_index, effort)
wide <- dcast(cells, task + stim_level_index + offset_label ~ effort, value.var = c("n", "p_diff"))
wide[, high_minus_low := p_diff_High - p_diff_Low]
wide[, high_lower := high_minus_low < 0]
stopifnot(nrow(wide) == 10L, all(wide$high_lower))

top <- m1[stim_level_index == 4L, .(
  n = .N,
  p_diff = mean(diff)
), by = task]
catch_fa <- m1[stim_level_index == 0L, .(
  n = .N,
  p_diff = mean(diff)
), by = task]

m2_var <- packed$M2$ranef[grp == "subject" & var1 == "effort_c" & (is.na(var2) | var2 == "")]
stopifnot(nrow(m2_var) == 1L)

coef_table <- rbindlist(lapply(names(packed), function(id) {
  fx <- packed[[id]]$fixef
  fx[, model := id]
  fx[, accepted := packed[[id]]$accepted]
  fx[, stat_fmt := vapply(statistic, fmt_signed, character(1), digits = 2)]
  fx[, df_fmt := vapply(df, fmt_df, character(1))]
  fx[, est_fmt := vapply(estimate, fmt_est, character(1))]
  fx[, se_fmt := vapply(SE, fmt_num, character(1), digits = 3)]
  fx[, p_fmt := vapply(p, fmt_p, character(1))]
  fx[, ci_fmt := mapply(fmt_ci, ci_low, ci_high)]
  fx[, .(model, accepted, term, est_fmt, se_fmt, stat_fmt, df_fmt, ci_fmt, p_fmt, key_term)]
}))
fwrite(coef_table, file.path(OUT, "coefficients.csv"))
fwrite(cells, file.path(OUT, "p_diff_by_cell.csv"))
fwrite(wide, file.path(OUT, "p_diff_high_minus_low.csv"))

fig <- ggplot(cells, aes(x = stim_level_index, y = p_diff, color = effort, group = effort)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ task, labeller = as_labeller(c(ADT = "Auditory", VDT = "Visual"))) +
  scale_x_continuous(
    breaks = 0:4,
    labels = c("Catch", "1", "2", "3", "4")
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_color_manual(values = c(Low = "#4C78A8", High = "#E45756"), name = "Effort") +
  labs(
    x = "Offset level (catch, then the four increasing offsets)",
    y = "P(\"different\")"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(OUT, "fig_p_diff.png"), fig, width = 7.2, height = 3.6, dpi = 150)

drop_heavy <- function(x) {
  x$fixef <- NULL
  x$ranef <- NULL
  x
}
export_models <- lapply(packed, drop_heavy)

payload <- list(
  run = RUN,
  source = "Phase 2b locked fits. This export does not refit them.",
  lme4 = packed$M1$lme4,
  lmerTest = packed$M1$lmerTest,
  models = export_models,
  descriptives = list(
    high_lower_all_10 = TRUE,
    n_cells = 10L,
    top = lapply(split(top, top$task), function(r) {
      list(task = r$task, n = r$n, p_diff = r$p_diff, p_fmt = fmt_num(r$p_diff, 2))
    }),
    catch = lapply(split(catch_fa, catch_fa$task), function(r) {
      list(task = r$task, n = r$n, p_diff = r$p_diff, p_fmt = fmt_num(r$p_diff, 3))
    }),
    level4 = lapply(split(wide[stim_level_index == 4L], wide[stim_level_index == 4L]$task), function(r) {
      list(
        task = r$task,
        offset_label = r$offset_label,
        high_minus_low = r$high_minus_low,
        diff_fmt = fmt_est(r$high_minus_low),
        p_high_fmt = fmt_num(r$p_diff_High, 3),
        p_low_fmt = fmt_num(r$p_diff_Low, 3)
      )
    })
  ),
  m2_effort_slope_variance = m2_var$vcov,
  m2_effort_slope_variance_fmt = fmt_num(m2_var$vcov, 3),
  m2_effort_df_fmt = packed$M2$keys$effort_c$df_fmt
)
allfit <- rbindlist(lapply(
  list.files(DIAG, pattern = "^allFit_M.*\\.csv$", full.names = TRUE),
  fread
))
stopifnot(nrow(allfit) > 0L, !any(allfit$flag_gt_0.01 %in% TRUE))
payload$allfit <- list(
  any_flag = FALSE,
  max_abs_dev = max(allfit$max_abs_dev_key, na.rm = TRUE),
  max_abs_dev_fmt = fmt_est(max(allfit$max_abs_dev_key, na.rm = TRUE)),
  optimizers = paste(sort(unique(allfit$optimizer)), collapse = ", ")
)

write_json(payload, file.path(OUT, "values.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)

cat("WROTE", OUT, "\n")
cat("M1 effort", packed$M1$keys$effort_c$est_fmt, packed$M1$keys$effort_c$p_fmt, "\n")
cat("top", paste(top$task, top$p_diff, collapse = " | "), "\n")
cat("high_lower_all_10 TRUE\n")
