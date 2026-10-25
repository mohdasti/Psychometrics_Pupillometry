#!/usr/bin/env Rscript
# Chapter 2 Phase 2b, part 2. Fit the locked models M1-M5, S1, and S2.
# A fit is accepted only when it is non-singular and conv$lme4$messages is NULL.
# STOP_2 is per model. A sample-size mismatch ends the run.

options(warn = 1, scipen = 999)
suppressPackageStartupMessages({
  library(data.table)
  library(lme4)
  library(lmerTest)
  library(jsonlite)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1L]])) {
  stop("Usage: Rscript scripts/phase2b/02_fit_models.R RUN", call. = FALSE)
}
RUN <- args[[1L]]

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
DIAG <- file.path(CH2, "output/diagnostics/ch2_phase2b", RUN)
MODELS <- file.path(CH2, "output/models/ch2_phase2b", RUN)
TEST <- file.path(CH2, "tests/test_ch2_coding.R")
LOG <- file.path(DIAG, "run_log.txt")
REDUCTION_PATH <- file.path(DIAG, "P2_reduction_log.csv")
STATUS <- 0L
DEVIATIONS <- character()
UNDETERMINED <- character()
ACCEPTED <- list()
COEFS <- list()

dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
dir.create(MODELS, recursive = TRUE, showWarnings = FALSE)

log_line <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = LOG, append = TRUE)
  flush(stdout())
}

log_mem <- function(where) {
  g <- gc(FALSE)
  rss <- suppressWarnings(system2("ps", c("-o", "rss=", "-p", as.character(Sys.getpid())), stdout = TRUE))
  log_line("MEM ", where,
           " rss_kb=", paste(rss, collapse = ""),
           " gc_Ncells_used=", g[1, "used"],
           " gc_Vcells_used=", g[2, "used"])
}

add_deviation <- function(txt) {
  DEVIATIONS <<- c(DEVIATIONS, txt)
  log_line("DEVIATION ", txt)
}

add_undetermined <- function(txt) {
  UNDETERMINED <<- c(UNDETERMINED, txt)
  log_line("UNDETERMINED ", txt)
}

write_stop <- function(part, step, detail, fatal) {
  path <- file.path(DIAG, sprintf("STOPPED_%s.md", part))
  block <- c(
    sprintf("# STOPPED_%s", part),
    "",
    paste("Part:", part),
    paste("Step:", step),
    paste("Fatal:", fatal),
    "",
    "## Error or count",
    "",
    detail,
    ""
  )
  if (file.exists(path)) {
    cat(c("---", "", block), file = path, sep = "\n", append = TRUE)
  } else {
    writeLines(block, path)
  }
  log_line("WROTE ", path, if (fatal) " FATAL" else " continue")
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
  log_line("ZIP ", zip_path, " bytes ", file.info(zip_path)$size)
  zip_path
}

fatal <- function(step, detail) {
  write_stop("2", step, detail, TRUE)
  STATUS <<- 1L
  stop(paste0("FATAL ", step, ": ", detail), call. = FALSE)
}

fmt_full <- function(x) {
  if (length(x) != 1L || is.na(x)) return("NA")
  sprintf("%.16g", x)
}

ctrl_glmer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
ctrl_lmer <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
fam_probit <- binomial(link = "probit")
fam_logit <- binomial(link = "logit")

re_formula <- function(slopes, correlated) {
  if (!length(slopes)) return("(1 | subject)")
  op <- if (correlated) "|" else "||"
  sprintf("(1 + %s %s subject)", paste(slopes, collapse = " + "), op)
}

full_formula <- function(fixed, slopes, correlated) {
  paste(fixed, "+", re_formula(slopes, correlated))
}

conv_messages <- function(fit) {
  if (!inherits(fit, "merMod")) return(NULL)
  fit@optinfo$conv$lme4$messages
}

max_abs_grad <- function(fit) {
  if (!inherits(fit, "merMod")) return(NA_real_)
  g <- fit@optinfo$derivs$gradient
  if (is.null(g) || !length(g)) return(NA_real_)
  max(abs(g))
}

is_accepted <- function(fit) {
  if (!inherits(fit, "merMod")) return(FALSE)
  isFALSE(isSingular(fit)) && is.null(conv_messages(fit))
}

re_pieces <- function(fit) {
  if (!inherits(fit, "merMod")) return(list(var = NA_character_, cor = NA_character_))
  vc <- as.data.frame(VarCorr(fit), stringsAsFactors = FALSE)
  if (!nrow(vc)) return(list(var = NA_character_, cor = NA_character_))
  var_rows <- vc[is.na(vc$var2), , drop = FALSE]
  cor_rows <- vc[!is.na(vc$var2), , drop = FALSE]
  var_txt <- if (nrow(var_rows)) {
    paste(sprintf("%s:%s=%.8g", var_rows$grp, var_rows$var1, var_rows$vcov), collapse = "; ")
  } else {
    NA_character_
  }
  cor_txt <- if (nrow(cor_rows)) {
    paste(sprintf("%s:%s:%s=%.8g", cor_rows$grp, cor_rows$var1, cor_rows$var2, cor_rows$sdcor), collapse = "; ")
  } else {
    NA_character_
  }
  list(var = var_txt, cor = cor_txt)
}

slope_variances <- function(fit, slopes) {
  if (!inherits(fit, "merMod") || !length(slopes)) return(NULL)
  vc <- as.data.frame(VarCorr(fit), stringsAsFactors = FALSE)
  if (!nrow(vc)) return(NULL)
  var_rows <- vc[is.na(vc$var2), , drop = FALSE]
  if (!nrow(var_rows)) return(NULL)
  out <- setNames(var_rows$vcov, var_rows$var1)
  out <- out[!duplicated(names(out))]
  if (!all(slopes %in% names(out))) return(NULL)
  out[slopes]
}

append_reduction <- function(row) {
  dt <- as.data.table(row)
  if (file.exists(REDUCTION_PATH)) {
    fwrite(dt, REDUCTION_PATH, append = TRUE)
  } else {
    fwrite(dt, REDUCTION_PATH)
  }
}

fit_once <- function(formula_string, data, kind) {
  fml <- as.formula(formula_string)
  # allFit() re-evaluates the stored call. The symbol in data = must exist in
  # the global environment, and the control expression must be a call, not a
  # symbol. Naming the argument `data` collides with allFit's own data argument.
  assign("ch2_model_data", as.data.frame(data), envir = .GlobalEnv)
  if (kind == "lmer") {
    lmerTest::lmer(
      fml, data = ch2_model_data, REML = TRUE, na.action = na.fail,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  } else if (kind == "probit") {
    glmer(
      fml, data = ch2_model_data, family = binomial(link = "probit"), na.action = na.fail,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  } else if (kind == "logit") {
    glmer(
      fml, data = ch2_model_data, family = binomial(link = "logit"), na.action = na.fail,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    )
  } else {
    stop("unknown kind ", kind, call. = FALSE)
  }
}

attempt_fit <- function(model_id, step, formula_string, data, kind) {
  log_mem(paste(model_id, step, "before"))
  log_line("START ", model_id, " ", step, " ", formula_string)
  t0 <- proc.time()[["elapsed"]]
  fit <- tryCatch(fit_once(formula_string, data, kind), error = function(e) e)
  log_mem(paste(model_id, step, "after"))
  elapsed <- proc.time()[["elapsed"]] - t0
  accepted <- is_accepted(fit)
  if (inherits(fit, "error")) {
    singular <- NA
    messages <- conditionMessage(fit)
    grad <- NA_real_
    pieces <- list(var = NA_character_, cor = NA_character_)
    log_line("ERROR ", model_id, " ", step, " ", messages)
  } else {
    singular <- isTRUE(isSingular(fit))
    msg <- conv_messages(fit)
    messages <- if (is.null(msg)) "NULL" else paste(msg, collapse = " | ")
    grad <- max_abs_grad(fit)
    pieces <- re_pieces(fit)
    log_line("END ", model_id, " ", step, " singular=", singular,
             " messages=", messages, " accepted=", accepted,
             " sec=", sprintf("%.1f", elapsed))
  }
  append_reduction(list(
    model = model_id,
    step = step,
    formula = formula_string,
    singular = if (is.na(singular)) NA_character_ else as.character(singular),
    conv_messages = messages,
    max_abs_grad = grad,
    re_variances = pieces$var,
    re_correlations = pieces$cor,
    elapsed_sec = elapsed,
    accepted = accepted
  ))
  list(fit = fit, accepted = accepted, formula = formula_string, step = step)
}

coef_table <- function(fit, key_terms) {
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
  z <- stats::qnorm(0.975)
  data.table(
    term = rownames(sm),
    estimate = as.numeric(est),
    SE = as.numeric(se),
    statistic = as.numeric(stat),
    df = as.numeric(df),
    p = as.numeric(p),
    ci_low = as.numeric(est) - z * as.numeric(se),
    ci_high = as.numeric(est) + z * as.numeric(se),
    key_term = rownames(sm) %in% key_terms
  )
}

write_fit_files <- function(model_id, fit, formula_string, step, data, key_terms, data_md5, accepted) {
  rds_path <- file.path(MODELS, paste0(model_id, "_fit.rds"))
  fixef_path <- file.path(MODELS, paste0(model_id, "_fixef.csv"))
  ranef_path <- file.path(MODELS, paste0(model_id, "_ranef_var.csv"))
  meta_path <- file.path(MODELS, paste0(model_id, "_meta.json"))
  if (inherits(fit, "merMod")) saveRDS(fit, rds_path)
  ct <- NULL
  if (inherits(fit, "merMod")) {
    ct <- coef_table(fit, key_terms)
    fwrite(ct, fixef_path)
    vc <- as.data.frame(VarCorr(fit), stringsAsFactors = FALSE)
    fwrite(vc, ranef_path)
  }
  by_task_n <- data[, .N, by = task]
  by_task_p <- data[, .(n = uniqueN(subject_id)), by = task]
  nobs_by_task <- setNames(as.list(by_task_n$N), by_task_n$task)
  npart_by_task <- setNames(as.list(by_task_p$n), by_task_p$task)
  msgs <- if (inherits(fit, "merMod")) conv_messages(fit) else NULL
  meta <- list(
    model = model_id,
    nobs = if (inherits(fit, "merMod")) nobs(fit) else nrow(data),
    n_participants = uniqueN(data$subject_id),
    nobs_by_task = nobs_by_task,
    n_participants_by_task = npart_by_task,
    accepted = isTRUE(accepted),
    formula = formula_string,
    reduction_step = step,
    optimizer = if (inherits(fit, "merMod")) fit@optinfo$optimizer else NA_character_,
    singular = if (inherits(fit, "merMod")) isTRUE(isSingular(fit)) else NA,
    convergence_messages = msgs,
    logLik = if (inherits(fit, "merMod")) as.numeric(logLik(fit)) else NA_real_,
    AIC = if (inherits(fit, "merMod")) AIC(fit) else NA_real_,
    data_md5 = data_md5,
    lme4_version = as.character(packageVersion("lme4")),
    lmerTest_version = as.character(packageVersion("lmerTest"))
  )
  write_json(meta, meta_path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 16)
  ct
}

key_line <- function(ct, term_name) {
  if (is.null(ct)) {
    return(paste0(term_name, " estimate=NA SE=NA CI=[NA, NA] p=NA"))
  }
  hit <- which(ct$term == term_name)
  if (length(hit) != 1L) {
    return(paste0(term_name, " estimate=NA SE=NA CI=[NA, NA] p=NA"))
  }
  paste0(term_name,
         " estimate=", fmt_full(ct$estimate[[hit]]),
         " SE=", fmt_full(ct$SE[[hit]]),
         " CI=[", fmt_full(ct$ci_low[[hit]]), ", ", fmt_full(ct$ci_high[[hit]]), "]",
         " p=", fmt_full(ct$p[[hit]]))
}

summary_line <- function(model_id, accepted, step, data, ct, key_terms, formula_string, singular, messages) {
  bits <- c(
    model_id,
    paste0("accepted=", accepted),
    paste0("step=", step),
    paste0("nobs=", nrow(data)),
    paste0("participants=", uniqueN(data$subject_id)),
    paste0("singular=", singular),
    paste0("convergence=", messages),
    paste0("formula=", formula_string)
  )
  if (!is.null(ct)) {
    bits <- c(bits, vapply(key_terms, function(tm) key_line(ct, tm), character(1)))
  }
  paste(bits, collapse = " | ")
}

prepare_sample <- function(filename, n_expect, extra_cols) {
  path <- file.path(DIAG, filename)
  stopifnot(file.exists(path))
  d <- fread(path)
  if (nrow(d) != n_expect) {
    fatal("sample_size", paste(filename, "expected", n_expect, "observed", nrow(d)))
  }
  need <- c("subject_id", "task", "effort_c", "task_c", extra_cols)
  missing <- unique(need[!need %in% names(d)])
  if (length(missing)) fatal("columns", paste(filename, "missing", paste(missing, collapse = ",")))
  d[, subject := factor(subject_id)]
  d[]
}

assert_binary <- function(d, col, filename) {
  if (any(!d[[col]] %in% c(0, 1))) {
    fatal("outcome", paste(filename, col, "has a value outside 0/1"))
  }
  d[, (col) := as.integer(get(col))]
}

assert_sum_codes <- function(d, filename) {
  if (any(!d$effort_c %in% c(-0.5, 0.5))) fatal("effort_c", paste(filename, "effort_c outside -0.5/0.5"))
  if (any(!d$task_c %in% c(-0.5, 0.5))) fatal("task_c", paste(filename, "task_c outside -0.5/0.5"))
  xc_ok <- c(-1.5, -0.5, 0, 0.5, 1.5)
  if ("x_c" %in% names(d) && any(!d$x_c %in% xc_ok)) {
    fatal("x_c", paste(filename, "x_c outside -1.5,-0.5,0,0.5,1.5"))
  }
  if ("catch" %in% names(d)) {
    d[, catch := as.numeric(catch)]
    if (any(d[stim_level_index == 0L, x_c] != 0)) fatal("x_c", paste(filename, "level 0 x_c is not 0"))
    if (any(d[stim_level_index == 0L, catch] != 1)) fatal("catch", paste(filename, "level 0 catch is not 1"))
  }
  d[]
}

check_nobs <- function(fit, n_expect, model_id) {
  if (!inherits(fit, "merMod")) return(invisible())
  if (nobs(fit) != n_expect) {
    fatal("nobs", paste(model_id, "nobs", nobs(fit), "expected", n_expect))
  }
}

check_link <- function(fit, kind, model_id) {
  if (!inherits(fit, "merMod")) return(invisible())
  if (kind == "lmer") {
    if (isGLMM(fit)) fatal("link", paste(model_id, "fitted as glmer"))
    return(invisible())
  }
  link <- fit@resp$family$link
  if (kind == "probit" && !identical(link, "probit")) fatal("link", paste(model_id, "link", link))
  if (kind == "logit" && !identical(link, "logit")) fatal("link", paste(model_id, "link", link))
}

load_accepted <- function(model_id, data, key_terms) {
  meta_path <- file.path(MODELS, paste0(model_id, "_meta.json"))
  rds_path <- file.path(MODELS, paste0(model_id, "_fit.rds"))
  fixef_path <- file.path(MODELS, paste0(model_id, "_fixef.csv"))
  if (!file.exists(meta_path) || !file.exists(rds_path) || !file.exists(fixef_path)) return(NULL)
  meta <- fromJSON(meta_path, simplifyVector = TRUE)
  if (!isTRUE(meta$accepted)) return(NULL)
  fit <- readRDS(rds_path)
  saved <- fread(fixef_path)
  live <- coef_table(fit, key_terms)
  both <- merge(saved[, .(term, estimate)], live[, .(term, estimate)], by = "term", suffixes = c("_saved", "_live"))
  if (nrow(both) != nrow(saved) || nrow(both) != nrow(live)) {
    fatal("resume_fixef", paste(model_id, "saved and loaded coefficient rows differ"))
  }
  max_abs <- max(abs(both$estimate_saved - both$estimate_live))
  log_line("SKIP ", model_id, " loaded accepted step ", meta$reduction_step,
           " fixef max abs diff ", sprintf("%.3e", max_abs))
  if (!is.finite(max_abs) || max_abs > 0) {
    fatal("resume_fixef", paste(model_id, "loaded fixed effects differ from the saved CSV by", max_abs))
  }
  check_nobs(fit, nrow(data), model_id)
  ct <- live
  ACCEPTED[[model_id]] <<- list(fit = fit, formula = meta$formula, step = meta$reduction_step,
                               key_terms = key_terms, ct = ct, data = data)
  COEFS[[model_id]] <<- ct
  list(accepted = TRUE, ct = ct, step = meta$reduction_step, formula = meta$formula,
       singular = "FALSE", messages = "NULL")
}

fit_ladder <- function(model_id, data, fixed, slopes, kind, key_terms, data_md5) {
  loaded <- load_accepted(model_id, data, key_terms)
  if (!is.null(loaded)) return(loaded)
  current <- slopes
  prev <- NULL
  result <- NULL
  steps <- list(
    list(step = "1_maximal", slopes = current, correlated = TRUE),
    list(step = "2_uncorrelated", slopes = current, correlated = FALSE)
  )
  for (st in steps) {
    if (!length(current) && st$step == "2_uncorrelated") next
    fml <- full_formula(fixed, st$slopes, st$correlated)
    att <- attempt_fit(model_id, st$step, fml, data, kind)
    prev <- att
    if (att$accepted) {
      result <- att
      break
    }
  }
  if (is.null(result) && length(current)) {
    repeat {
      vars <- NULL
      if (!is.null(prev)) vars <- slope_variances(prev$fit, current)
      if (is.null(vars)) {
        add_deviation(paste(model_id, "slope drop skipped: previous fit has no variance for every remaining slope"))
        add_undetermined(paste(model_id, "which random slope to drop next"))
        break
      }
      if (sum(abs(vars - min(vars)) <= 1e-12) > 1L) {
        add_deviation(paste(model_id, "tied random-slope variances; dropped the earliest tied slope in the current list:",
                            current[which.min(vars)]))
      }
      drop <- current[which.min(vars)]
      current <- current[current != drop]
      if (!length(current)) {
        fml <- full_formula(fixed, character(), TRUE)
        prev <- attempt_fit(model_id, "4_intercept", fml, data, kind)
        if (prev$accepted) result <- prev
        break
      }
      fml <- full_formula(fixed, current, FALSE)
      prev <- attempt_fit(model_id, paste0("3_drop_", drop), fml, data, kind)
      if (prev$accepted) {
        result <- prev
        break
      }
    }
  }
  if (is.null(result)) {
    already <- FALSE
    if (file.exists(REDUCTION_PATH)) {
      logged <- fread(REDUCTION_PATH)
      already <- any(logged$model == model_id & logged$step == "4_intercept")
    }
    if (!already) {
      fml <- full_formula(fixed, character(), TRUE)
      att <- attempt_fit(model_id, "4_intercept", fml, data, kind)
      if (att$accepted) result <- att
    }
  }
  if (is.null(result)) {
    write_stop("2", model_id, paste(model_id, "no ladder step was accepted"), FALSE)
    return(list(accepted = FALSE, ct = NULL, step = "none", formula = NA_character_,
                singular = "NA", messages = "NA"))
  }
  check_nobs(result$fit, nrow(data), model_id)
  check_link(result$fit, kind, model_id)
  ct <- write_fit_files(model_id, result$fit, result$formula, result$step, data, key_terms, data_md5, TRUE)
  missing_keys <- key_terms[!key_terms %in% ct$term]
  if (length(missing_keys)) {
    add_deviation(paste(model_id, "key term absent from coefficients:", paste(missing_keys, collapse = ",")))
  }
  ACCEPTED[[model_id]] <<- list(fit = result$fit, formula = result$formula, step = result$step,
                               key_terms = key_terms, ct = ct, data = data)
  COEFS[[model_id]] <<- ct
  list(accepted = TRUE, ct = ct, step = result$step, formula = result$formula,
       singular = "FALSE", messages = "NULL")
}

fit_specified <- function(model_id, data, formula_string, kind, key_terms, data_md5, step) {
  loaded <- load_accepted(model_id, data, key_terms)
  if (!is.null(loaded)) return(loaded)
  att <- attempt_fit(model_id, step, formula_string, data, kind)
  if (!inherits(att$fit, "merMod")) {
    write_stop("2", model_id, paste(model_id, "fit error:", conditionMessage(att$fit)), FALSE)
    return(list(accepted = FALSE, ct = NULL, step = step, formula = formula_string,
                singular = "NA", messages = conditionMessage(att$fit)))
  }
  check_nobs(att$fit, nrow(data), model_id)
  check_link(att$fit, kind, model_id)
  ct <- write_fit_files(model_id, att$fit, formula_string, step, data, key_terms, data_md5, att$accepted)
  COEFS[[model_id]] <<- ct
  if (!att$accepted) {
    msg <- conv_messages(att$fit)
    detail <- paste(model_id, "not accepted. singular=", isTRUE(isSingular(att$fit)),
                    " messages=", if (is.null(msg)) "NULL" else paste(msg, collapse = " | "))
    write_stop("2", model_id, detail, FALSE)
  } else {
    ACCEPTED[[model_id]] <<- list(fit = att$fit, formula = formula_string, step = step,
                                 key_terms = key_terms, ct = ct, data = data)
  }
  msg <- conv_messages(att$fit)
  list(accepted = att$accepted, ct = ct, step = step, formula = formula_string,
       singular = as.character(isTRUE(isSingular(att$fit))),
       messages = if (is.null(msg)) "NULL" else paste(msg, collapse = " | "))
}

run_all_fit <- function(model_id) {
  info <- ACCEPTED[[model_id]]
  if (is.null(info)) {
    log_line("allFit ", model_id, " skipped")
    return(invisible())
  }
  log_mem(paste(model_id, "allFit before"))
  fit <- info$fit
  assign("ch2_allfit_data", as.data.frame(info$data), envir = .GlobalEnv)
  fml <- as.formula(info$formula)
  environment(fml) <- .GlobalEnv
  fit@call$formula <- fml
  fit@call$data <- quote(ch2_allfit_data)
  meth <- lme4:::meth.tab.0
  meth <- meth[meth[, "optimizer"] %in% c("bobyqa", "Nelder_Mead", "nlminbwrap", "nloptwrap"), , drop = FALSE]
  log_line("allFit START ", model_id, " maxfun=2e5 optimizers ",
           paste(gsub("\\.$", "", paste(meth[, "optimizer"], meth[, "method"], sep = ".")), collapse = ","))
  af <- tryCatch(
    allFit(fit, meth.tab = meth, maxfun = 2e5),
    error = function(e) e
  )
  log_mem(paste(model_id, "allFit after"))
  if (inherits(af, "error")) {
    add_deviation(paste(model_id, "allFit error:", conditionMessage(af)))
    add_undetermined(paste(model_id, "allFit"))
    return(invisible())
  }
  ref <- fixef(info$fit)
  rows <- list()
  for (nm in names(af)) {
    rr <- af[[nm]]
    if (inherits(rr, "error")) {
      rows[[length(rows) + 1L]] <- data.table(
        optimizer = nm, convergence = conditionMessage(rr), singular = NA,
        term = NA_character_, estimate = NA_real_, accepted_bobyqa_estimate = NA_real_,
        abs_dev = NA_real_, max_abs_dev_key = NA_real_, flag_gt_0.01 = FALSE
      )
      next
    }
    fe <- fixef(rr)
    msg <- rr@optinfo$conv$lme4$messages
    conv <- if (is.null(msg)) "OK" else paste(msg, collapse = " | ")
    sing <- isTRUE(isSingular(rr))
    present <- info$key_terms[info$key_terms %in% names(fe) & info$key_terms %in% names(ref)]
    if (!length(present)) {
      rows[[length(rows) + 1L]] <- data.table(
        optimizer = nm, convergence = conv, singular = sing,
        term = NA_character_, estimate = NA_real_, accepted_bobyqa_estimate = NA_real_,
        abs_dev = NA_real_, max_abs_dev_key = NA_real_, flag_gt_0.01 = FALSE
      )
      next
    }
    abs_dev <- abs(fe[present] - ref[present])
    max_dev <- max(abs_dev)
    flag <- is.null(msg) && isTRUE(max_dev > 0.01)
    for (tm in present) {
      rows[[length(rows) + 1L]] <- data.table(
        optimizer = nm, convergence = conv, singular = sing, term = tm,
        estimate = unname(fe[[tm]]),
        accepted_bobyqa_estimate = unname(ref[[tm]]),
        abs_dev = unname(abs_dev[[tm]]),
        max_abs_dev_key = max_dev,
        flag_gt_0.01 = flag
      )
    }
  }
  out <- rbindlist(rows)
  fwrite(out, file.path(DIAG, paste0("allFit_", model_id, ".csv")))
  log_line("allFit END ", model_id, " rows ", nrow(out))
  rm(af)
  gc()
  invisible(out)
}

m1_fit_check <- function() {
  info <- ACCEPTED[["M1"]]
  if (is.null(info)) return(invisible())
  d <- info$data
  if (any(d[, uniqueN(stim_offset_abs), by = .(task, stim_level_index)]$V1 != 1L)) {
    fatal("M1_fit_check", "stim_offset_abs is not unique within task x level")
  }
  d[, effort := ifelse(effort_c == 0.5, "High", "Low")]
  d[, p_hat := predict(info$fit, newdata = as.data.frame(d), type = "response", re.form = NA)]
  chk <- d[, .(
    n = .N,
    effort_c = effort_c[[1L]],
    stim_offset_abs = stim_offset_abs[[1L]],
    p_obs = mean(diff),
    p_pred = mean(p_hat)
  ), by = .(task, effort, stim_level_index)]
  setorder(chk, task, effort, stim_level_index)
  fwrite(chk, file.path(DIAG, "M1_fit_check.csv"))
  p <- ggplot(chk, aes(x = stim_offset_abs, colour = effort)) +
    geom_point(aes(y = p_obs), size = 2) +
    geom_line(aes(y = p_pred)) +
    facet_wrap(~ task, scales = "free_x") +
    labs(x = "physical offset", y = "P(different)", colour = "effort") +
    theme_bw()
  png_path <- file.path(DIAG, "M1_fit_check.png")
  grDevices::png(png_path, width = 8, height = 4.5, units = "in", res = 120)
  print(p)
  grDevices::dev.off()
  log_line("WROTE M1_fit_check")
}

write_report <- function(lines_summary) {
  p1_path <- file.path(DIAG, "P1_build_checks.csv")
  stopifnot(file.exists(p1_path))
  p1 <- fread(p1_path)
  red <- if (file.exists(REDUCTION_PATH)) fread(REDUCTION_PATH) else data.table()
  lines <- c("# Chapter 2 Phase 2b", "", "## 1. Summary", "")
  lines <- c(lines, lines_summary, "")
  lines <- c(lines, "Summary estimates are sprintf %.16g. Full coefficient rows are in the fixef CSV files.", "")
  lines <- c(lines, "## 2. Part 1 checks", "")
  lines <- c(lines, "| check | expected | observed | pass |", "|---|---|---|---|")
  for (i in seq_len(nrow(p1))) {
    lines <- c(lines, paste0("| ", p1$check[[i]], " | ", p1$expected[[i]], " | ",
                             p1$observed[[i]], " | ", p1$pass[[i]], " |"))
  }
  lines <- c(lines, "", "## 3. Reduction log", "")
  if (nrow(red)) {
    lines <- c(lines, "| model | step | singular | convergence | max_abs_grad | accepted | re_variances |",
               "|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(red))) {
      lines <- c(lines, paste0("| ", red$model[[i]], " | ", red$step[[i]], " | ",
                               red$singular[[i]], " | ", gsub("|", "/", red$conv_messages[[i]], fixed = TRUE),
                               " | ", fmt_full(red$max_abs_grad[[i]]), " | ", red$accepted[[i]],
                               " | ", red$re_variances[[i]], " |"))
    }
    lines <- c(lines, "")
    lines <- c(lines, "Formulas:")
    for (i in seq_len(nrow(red))) {
      lines <- c(lines, paste0(red$model[[i]], " ", red$step[[i]], ": ", red$formula[[i]]))
    }
  } else {
    lines <- c(lines, "P2_reduction_log.csv has 0 rows")
  }
  lines <- c(lines, "", "## 4. allFit", "")
  for (id in c("M1", "M4a", "M4b")) {
    path <- file.path(DIAG, paste0("allFit_", id, ".csv"))
    if (!file.exists(path)) {
      lines <- c(lines, paste0(id, " allFit file absent"))
      next
    }
    af <- fread(path)
    flagged_n <- sum(af$flag_gt_0.01 %in% TRUE)
    lines <- c(lines, paste0(id, " flag_rows=", flagged_n))
    opts <- unique(af$optimizer)
    for (op in opts) {
      sub <- af[which(af$optimizer == op)]
      lines <- c(lines, paste0(id, " | ", op,
                               " | convergence=", gsub("|", "/", sub$convergence[[1L]], fixed = TRUE),
                               " | singular=", sub$singular[[1L]],
                               " | max_abs_dev_key=", fmt_full(sub$max_abs_dev_key[[1L]]),
                               " | flag_gt_0.01=", sub$flag_gt_0.01[[1L]]))
      for (j in seq_len(nrow(sub))) {
        if (is.na(sub$term[[j]])) next
        lines <- c(lines, paste0("  ", sub$term[[j]],
                                 " estimate=", fmt_full(sub$estimate[[j]]),
                                 " bobyqa=", fmt_full(sub$accepted_bobyqa_estimate[[j]]),
                                 " abs_dev=", fmt_full(sub$abs_dev[[j]])))
      }
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "## 5. S1 beside M4b, S2 beside M1", "")
  cell_ct <- function(ct, term_name, col_name) {
    if (is.null(ct)) return("NA")
    hit <- which(ct$term == term_name)
    if (length(hit) != 1L) return("NA")
    fmt_full(ct[[col_name]][[hit]])
  }
  pair_table <- function(left_id, right_id, pairs) {
    out <- c(
      paste0("| term | ", left_id, " estimate | ", left_id, " SE | ", left_id, " CI low | ",
             left_id, " CI high | ", left_id, " p | ", right_id, " estimate | ", right_id,
             " SE | ", right_id, " CI low | ", right_id, " CI high | ", right_id, " p |"),
      "|---|---|---|---|---|---|---|---|---|---|---|"
    )
    left <- COEFS[[left_id]]
    right <- COEFS[[right_id]]
    for (i in seq_len(nrow(pairs))) {
      lt <- pairs$left[[i]]
      rt <- pairs$right[[i]]
      out <- c(out, paste0(
        "| ", lt, " / ", rt,
        " | ", cell_ct(left, lt, "estimate"),
        " | ", cell_ct(left, lt, "SE"),
        " | ", cell_ct(left, lt, "ci_low"),
        " | ", cell_ct(left, lt, "ci_high"),
        " | ", cell_ct(left, lt, "p"),
        " | ", cell_ct(right, rt, "estimate"),
        " | ", cell_ct(right, rt, "SE"),
        " | ", cell_ct(right, rt, "ci_low"),
        " | ", cell_ct(right, rt, "ci_high"),
        " | ", cell_ct(right, rt, "p"),
        " |"
      ))
    }
    out
  }
  lines <- c(lines, pair_table("M4b", "S1", data.table(
    left = c("x_c:tepr_state", "tepr_state", "catch:tepr_state"),
    right = c("x_c:teprauc_state", "teprauc_state", "catch:teprauc_state")
  )))
  lines <- c(lines, "")
  lines <- c(lines, pair_table("M1", "S2", data.table(
    left = c("effort_c", "catch:effort_c", "x_c:effort_c"),
    right = c("effort_c", "catch:effort_c", "x_c:effort_c")
  )))
  lines <- c(lines, "", "## 6. Deviations", "")
  if (!length(DEVIATIONS)) lines <- c(lines, "none") else lines <- c(lines, DEVIATIONS)
  lines <- c(lines, "", "## 7. UNDETERMINED", "")
  if (!length(UNDETERMINED)) lines <- c(lines, "none") else lines <- c(lines, UNDETERMINED)
  lines <- c(lines, "")
  report <- file.path(DIAG, "report.md")
  writeLines(lines, report)
  txt <- paste(lines, collapse = "\n")
  if (grepl("significant|supports|suggests|consistent with", txt, ignore.case = TRUE)) {
    fatal("report_words", "report.md contains a blocked word")
  }
  log_line("WROTE report.md")
}

write_a4 <- function(d_tepr) {
  specs <- list(
    list(model = "M4a", slope = "b1_state",
         formula = "diff ~ (catch + x_c) * (b1_state + effort_c + task_c) + b1_trait + (1 + catch + x_c + b1_state || subject)",
         terms = c("x_c:b1_state", "b1_state", "catch:b1_state")),
    list(model = "M4b", slope = "tepr_state",
         formula = "diff ~ (catch + x_c) * (tepr_state + effort_c + task_c) + tepr_trait + (1 + catch + x_c + tepr_state || subject)",
         terms = c("x_c:tepr_state", "tepr_state", "catch:tepr_state"))
  )
  rows <- list()
  for (sp in specs) {
    path <- file.path(MODELS, paste0(sp$model, "_2_uncorrelated_fit.rds"))
    if (file.exists(path)) {
      fit <- readRDS(path)
      log_line("A4 ", sp$model, " loaded saved uncorrelated fit")
    } else {
      log_mem(paste("A4", sp$model, "before"))
      fit <- tryCatch(fit_once(sp$formula, d_tepr, "probit"), error = function(e) e)
      log_mem(paste("A4", sp$model, "after"))
      if (!inherits(fit, "merMod")) fatal("A4", paste(sp$model, conditionMessage(fit)))
      saveRDS(fit, path)
      log_line("A4 ", sp$model, " refit uncorrelated state-slope model because it had not been saved")
    }
    vc <- as.data.frame(VarCorr(fit), stringsAsFactors = FALSE)
    var_rows <- vc[is.na(vc$var2) & vc$var1 == sp$slope, , drop = FALSE]
    re_var <- if (nrow(var_rows)) var_rows$vcov[[1L]] else NA_real_
    re_sd <- if (nrow(var_rows)) var_rows$sdcor[[1L]] else NA_real_
    ct_slope <- coef_table(fit, sp$terms)
    ct_acc <- COEFS[[sp$model]]
    one <- function(term_name) {
      hs <- which(ct_slope$term == term_name)
      ha <- which(ct_acc$term == term_name)
      data.table(
        model = sp$model,
        term = term_name,
        estimate_with_state_slope = if (length(hs) == 1L) ct_slope$estimate[[hs]] else NA_real_,
        SE_with_state_slope = if (length(hs) == 1L) ct_slope$SE[[hs]] else NA_real_,
        estimate_accepted = if (length(ha) == 1L) ct_acc$estimate[[ha]] else NA_real_,
        SE_accepted = if (length(ha) == 1L) ct_acc$SE[[ha]] else NA_real_,
        re_variance_with_state_slope = re_var,
        re_sd_with_state_slope = re_sd
      )
    }
    for (tm in sp$terms) rows[[length(rows) + 1L]] <- one(tm)
    rows[[length(rows) + 1L]] <- data.table(
      model = sp$model, term = paste0("RE:", sp$slope),
      estimate_with_state_slope = NA_real_, SE_with_state_slope = NA_real_,
      estimate_accepted = NA_real_, SE_accepted = NA_real_,
      re_variance_with_state_slope = re_var, re_sd_with_state_slope = re_sd
    )
  }
  out <- rbindlist(rows)
  fwrite(out, file.path(DIAG, "A4_state_slope_comparison.csv"))
  log_line("WROTE A4_state_slope_comparison.csv")
  out
}

fit_s1_from_m4b <- function(d_tepr, md5_tepr) {
  key_terms <- c("x_c:teprauc_state", "teprauc_state", "catch:teprauc_state")
  f0 <- gsub("tepr_state", "teprauc_state", ACCEPTED[["M4b"]]$formula, fixed = TRUE)
  f0 <- gsub("tepr_trait", "teprauc_trait", f0, fixed = TRUE)
  step0 <- paste0("same_as_M4b_", ACCEPTED[["M4b"]]$step)
  fixed <- "diff ~ (catch + x_c) * (teprauc_state + effort_c + task_c) + teprauc_trait"
  rds <- file.path(MODELS, "S1_fit.rds")
  meta_path <- file.path(MODELS, "S1_meta.json")
  att <- NULL
  if (file.exists(rds) && file.exists(meta_path)) {
    meta <- fromJSON(meta_path, simplifyVector = TRUE)
    if (identical(as.character(meta$formula), f0) && identical(as.character(meta$reduction_step), step0)) {
      fit <- readRDS(rds)
      log_line("S1 loaded existing ", step0, " accepted=", isTRUE(meta$accepted))
      att <- list(fit = fit, accepted = isTRUE(meta$accepted) && is_accepted(fit), formula = f0, step = step0)
    }
  }
  if (is.null(att)) att <- attempt_fit("S1", step0, f0, d_tepr, "probit")
  result <- if (isTRUE(att$accepted)) att else NULL
  if (is.null(result)) {
    add_deviation("Deviation D-S1. S1 did not converge with M4b's accepted random-effects structure. The reduction ladder was continued from that structure.")
    slopes <- c("catch", "x_c")
    prev <- att
    repeat {
      vars <- slope_variances(prev$fit, slopes)
      if (is.null(vars)) {
        add_deviation("S1 slope drop skipped: previous fit has no variance for every remaining slope")
        break
      }
      if (sum(abs(vars - min(vars)) <= 1e-12) > 1L) {
        add_deviation(paste("S1 tied random-slope variances; dropped the earliest tied slope in the current list:",
                            slopes[which.min(vars)]))
      }
      drop <- slopes[which.min(vars)]
      slopes <- slopes[slopes != drop]
      if (!length(slopes)) {
        fml <- paste(fixed, "+ (1 | subject)")
        prev <- attempt_fit("S1", "4_intercept", fml, d_tepr, "probit")
        if (isTRUE(prev$accepted)) result <- prev
        break
      }
      fml <- full_formula(fixed, slopes, FALSE)
      prev <- attempt_fit("S1", paste0("D-S1_drop_", drop), fml, d_tepr, "probit")
      if (isTRUE(prev$accepted)) {
        result <- prev
        break
      }
    }
  }
  if (is.null(result)) {
    ct <- if (inherits(att$fit, "merMod")) coef_table(att$fit, key_terms) else NULL
    COEFS[["S1"]] <<- ct
    msg <- if (inherits(att$fit, "merMod")) conv_messages(att$fit) else NULL
    grad <- if (inherits(att$fit, "merMod")) max_abs_grad(att$fit) else NA_real_
    log_line("S1 not converged max_abs_grad=", fmt_full(grad))
    return(list(accepted = FALSE, ct = ct, step = step0, formula = f0,
                singular = if (inherits(att$fit, "merMod")) as.character(isTRUE(isSingular(att$fit))) else "NA",
                messages = if (is.null(msg)) "NULL" else paste(msg, collapse = " | ")))
  }
  check_nobs(result$fit, nrow(d_tepr), "S1")
  check_link(result$fit, "probit", "S1")
  ct <- write_fit_files("S1", result$fit, result$formula, result$step, d_tepr, key_terms, md5_tepr, TRUE)
  COEFS[["S1"]] <<- ct
  ACCEPTED[["S1"]] <<- list(fit = result$fit, formula = result$formula, step = result$step,
                           key_terms = key_terms, ct = ct, data = d_tepr)
  list(accepted = TRUE, ct = ct, step = result$step, formula = result$formula,
       singular = "FALSE", messages = "NULL")
}

main <- function() {
  log_line("R ", R.version.string)
  log_line("lme4 ", as.character(packageVersion("lme4")))
  log_line("lmerTest ", as.character(packageVersion("lmerTest")))
  log_line("optimx ", as.character(packageVersion("optimx")))
  log_line("dfoptim ", as.character(packageVersion("dfoptim")))
  log_line("RUN ", RUN)
  add_deviation("Plan text names lme4 1.1-35. This run uses the installed lme4 and records that version.")
  add_deviation("allFit(fit, maxfun = 2e5). allFit default maxfun is 1e5. Model controls use maxfun 2e5. start_from_mle stays at the installed allFit default TRUE.")
  add_deviation("allFit optimizer set: bobyqa, Nelder_Mead, nlminbwrap, nloptwrap.NLOPT_LN_NELDERMEAD, nloptwrap.NLOPT_LN_BOBYQA. optimx and dfoptim are installed. This resume uses the optimizers that ship with lme4.")

  p1_path <- file.path(DIAG, "P1_build_checks.csv")
  stopifnot(file.exists(p1_path))
  p1 <- fread(p1_path)
  if (p1[gate == TRUE & pass == FALSE, .N] > 0L) fatal("P1", "a gated Part 1 check is not pass")

  d_m1 <- prepare_sample("analysis_data_m1.csv", 18266L, c("diff", "catch", "x_c", "stim_level_index", "stim_offset_abs"))
  d_m2 <- prepare_sample("analysis_data_m2.csv", 5240L, "grip_mean")
  d_tepr <- prepare_sample("analysis_data_tepr.csv", 6299L, c(
    "diff", "catch", "x_c", "stim_level_index", "b1_state", "b1_trait",
    "tepr_state", "tepr_trait", "teprauc_state", "teprauc_trait", "tepr_mean_465_765"
  ))
  d_m5 <- prepare_sample("analysis_data_m5.csv", 12630L, c("in_tepr_final", "catch", "x_c", "stim_level_index"))
  assert_binary(d_m1, "diff", "m1")
  assert_binary(d_tepr, "diff", "tepr")
  assert_binary(d_m5, "in_tepr_final", "m5")
  d_m1 <- assert_sum_codes(d_m1, "m1")
  d_m2 <- assert_sum_codes(d_m2, "m2")
  d_tepr <- assert_sum_codes(d_tepr, "tepr")
  d_m5 <- assert_sum_codes(d_m5, "m5")
  if (any(!is.finite(d_m2$grip_mean))) fatal("grip_mean", "M2 grip_mean is not finite on every row")
  if (any(!is.finite(d_tepr$tepr_mean_465_765))) fatal("tepr_mean", "M3 outcome is not finite on every row")
  if (sum(d_m5$in_tepr_final) != 6299L) {
    fatal("m5_outcome", paste("sum in_tepr_final", sum(d_m5$in_tepr_final), "expected 6299"))
  }
  state_cols <- c("b1_state", "tepr_state", "teprauc_state")
  for (col in state_cols) {
    mx <- max(abs(d_tepr[, .(m = mean(.SD[[1L]])), by = .(subject_id, task), .SDcols = col]$m))
    if (!is.finite(mx) || mx > 1e-8) fatal("state_mean_reload", paste(col, "max abs cell mean", mx))
    log_line(col, " reload max abs cell mean ", sprintf("%.3e", mx))
  }

  md5_m1 <- unname(tools::md5sum(file.path(DIAG, "analysis_data_m1.csv")))
  md5_m2 <- unname(tools::md5sum(file.path(DIAG, "analysis_data_m2.csv")))
  md5_tepr <- unname(tools::md5sum(file.path(DIAG, "analysis_data_tepr.csv")))
  md5_m5 <- unname(tools::md5sum(file.path(DIAG, "analysis_data_m5.csv")))

  summaries <- list()
  r1 <- fit_ladder("M1", d_m1,
                   "diff ~ (catch + x_c) * effort_c * task_c",
                   c("catch", "x_c", "effort_c", "task_c"),
                   "probit", c("effort_c", "catch:effort_c", "x_c:effort_c"), md5_m1)
  summaries[["M1"]] <- summary_line("M1", r1$accepted, r1$step, d_m1, r1$ct,
                                    c("effort_c", "catch:effort_c", "x_c:effort_c"),
                                    r1$formula, r1$singular, r1$messages)
  run_all_fit("M1")

  r2 <- fit_ladder("M2", d_m2,
                   "grip_mean ~ effort_c * task_c",
                   "effort_c", "lmer", "effort_c", md5_m2)
  summaries[["M2"]] <- summary_line("M2", r2$accepted, r2$step, d_m2, r2$ct,
                                    "effort_c", r2$formula, r2$singular, r2$messages)

  r3 <- fit_ladder("M3", d_tepr,
                   "tepr_mean_465_765 ~ effort_c * task_c + catch + x_c",
                   c("effort_c", "x_c"), "lmer",
                   c("effort_c", "x_c", "catch"), md5_tepr)
  summaries[["M3"]] <- summary_line("M3", r3$accepted, r3$step, d_tepr, r3$ct,
                                    c("effort_c", "x_c", "catch"),
                                    r3$formula, r3$singular, r3$messages)

  r4a <- fit_ladder("M4a", d_tepr,
                    "diff ~ (catch + x_c) * (b1_state + effort_c + task_c) + b1_trait",
                    c("catch", "x_c", "b1_state"), "probit",
                    c("x_c:b1_state", "b1_state", "catch:b1_state"), md5_tepr)
  summaries[["M4a"]] <- summary_line("M4a", r4a$accepted, r4a$step, d_tepr, r4a$ct,
                                     c("x_c:b1_state", "b1_state", "catch:b1_state"),
                                     r4a$formula, r4a$singular, r4a$messages)
  run_all_fit("M4a")

  r4b <- fit_ladder("M4b", d_tepr,
                    "diff ~ (catch + x_c) * (tepr_state + effort_c + task_c) + tepr_trait",
                    c("catch", "x_c", "tepr_state"), "probit",
                    c("x_c:tepr_state", "tepr_state", "catch:tepr_state"), md5_tepr)
  summaries[["M4b"]] <- summary_line("M4b", r4b$accepted, r4b$step, d_tepr, r4b$ct,
                                     c("x_c:tepr_state", "tepr_state", "catch:tepr_state"),
                                     r4b$formula, r4b$singular, r4b$messages)
  run_all_fit("M4b")

  write_a4(d_tepr)

  r5 <- fit_specified("M5", d_m5,
                      "in_tepr_final ~ effort_c * task_c + catch + x_c + (1 | subject)",
                      "logit", "effort_c", md5_m5, "specified_intercept")
  summaries[["M5"]] <- summary_line("M5", r5$accepted, r5$step, d_m5, r5$ct,
                                    "effort_c", r5$formula, r5$singular, r5$messages)

  if (is.null(ACCEPTED[["M4b"]])) {
    add_deviation("S1 not fit: M4b has no accepted random-effects structure")
    summaries[["S1"]] <- "S1 | accepted=FALSE | step=not_fit | nobs=NA | participants=NA | singular=NA | convergence=NA | formula=NA"
  } else {
    f_s1 <- gsub("tepr_state", "teprauc_state", ACCEPTED[["M4b"]]$formula, fixed = TRUE)
    f_s1 <- gsub("tepr_trait", "teprauc_trait", f_s1, fixed = TRUE)
    if (grepl("tepr_state", f_s1, fixed = TRUE) || grepl("tepr_trait", f_s1, fixed = TRUE)) {
      fatal("S1_formula", f_s1)
    }
    if (!grepl("teprauc_state", f_s1, fixed = TRUE) || !grepl("teprauc_trait", f_s1, fixed = TRUE)) {
      fatal("S1_formula", f_s1)
    }
    r_s1 <- fit_s1_from_m4b(d_tepr, md5_tepr)
    summaries[["S1"]] <- summary_line("S1", r_s1$accepted, r_s1$step, d_tepr, r_s1$ct,
                                      c("x_c:teprauc_state", "teprauc_state", "catch:teprauc_state"),
                                      r_s1$formula, r_s1$singular, r_s1$messages)
  }

  if (is.null(ACCEPTED[["M1"]])) {
    add_deviation("S2 not fit: M1 has no accepted random-effects structure")
    summaries[["S2"]] <- "S2 | accepted=FALSE | step=not_fit | nobs=NA | participants=NA | singular=NA | convergence=NA | formula=NA"
  } else {
    ids <- unique(d_tepr$subject_id)
    d_s2 <- d_m1[subject_id %in% ids]
    d_s2[, subject := factor(subject_id)]
    log_line("S2 rows ", nrow(d_s2), " participants ", uniqueN(d_s2$subject_id))
    s2_path <- file.path(DIAG, "analysis_data_s2.csv")
    fwrite(d_s2, s2_path)
    md5_s2 <- unname(tools::md5sum(s2_path))
    r_s2 <- fit_specified("S2", d_s2, ACCEPTED[["M1"]]$formula, "probit",
                          c("effort_c", "catch:effort_c", "x_c:effort_c"),
                          md5_s2, paste0("same_as_M1_", ACCEPTED[["M1"]]$step))
    summaries[["S2"]] <- summary_line("S2", r_s2$accepted, r_s2$step, d_s2, r_s2$ct,
                                      c("effort_c", "catch:effort_c", "x_c:effort_c"),
                                      r_s2$formula, r_s2$singular, r_s2$messages)
  }

  if (exists("ch2_model_data", envir = .GlobalEnv, inherits = FALSE)) {
    rm(ch2_model_data, envir = .GlobalEnv)
  }
  m1_fit_check()
  write_report(unlist(summaries[c("M1", "M2", "M3", "M4a", "M4b", "M5", "S1", "S2")], use.names = FALSE))
}

log_line("script 02 start")
ran <- tryCatch({
  main()
  TRUE
}, error = function(e) {
  STATUS <<- 1L
  log_line("ERROR ", conditionMessage(e))
  if (!file.exists(file.path(DIAG, "STOPPED_2.md"))) {
    write_stop("2", "uncaught", conditionMessage(e), TRUE)
  }
  if (!file.exists(file.path(DIAG, "report.md"))) {
    writeLines(c("# Chapter 2 Phase 2b", "", conditionMessage(e), ""), file.path(DIAG, "report.md"))
  }
  FALSE
})
try(package_zip(), silent = FALSE)
cat(paste0("EXIT: ", STATUS, "\n"))
flush(stdout())
quit(save = "no", status = STATUS)
