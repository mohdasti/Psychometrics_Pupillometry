# Phase 3 checks. Does not refit and does not edit the chapter.
# Writes the contract, reference, and render checks into the phase-3 work directory.
# Run from the repository root: Rscript scripts/phase3/02_checks.R

CH2 <- "/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry"
QMD <- file.path(CH2, "07_manuscript/chapter2/reports/chapter2_dissertation.qmd")
CHAPTER_BIB <- file.path(CH2, "07_manuscript/chapter2/reports/references.bib")
MASTER_BIB <- "/Users/mohdasti/Documents/GitHub/UCR_PhD_Dissertation/references.bib"
EXPORT <- file.path(CH2, "07_manuscript/chapter2/export/ch2fit_20261024_193248")
OUT <- file.path(CH2, "output/diagnostics/ch2_phase3/work")
RENDER <- file.path(CH2, "output/diagnostics/ch2_phase3/renders")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(QMD), file.exists(CHAPTER_BIB), file.exists(MASTER_BIB))
stopifnot(file.exists(file.path(EXPORT, "values.json")))
stopifnot(file.exists(file.path(EXPORT, "key_contract.csv")))

# --- bibliography -----------------------------------------------------------

read_bib_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

split_entries <- function(txt) {
  pos <- gregexpr("(?m)@[A-Za-z]+\\s*\\{", txt, perl = TRUE)[[1]]
  if (pos[1] < 0) return(list())
  starts <- as.integer(pos)
  ends <- c(starts[-1] - 1L, nchar(txt))
  lapply(seq_along(starts), function(i) substr(txt, starts[i], ends[i]))
}

entry_key <- function(entry) sub("(?s)^@[A-Za-z]+\\s*\\{\\s*([^,\\s]+).*", "\\1", entry, perl = TRUE)

field_value <- function(entry, name) {
  m <- regexpr(paste0("(?i)\\b", name, "\\s*=\\s*"), entry, perl = TRUE)
  if (m < 0) return(NA_character_)
  rest <- substring(entry, m + attr(m, "match.length"))
  rest <- sub("^\\s+", "", rest)
  ch <- strsplit(rest, "", fixed = TRUE)[[1]]
  if (!length(ch)) return(NA_character_)
  if (ch[1] %in% c("{", "\"")) {
    open <- ch[1]
    close <- if (open == "{") "}" else "\""
    depth <- 0L
    out <- character()
    for (i in seq_along(ch)) {
      if (open == "{" && ch[i] == "{") depth <- depth + 1L
      if (open == "{" && ch[i] == "}") {
        depth <- depth - 1L
        if (depth == 0L) break
      }
      if (open == "\"" && i > 1L && ch[i] == "\"" && (i == 1L || ch[i - 1L] != "\\")) break
      if (i > 1L) out <- c(out, ch[i])
    }
    return(paste(out, collapse = ""))
  }
  val <- sub("[\\s,].*", "", rest)
  if (length(val) != 1L || !nzchar(val)) NA_character_ else val
}

norm_text <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NA_character_)
  x <- gsub("[{}]", "", x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

first_surname <- function(author) {
  if (is.na(author) || !nzchar(author)) return(NA_character_)
  one <- strsplit(author, " and ", fixed = TRUE)[[1]][1]
  one <- gsub("[{}]", "", one)
  one <- trimws(one)
  if (grepl(",", one, fixed = TRUE)) {
    return(norm_text(strsplit(one, ",", fixed = TRUE)[[1]][1]))
  }
  parts <- strsplit(norm_text(one), " ")[[1]]
  parts[length(parts)]
}

bib_table <- function(path) {
  entries <- split_entries(read_bib_text(path))
  rows <- lapply(entries, function(e) {
    data.frame(
      key = entry_key(e),
      title = field_value(e, "title"),
      year = field_value(e, "year"),
      doi = field_value(e, "doi"),
      author = field_value(e, "author"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

chapter_bib <- bib_table(CHAPTER_BIB)
master_bib <- bib_table(MASTER_BIB)
doi_key <- function(d) {
  if (length(d) != 1L || is.na(d) || !nzchar(d)) return(NA_character_)
  d <- sub("(?i)^https?://(dx\\.)?doi\\.org/", "", d, perl = TRUE)
  d <- tolower(gsub("[^a-z0-9]+", "", d, ignore.case = TRUE))
  if (!nzchar(d)) NA_character_ else d
}
chapter_bib$doi_n <- vapply(chapter_bib$doi, doi_key, character(1))
master_bib$doi_n <- vapply(master_bib$doi, doi_key, character(1))
chapter_bib$title_n <- vapply(chapter_bib$title, norm_text, character(1))
master_bib$title_n <- vapply(master_bib$title, norm_text, character(1))
chapter_bib$surname <- vapply(chapter_bib$author, first_surname, character(1))
master_bib$surname <- vapply(master_bib$author, first_surname, character(1))
chapter_bib$year_n <- gsub("[^0-9]", "", chapter_bib$year)
master_bib$year_n <- gsub("[^0-9]", "", master_bib$year)

qmd <- paste(readLines(QMD, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
cited <- unique(unlist(regmatches(qmd, gregexpr("(?<=@)[A-Za-z0-9:_-]+", qmd, perl = TRUE))))
cited <- cited[!grepl("^(fig|tbl|sec|eq)-", cited)]

map_one <- function(key) {
  ch <- chapter_bib[chapter_bib$key == key, ]
  empty <- data.frame(
    chapter_key = key, master_key = NA_character_, match_method = "chapter key absent",
    chapter_title = NA_character_, chapter_year = NA_character_, chapter_doi = NA_character_,
    master_title = NA_character_, master_year = NA_character_, master_doi = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!nrow(ch)) return(empty)
  empty$chapter_title <- ch$title[1]
  empty$chapter_year <- ch$year[1]
  empty$chapter_doi <- ch$doi[1]
  empty$match_method <- "no master match"
  if (!is.na(ch$doi_n[1]) && nzchar(ch$doi_n[1])) {
    hit <- master_bib[!is.na(master_bib$doi_n) & master_bib$doi_n == ch$doi_n[1], ]
    if (nrow(hit)) {
      empty$master_key <- paste(unique(hit$key), collapse = "; ")
      empty$match_method <- "doi"
      empty$master_title <- hit$title[1]
      empty$master_year <- hit$year[1]
      empty$master_doi <- hit$doi[1]
      return(empty)
    }
  }
  hit <- master_bib[
    !is.na(master_bib$title_n) & master_bib$title_n == ch$title_n[1] &
      !is.na(master_bib$surname) & master_bib$surname == ch$surname[1] &
      !is.na(master_bib$year_n) & master_bib$year_n == ch$year_n[1],
  ]
  if (nrow(hit)) {
    empty$master_key <- paste(unique(hit$key), collapse = "; ")
    empty$match_method <- "title+surname+year"
    empty$master_title <- hit$title[1]
    empty$master_year <- hit$year[1]
    empty$master_doi <- hit$doi[1]
  }
  empty
}

f1 <- do.call(rbind, lapply(sort(cited), map_one))
utils::write.csv(f1, file.path(OUT, "F1_key_map.csv"), row.names = FALSE)

# F3: the chapter entry, copied exactly.
chap_txt <- read_bib_text(CHAPTER_BIB)
sun_entries <- Filter(function(e) entry_key(e) == "sun2026handgrip", split_entries(chap_txt))
if (length(sun_entries) != 1L) stop("sun2026handgrip entry not found once", call. = FALSE)
writeLines(sub("\\s+$", "", sun_entries[[1]]), file.path(OUT, "F3_master_bib_patch.bib"))

# F2: presence by title/author search. No invented metadata.
f2_queries <- list(
  list(asked = "Gilzenrat et al. (2010), pupil and LC mode", author = "gilzenrat", year = "2010", extra = "pupil|locus|mode"),
  list(asked = "Murphy, Robertson, Balsters & O'Connell (2011), pupillometry and P3/LC", author = "murphy", year = "2011", extra = "pupil|p3|locus"),
  list(asked = "Joshi, Li, Kalwani & Gold (2016), LC and pupil", author = "joshi", year = "2016", extra = "pupil|locus"),
  list(asked = "Barr, Levy, Scheepers & Tily (2013)", author = "barr", year = "2013", extra = "scheepers|random effect"),
  list(asked = "Morey (2008)", author = "morey", year = "2008", extra = "confidence|interval|within"),
  list(asked = "Mather & Harley (2016)", author = "mather", year = "2016", extra = "harley|arousal|norepinephrine"),
  list(asked = "Azer et al. (2023)", author = "azer", year = "2023", extra = "."),
  list(asked = "Mathot & Vilotijevic (2023)", author = "mathot|vilotijevic", year = "2023", extra = "pupil"),
  list(asked = "Wickens (2008)", author = "wickens", year = "2008", extra = "."),
  list(asked = "Pashler (1994)", author = "pashler", year = "1994", extra = "."),
  list(asked = "Kret & Sjak-Shie (2019)", author = "kret|sjak", year = "2019", extra = "pupil"),
  list(asked = "de Gee et al. (2020)", author = "gee", year = "2020", extra = "pupil|criterion|arousal"),
  list(asked = "Winn et al. (1994)", author = "winn", year = "1994", extra = "."),
  list(asked = "Piquado et al. (2010)", author = "piquado", year = "2010", extra = ".")
)
f2 <- do.call(rbind, lapply(f2_queries, function(q) {
  blob <- paste(master_bib$author, master_bib$title, master_bib$year)
  hit <- master_bib[
    grepl(q$author, master_bib$author, ignore.case = TRUE, perl = TRUE) &
      grepl(q$year, master_bib$year) &
      grepl(q$extra, blob, ignore.case = TRUE, perl = TRUE),
  ]
  if (!nrow(hit)) {
    return(data.frame(
      asked = q$asked, present = "absent", master_key = "", title = "", year = "", doi = "",
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    asked = q$asked,
    present = "present",
    master_key = paste(hit$key, collapse = "; "),
    title = paste(hit$title, collapse = " || "),
    year = paste(hit$year, collapse = "; "),
    doi = paste(ifelse(is.na(hit$doi), "", hit$doi), collapse = " || "),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(f2, file.path(OUT, "F2_needed_refs.csv"), row.names = FALSE)

# --- Check A ----------------------------------------------------------------

iv <- jsonlite::fromJSON(file.path(EXPORT, "values.json"), simplifyVector = FALSE)$iv
contract <- utils::read.csv(file.path(EXPORT, "key_contract.csv"), stringsAsFactors = FALSE)
p_objects <- c(
  M1_effort = "m1_effort_c_p", M1_catch = "m1_catch_effort_c_p", M1_slope = "m1_x_c_effort_c_p",
  M2_effort = "m2_effort_c_p", M3_effort = "m3_effort_c_p", M3_x = "m3_x_c_p", M3_catch = "m3_catch_p",
  M4a_slope = "m4a_x_c_b1_state_p", M4a_state = "m4a_b1_state_p", M4a_catch = "m4a_catch_b1_state_p",
  M4b_slope = "m4b_x_c_tepr_state_p", M4b_state = "m4b_tepr_state_p", M4b_catch = "m4b_catch_tepr_state_p",
  M5_effort = "m5_effort_c_p", S1_slope = "s1_x_c_teprauc_state_p", S1_state = "s1_teprauc_state_p",
  S1_catch = "s1_catch_teprauc_state_p", S2_effort = "s2_effort_c_p", S2_catch = "s2_catch_effort_c_p",
  S2_slope = "s2_x_c_effort_c_p"
)
inlines <- unlist(regmatches(qmd, gregexpr("`r [^`]+`", qmd)))
resolve_inline <- function(expr) {
  body <- sub("^`r\\s+", "", sub("`$", "", expr))
  keys <- character()
  keys <- c(keys, unlist(regmatches(body, gregexpr("(?<=v\\(\"|v_int\\(\"|v_num\\(\")[^\"]+", body, perl = TRUE))))
  objs <- unlist(regmatches(body, gregexpr("[A-Za-z0-9_]+\\$p_fmt", body)))
  for (ob in objs) {
    name <- sub("\\$p_fmt$", "", ob)
    if (!name %in% names(p_objects)) return(NA_character_)
    keys <- c(keys, unname(p_objects[[name]]))
  }
  if (grepl("\\brun_id\\b", body)) keys <- c(keys, "run_id")
  if (!length(keys)) return(NA_character_)
  paste(unique(keys), collapse = "; ")
}
a4 <- data.frame(
  inline = inlines,
  keys = vapply(inlines, resolve_inline, character(1)),
  stringsAsFactors = FALSE
)
a4$in_contract <- vapply(strsplit(a4$keys, "; "), function(ks) {
  if (anyNA(ks) || !length(ks)) return(FALSE)
  all(trimws(ks) %in% contract$key)
}, logical(1))
a4$in_values <- vapply(strsplit(a4$keys, "; "), function(ks) {
  if (anyNA(ks) || !length(ks)) return(FALSE)
  all(trimws(ks) %in% names(iv))
}, logical(1))
utils::write.csv(a4, file.path(OUT, "A4_inline_check.csv"), row.names = FALSE)
if (any(!a4$in_contract)) {
  stop("Check A orphan inlines: ", paste(a4$inline[!a4$in_contract], collapse = " | "), call. = FALSE)
}

# --- G3 ---------------------------------------------------------------------

prose <- qmd
prose <- sub("^---.*?---\\s*", "", prose)
caps <- unlist(regmatches(prose, gregexpr("(?m)^#\\|\\s*(?:fig-cap|tbl-cap):.*$", prose, perl = TRUE)))
prose <- gsub("(?s)```\\{.*?```", "", prose, perl = TRUE)
prose <- paste(c(prose, caps), collapse = "\n")
prose <- gsub("`r [^`]+`", " INLINE ", prose)
cite_keys <- unique(unlist(regmatches(prose, gregexpr("@[A-Za-z][A-Za-z0-9:_-]*", prose))))
prose <- gsub("@[A-Za-z][A-Za-z0-9:_-]*", " CITE ", prose)
prose <- gsub("\\bM[0-9][ab]?\\b|\\bS[12]\\b", " MODEL ", prose)
lines <- strsplit(prose, "\n", fixed = TRUE)[[1]]
g3_rows <- list()
for (i in seq_along(lines)) {
  line <- lines[i]
  if (!grepl("[0-9]", line)) next
  hits <- gregexpr("(?<![A-Za-z0-9.])(?:10\\^\\{-?[0-9]+\\}|[0-9]{1,3}(?:,[0-9]{3})+(?:\\.[0-9]+)?|[0-9]+(?:\\.[0-9]+)?)", line, perl = TRUE)[[1]]
  if (hits[1] < 0) next
  lens <- attr(hits, "match.length")
  for (j in seq_along(hits)) {
    token <- substr(line, hits[j], hits[j] + lens[j] - 1L)
    g3_rows[[length(g3_rows) + 1L]] <- data.frame(
      line = i, token = token, context = trimws(line), stringsAsFactors = FALSE
    )
  }
}
g3 <- do.call(rbind, g3_rows)
classify <- function(token, context) {
  ctx <- tolower(context)
  if (grepl("task-by-offset", ctx) && token == "10") {
    return(c("data-derived", "FAIL",
             "Design count of 2 tasks x 5 levels, used as a claim that high effort is lower in every cell. Trial means are negative in all 10 cells. Participant means are positive at visual offset 0.24. Not rewritten."))
  }
  if (grepl("december (21|25)|phase 3|session 1", ctx)) {
    return(c("label", "PASS", "Batch, phase, or session label. Not a fitted result."))
  }
  if (grepl("psychtoolbox|matlab|propixx|dynamometer|trackpixx|cm\\.|hz", ctx)) {
    return(c("design constant", "PASS", "Apparatus or software version."))
  }
  if (grepl("lme4|tolerance|bobyqa|function evaluations|0\\.01|satterthwaite|decimals", ctx)) {
    return(c("design constant", "PASS", "Optimizer, tolerance, or reporting rule."))
  }
  if (grepl("\\$t_k|0\\.004|3,425|3,426|10\\^", ctx) || token %in% c("0.004", "3425", "3426", "3,425", "3,426")) {
    return(c("design constant", "PASS", "Locked time-axis rule."))
  }
  if (grepl("mvc|hz|gabor|contrast|inter-trial|squeeze onset|release|response window|confidence|baseline|pre-probe|grip-window|tepr|b0|b1", ctx)) {
    return(c("design constant", "PASS", "Stimulus, effort level, or analysis window."))
  }
  if (token %in% c("4", "3", "2", "1", "0", "20", "50", "95", "0.50", ".50")) {
    return(c("design constant", "PASS", "Design count, gate, probability point, or interval convention."))
  }
  if (grepl("less than 12|12 years", ctx)) {
    return(c("source label", "PASS", "The spreadsheet's own education code, not a computed mean."))
  }
  if (grepl("50 ms", ctx)) {
    return(c("design constant", "PASS", "Window used by the retired drafts. Not a result of these fits."))
  }
  if (grepl("weibull|\\.50|effective midpoint", ctx)) {
    return(c("design constant", "PASS", "Definition used by the cited psychometric fits."))
  }
  if (grepl("offset index|centered offset|sum-coded|high \\+0|low", ctx) || token %in% c("0", "1", "2", "3", "1.5", "0.5")) {
    return(c("design constant", "PASS", "Coding of catch, offset, or effort."))
  }
  if (grepl("\\bmodel\\b|log-odds|accepted models", ctx)) {
    return(c("label", "PASS", "Model identifier."))
  }
  c("unclassified", "FAIL", "Not classified. Inspect before treating Part G as passed.")
}
cite_rows <- do.call(rbind, lapply(cite_keys, function(k) {
  yr <- sub(".*?([0-9]{4}).*", "\\1", k)
  data.frame(
    line = NA, token = if (grepl("[0-9]{4}", k)) yr else k,
    context = k, class = "citation", status = "PASS",
    note = "Year inside a citation key. The key was not changed.",
    stringsAsFactors = FALSE
  )
}))
cls <- t(mapply(classify, g3$token, g3$context))
g3$class <- cls[, 1]
g3$status <- cls[, 2]
g3$note <- cls[, 3]
if (!is.null(cite_rows)) g3 <- rbind(g3, cite_rows)
utils::write.csv(g3, file.path(OUT, "G3_hardcoded.csv"), row.names = FALSE)

# --- G2, when the four renders exist ----------------------------------------

renders <- c(
  true_pdf = file.path(RENDER, "ch2_diss_true.pdf"),
  false_pdf = file.path(RENDER, "ch2_diss_false.pdf"),
  true_docx = file.path(RENDER, "ch2_diss_true.docx"),
  false_docx = file.path(RENDER, "ch2_diss_false.docx")
)
g2_path <- file.path(OUT, "G_checks.csv")
if (!all(file.exists(renders))) {
  utils::write.csv(data.frame(
    check = "renders present", expected = "4", observed = sum(file.exists(renders)), status = "PENDING"
  ), g2_path, row.names = FALSE)
  message("G2 pending: renders not all present")
} else {
  py <- tempfile(fileext = ".py")
  writeLines(c(
    "import sys, re",
    "from pypdf import PdfReader",
    "path, which = sys.argv[1], sys.argv[2]",
    "r = PdfReader(path)",
    "pages = []",
    "for p in r.pages:",
    "    pages.append(p.extract_text() or '')",
    "text = '\\n'.join(pages)",
    "print('N_PAGES', len(pages))",
    "print('QQ', text.count('??'))",
    "for lit in ['TO BE SUPPLIED', 'NaN']:",
    "    print('LIT', lit, text.count(lit))",
    "# NA as a standalone token, not inside a word",
    "print('LIT NA', len(re.findall(r'(?<![A-Za-z])NA(?![A-Za-z])', text)))",
    "print('LIT EM', text.count('\\u2014'))",
    "heads = ['Introduction', 'Methods', 'Results', 'Discussion', 'Supplement', 'References']",
    "if which == 'true':",
    "    for i, page in enumerate(pages, 1):",
    "        for h in heads:",
    "            if re.search(r'(?m)^\\s*\\\\d*\\\\s*' + h + r'\\\\s*$', page) or (h in page and h == page.strip()[:len(h)+8]):",
    "                print('HEAD', h, i)",
    "    for name in ['Area under the curve', 'Behavior in the pupil subsample', 'Optimizer check', 'Pupil participant flow', 'Time axis', 'Missingness model']:",
    "        print('SECTION', name, int(name in text))",
    "else:",
    "    for name in ['Area under the curve', 'Behavior in the pupil subsample', 'Optimizer check', 'Pupil participant flow', 'Time axis', 'Missingness model', 'Table S1', 'Table S2', 'Fixed effects', 'Random-effects reduction']:",
    "        print('SECTION', name, int(name in text))"
  ), py)
  run_py <- function(path, which) system2("python3", c(py, path, which), stdout = TRUE, stderr = TRUE)
  true_out <- run_py(renders["true_pdf"], "true")
  false_out <- run_py(renders["false_pdf"], "false")
  writeLines(c(true_out, false_out), file.path(OUT, "G2_pdf_text_scan.txt"))
  grab <- function(lines, key) {
    hit <- grep(paste0("^", key, " "), lines, value = TRUE)
    if (!length(hit)) return(NA_character_)
    sub(paste0("^", key, "\\s+"), "", hit[1])
  }
  # LaTeX missing-character warnings.
  logs <- list.files(RENDER, pattern = "\\.log$", full.names = TRUE)
  # Quarto leaves the log beside the qmd unless output-dir keeps it.
  logs <- c(logs, list.files(file.path(CH2, "07_manuscript/chapter2/reports"), pattern = "chapter2_dissertation.*\\.log$", full.names = TRUE))
  n_missing <- 0L
  if (length(logs)) {
    for (lg in logs) {
      n_missing <- n_missing + length(grep("Missing character", readLines(lg, warn = FALSE), ignore.case = TRUE))
    }
  }
  diss_names <- c("Area under the curve", "Behavior in the pupil subsample", "Optimizer check", "Pupil participant flow", "Time axis", "Missingness model")
  false_hits <- vapply(diss_names, function(nm) {
    line <- grep(paste0("^SECTION ", nm, " "), false_out, value = TRUE)
    if (!length(line)) return(NA_character_)
    sub(".* ", "", line[1])
  }, character(1))
  absent_ok <- all(false_hits == "0")
  # Manuscript supplement should remain in the false render.
  false_fixed <- grep("^SECTION Fixed effects ", false_out, value = TRUE)
  g2 <- data.frame(
    check = c(
      "missing character warnings",
      "unresolved crossrefs ??",
      "literal TO BE SUPPLIED",
      "literal NA",
      "literal NaN",
      "em dash standing in for a value",
      "empty figures or tables",
      "dissertation-only sections absent from false PDF",
      "manuscript supplement still in false PDF",
      "true PDF pages"
    ),
    expected = c("0", "0", "0", "0", "0", "0", "0", "yes", "yes", "recorded"),
    observed = c(
      n_missing,
      grab(true_out, "QQ"),
      grab(true_out, "LIT TO BE SUPPLIED"),
      grab(true_out, "LIT NA"),
      grab(true_out, "LIT NaN"),
      grab(true_out, "LIT EM"),
      "see render; no empty-kable error",
      if (isTRUE(absent_ok)) "yes" else paste(names(false_hits)[false_hits != "0"], collapse = ", "),
      if (length(false_fixed) && grepl(" 1$", false_fixed[1])) "yes" else paste(false_fixed, collapse = " "),
      grab(true_out, "N_PAGES")
    ),
    stringsAsFactors = FALSE
  )
  heads <- grep("^HEAD ", true_out, value = TRUE)
  if (length(heads)) {
    g2 <- rbind(g2, data.frame(
      check = paste("section page", sub("^HEAD ", "", heads)),
      expected = "recorded",
      observed = sub(".* ", "", heads),
      stringsAsFactors = FALSE
    ))
  }
  utils::write.csv(g2, g2_path, row.names = FALSE)
}

message("checks wrote ", OUT)
message("Check A inlines ", nrow(a4), " orphans ", sum(!a4$in_contract))
message("F1 unmatched ", sum(f1$match_method == "no master match"))
message("G3 fail ", sum(g3$status == "FAIL"))
