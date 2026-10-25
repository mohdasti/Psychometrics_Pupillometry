# Chapter 2 report helpers.
# Adapted from modeling-pupil-DDM/R/ch3_report_helpers.R.
# Formatters return character. The minus sign is Unicode U+2212.

.minus <- "\u2212"

.stop_value <- function(...) stop(paste0(...), call. = FALSE)

# v() errors on a missing, NULL, or NA key. `iv` is the flat list in values.json.
v <- function(key) {
  if (!is.character(key) || length(key) != 1L || !nzchar(key)) {
    .stop_value("v() expects a single key name")
  }
  if (!exists("iv", inherits = TRUE)) .stop_value("iv is not loaded")
  if (!key %in% names(iv)) .stop_value("inline value key not found: ", key)
  val <- iv[[key]]
  if (is.null(val) || (is.atomic(val) && anyNA(val))) {
    .stop_value("inline value is NULL or NA: ", key)
  }
  val
}

fmt_num <- function(x, d) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) .stop_value("fmt_num expects one number")
  s <- sprintf(paste0("%.", as.integer(d), "f"), x)
  if (!is.na(suppressWarnings(as.numeric(s))) && as.numeric(s) == 0) {
    s <- sub("^-", "", s)
  } else {
    s <- sub("-", .minus, s, fixed = TRUE)
  }
  s
}

fmt_int <- function(x) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) .stop_value("fmt_int expects one number")
  rounded <- round(x)
  sign <- if (rounded < 0) .minus else ""
  paste0(sign, format(abs(rounded), big.mark = ",", scientific = FALSE, trim = TRUE))
}

v_int <- function(key) fmt_int(as.numeric(v(key)))

.words_0_19 <- c(
  "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
  "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
  "seventeen", "eighteen", "nineteen"
)
.words_tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

# English words for an integer from 0 to 99. Errors outside that range.
v_words <- function(key, capitalize = FALSE) {
  x <- as.numeric(v(key))
  if (length(x) != 1L || is.na(x) || abs(x - round(x)) > 1e-8 || x < 0 || x > 99) {
    .stop_value("v_words expects an integer from 0 to 99: ", key)
  }
  n <- as.integer(round(x))
  if (n < 20L) {
    word <- .words_0_19[n + 1L]
  } else {
    word <- .words_tens[n %/% 10L - 1L]
    if (n %% 10L != 0L) word <- paste0(word, "-", .words_0_19[n %% 10L + 1L])
  }
  if (isTRUE(capitalize)) {
    word <- paste0(toupper(substr(word, 1L, 1L)), substr(word, 2L, nchar(word)))
  }
  word
}

v_num <- function(key) {
  val <- v(key)
  if (is.character(val)) return(val)
  fmt_num(as.numeric(val), 7)
}

# APA proportion. No leading zero, because the value cannot exceed 1.
# The minus sign is Unicode U+2212.
fmt_prop <- function(x, d) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) .stop_value("fmt_prop expects one number")
  if (abs(x) > 1) .stop_value("fmt_prop expects a value that cannot exceed 1")
  sub("0.", ".", fmt_num(x, d), fixed = TRUE)
}

# APA p. The relation sign is part of the string: "= .044" or "< .001".
fmt_p <- function(p) {
  if (length(p) != 1L || !is.numeric(p) || is.na(p)) .stop_value("fmt_p expects one probability")
  if (p < .001) return("< .001")
  paste0("= ", sub("^0", "", sprintf("%.3f", p)))
}

# 2 decimals for probit, logit, and pupil; 3 where |x| < .01.
fmt_est <- function(x) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) .stop_value("fmt_est expects one number")
  d <- if (abs(x) > 0 && abs(x) < 0.01) 3L else 2L
  fmt_num(x, d)
}

fmt_ci <- function(lo, hi) {
  paste0("[", fmt_est(lo), ", ", fmt_est(hi), "]")
}
