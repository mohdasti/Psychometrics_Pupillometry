# Pupil trial axis for the BAP flat files (decision record D007, Phase 2a.1 evidence).
# Sample k is at -3 + 0.004 * (k - 1) s relative to squeeze onset. Squeeze onset is sample 751.
# The `time` column is not used as a clock: its first and last samples carry stale timestamps.
build_trial_time_250hz <- function(n, trial_median_dt, generation,
                                   seg_start = NA_real_, seg_end = NA_real_,
                                   subject = NA, task = NA, session = NA, run = NA, trial = NA) {
  id <- sprintf("%s %s ses %s run %s trial %s", subject, task, session, run, trial)
  if (!is.finite(trial_median_dt) || abs(trial_median_dt - 0.004) > 1e-6)
    return(list(t = NULL, rule = "refused", reason = sprintf("median dt %.6f != 0.004 (%s)", trial_median_dt, id)))
  if (generation == "dec21") {
    if (!n %in% c(3425L, 3426L))
      return(list(t = NULL, rule = "refused", reason = sprintf("n = %d not in {3425, 3426} (%s)", n, id)))
  } else if (generation == "dec25") {
    end_implied <- -3 + 0.004 * (n - 1)
    if (!is.finite(seg_start) || abs(seg_start + 3) > 1e-6 || !is.finite(seg_end) || abs(end_implied - seg_end) > 0.01)
      return(list(t = NULL, rule = "refused", reason = sprintf("dec25 segment [%s, %s] vs implied end %.4f (%s)", seg_start, seg_end, end_implied, id)))
  } else stop("unknown generation: ", generation)
  list(t = -3 + 0.004 * (seq_len(n) - 1L), rule = "fixed_250hz_from_m3", reason = NA_character_)
}
