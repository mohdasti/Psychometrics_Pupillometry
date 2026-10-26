# Chapter 2 wrapper: delegates to project-root subject-level coupling script.

script <- file.path(here::here(), "05_subject_level_analysis", "07_pf_pupil_subject_coupling.R")
if (!file.exists(script)) {
  stop("Missing coupling script: ", script)
}
source(script, local = TRUE)
