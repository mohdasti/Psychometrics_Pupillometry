# Compute motor-buffered cognitive pupil metrics from flat files + RT.
# Output: augments ch2_triallevel_merged.csv with motor-buffer columns.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(here)
})

source(file.path(here(), "config", "paths_config.R"))
source(file.path(here(), "R", "motorbuffer_pupil_helpers.R"))

cat("=== Motor-buffered cognitive pupil metrics ===\n\n")

if (!file.exists(merged_trial_file)) {
  stop("Merged trial file not found: ", merged_trial_file)
}

merged <- read_csv(merged_trial_file, show_col_types = FALSE)
rt_lookup <- merged %>%
  transmute(
    sub = as.character(sub),
    task = as.character(task),
    session_used = as.integer(session_used),
    run_used = as.integer(run_used),
    trial_index = as.integer(trial_index),
    rt = as.numeric(rt),
    t_resp_start_rel = as.numeric(t_resp_start_rel)
  )

processed_dir <- resolve_pupil_processed_dir()
flat_files <- list.files(
  processed_dir,
  pattern = ".*_(ADT|VDT)_flat\\.csv$",
  full.names = TRUE
)
if (!length(flat_files)) {
  stop("No flat CSV files found in ", processed_dir)
}
cat("Flat files:", length(flat_files), "in", processed_dir, "\n")

motor_feats <- map_dfr(
  flat_files,
  ~ process_flat_file_motorbuffer(.x, rt_lookup = rt_lookup),
  .progress = "text"
)

cat("\nMotor-buffer feature rows:", nrow(motor_feats), "\n")
cat("Valid motor-buffer trials:", sum(motor_feats$cog_win_motorbuffer_valid, na.rm = TRUE), "\n")

out_file <- file.path(processed_dir, "ch2_motorbuffer_pupil.csv")
write_csv(motor_feats, out_file)
cat("Saved:", out_file, "\n")

join_keys <- c("sub", "task", "session_used", "run_used", "trial_index")
merged <- merged %>%
  select(-any_of(c(
    "cog_mean_motorbuffered", "cog_auc_motorbuffered",
    "cog_win_primary_end_motorbuffered", "cog_win_motorbuffer_duration",
    "cog_win_truncated_by_motor", "cog_win_motorbuffer_valid",
    "cog_win_motorbuffer_n_valid",
    "pupil_cognitive_trait_motorbuffer", "pupil_cognitive_state_motorbuffer"
  ))) %>%
  left_join(motor_feats, by = join_keys)

merged <- add_motorbuffer_state_trait(merged)

write_csv(merged, merged_trial_file)
cat("Updated merged trial file:", merged_trial_file, "\n")

qc <- merged %>%
  filter(quality_primary) %>%
  summarise(
    n_primary = n(),
    n_mb_valid = sum(cog_win_motorbuffer_valid, na.rm = TRUE),
    n_mb_glmm = sum(
      cog_win_motorbuffer_valid &
        !is.na(pupil_cognitive_state_motorbuffer) &
        !is.na(stimulus_intensity) &
        !is.na(choice_num),
      na.rm = TRUE
    ),
    pct_truncated = mean(cog_win_truncated_by_motor, na.rm = TRUE),
    median_duration = median(cog_win_motorbuffer_duration[cog_win_motorbuffer_valid], na.rm = TRUE)
  )
cat("\nPrimary-tier QC:\n")
print(qc)
cat("\n=== Done ===\n")
