#!/usr/bin/env Rscript
# Generate CONSORT-inspired participant / analytic-sample flow figure (Chapter 2 B1).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(here)
})

repo_root <- here::here()
options(ch2.repo_root = repo_root)

source(file.path(repo_root, "config", "paths_config.R"))
source(file.path(repo_root, "R", "colors_manuscript.R"))
source(file.path(repo_root, "R", "pf_analysis_helpers.R"))
source(file.path(repo_root, "R", "ch2_sample_helpers.R"))
source(file.path(repo_root, "R", "plot_participant_flow.R"))

dir.create(figures_dir, recursive = TRUE, showWarnings = TRUE)

dat <- read_csv(merged_trial_file, show_col_types = FALSE)
pf <- read_csv(pf_params_file, show_col_types = FALSE)

flow <- summarize_participant_flow(dat, pf)
cat("Participant flow summary:\n")
str(flow, max.level = 2)

p <- plot_participant_flow(flow, task_colors = task_colors)

out_png <- file.path(figures_dir, "participant_flow.png")
out_pdf <- file.path(figures_dir, "participant_flow.pdf")

ggplot2::ggsave(out_png, p, width = 8.5, height = 10.5, dpi = 300, bg = "white")
ggplot2::ggsave(out_pdf, p, width = 8.5, height = 10.5, bg = "white")

cat("Saved:", out_png, "\n")
cat("Saved:", out_pdf, "\n")
