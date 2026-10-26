#!/usr/bin/env Rscript
# TOST equivalence forest plot for intensity x pupil-state interaction (Chapter 2 B2).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(here)
})

repo_root <- here::here()
source(file.path(repo_root, "config", "paths_config.R"))
source(file.path(repo_root, "R", "colors_manuscript.R"))
source(file.path(repo_root, "R", "tost_equivalence_helpers.R"))
source(file.path(repo_root, "R", "plot_equivalence_forest.R"))

dir.create(figures_dir, recursive = TRUE, showWarnings = TRUE)
dir.create(tables_dir, recursive = TRUE, showWarnings = TRUE)

tost_summary <- build_interaction_tost_summary(models_dir)
if (!nrow(tost_summary)) {
  stop("No GLMM model files found in ", models_dir)
}

write_csv(
  tost_summary,
  file.path(tables_dir, "pupil_psychometric_tost_equivalence.csv")
)

p <- plot_equivalence_forest(tost_summary)

out_png <- file.path(figures_dir, "fig_equivalence_forest.png")
out_pdf <- file.path(figures_dir, "fig_equivalence_forest.pdf")

ggsave(out_png, p, width = 8, height = 5.5, dpi = 300, bg = "white")
ggsave(out_pdf, p, width = 8, height = 5.5, bg = "white")

cat("Saved:", out_png, "\n")
cat("Saved:", out_pdf, "\n")
cat("Saved:", file.path(tables_dir, "pupil_psychometric_tost_equivalence.csv"), "\n")
print(
  tost_summary %>%
    transmute(
      Analysis = analysis_label,
      `beta (probit)` = estimate,
      `90% CI lower` = ci90_lower,
      `90% CI upper` = ci90_upper,
      `p (Wald)` = p_wald,
      `Inside +/- SESOI` = inside_sesoi,
      `TOST equivalent` = equivalent
    )
)
