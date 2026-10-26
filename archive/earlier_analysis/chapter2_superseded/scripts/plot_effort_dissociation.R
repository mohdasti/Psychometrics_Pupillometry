#!/usr/bin/env Rscript
# Effort effect on Total AUC vs Cognitive AUC (tonic / phasic dissociation).

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(here)
})

repo_root <- here::here()
source(file.path(repo_root, "config", "paths_config.R"))
source(file.path(repo_root, "R", "colors_manuscript.R"))

effort_effects <- tibble::tribble(
  ~metric,         ~descriptor,             ~beta,   ~se,    ~t,      ~p,
  "Total AUC",     "overall trial arousal",  0.857,  0.376,  2.282,   0.0230,
  "Cognitive AUC", "task-evoked response",  -0.012,  0.003, -3.859,   0.0005
) |>
  mutate(
    metric = factor(metric, levels = c("Cognitive AUC", "Total AUC")),
    p_label = ifelse(p < 0.001, "p < .001",
                     paste0("p = ", sub("^0", "", sprintf("%.3f", p)))),
    annot = paste0(sprintf("\u03b2 = %+.3f", beta), ", ", p_label),
    direction = ifelse(t > 0, "increase", "decrease")
  )

crit <- 1.96

p_fig <- ggplot(effort_effects, aes(x = t, y = metric, colour = direction)) +
  annotate("rect", xmin = -crit, xmax = crit, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.18) +
  annotate("text", x = 0, y = 0.42, label = "not significant (|t| < 1.96)",
           size = 3, colour = "grey35") +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey50") +
  geom_segment(aes(x = 0, xend = t, yend = metric), linewidth = 1.1) +
  geom_point(size = 3.8) +
  geom_text(aes(label = annot), vjust = -1.4, size = 3.1,
            colour = "grey20", show.legend = FALSE) +
  scale_colour_manual(values = c(increase = "#1D9E75", decrease = "#7F77DD"),
                      guide = "none") +
  scale_x_continuous(limits = c(-5, 3.5), breaks = seq(-4, 3, 1)) +
  scale_y_discrete(expand = expansion(add = c(0.7, 0.6))) +
  labs(x = "effort effect on pupil metric (t statistic)", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_ch2(12) +
  theme(
    axis.text.y = element_text(face = "bold", hjust = 1),
    plot.margin = margin(12, 18, 10, 12)
  )

dir.create(figures_dir, recursive = TRUE, showWarnings = TRUE)
out_png <- file.path(figures_dir, "fig_effort_dissociation.png")
ggsave(out_png, p_fig, width = 8.5, height = 3.2, dpi = 300, bg = "white")
message("Wrote ", out_png)
