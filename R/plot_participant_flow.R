# CONSORT-inspired participant / analytic-sample flow diagram (Chapter 2).

flow_box <- function(id, cx, cy, w, h, label, fill, border, size = 3.1) {
  tibble::tibble(
    id = id, x = cx, y = cy, w = w, h = h,
    label = label, fill = fill, border = border, size = size
  )
}

flow_edge <- function(from_id, to_id, boxes, style = "straight") {
  f <- boxes[boxes$id == from_id, , drop = FALSE]
  t <- boxes[boxes$id == to_id, , drop = FALSE]
  if (!nrow(f) || !nrow(t)) {
    return(NULL)
  }
  tibble::tibble(
    from_id = from_id,
    to_id = to_id,
    style = style,
    x = f$x[1],
    y = f$y[1] - f$h[1] / 2,
    xend = t$x[1],
    yend = t$y[1] + t$h[1] / 2
  )
}

flow_edges_for <- function(pairs, boxes) {
  out <- lapply(
    seq_len(nrow(pairs)),
    function(i) {
      flow_edge(pairs$from[i], pairs$to[i], boxes, pairs$style[i])
    }
  )
  dplyr::bind_rows(out)
}

draw_flow_edges <- function(edges) {
  if (!nrow(edges)) {
    return(list())
  }
  straight <- edges[edges$style == "straight", , drop = FALSE]
  merge <- edges[edges$style == "merge", , drop = FALSE]

  layers <- list()
  if (nrow(straight)) {
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = straight,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "#6B7280",
      linewidth = 0.45,
      arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")
    )
  }
  if (nrow(merge)) {
    # Route merge arrows through a shared mid-y to avoid crossing boxes.
    merge <- merge %>%
      dplyr::mutate(
        y_mid = y - 0.35,
        x_mid = xend
      )
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = merge,
      ggplot2::aes(x = x, y = y, xend = x, yend = y_mid),
      colour = "#6B7280",
      linewidth = 0.45
    )
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = merge,
      ggplot2::aes(x = x, y = y_mid, xend = x_mid, yend = y_mid),
      colour = "#6B7280",
      linewidth = 0.45
    )
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = merge,
      ggplot2::aes(x = x_mid, y = y_mid, xend = xend, yend = yend),
      colour = "#6B7280",
      linewidth = 0.45,
      arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")
    )
  }
  layers
}

plot_participant_flow <- function(flow, task_colors = NULL) {
  if (is.null(task_colors)) {
    if (!exists("task_colors", envir = .GlobalEnv)) {
      source(file.path(getOption("ch2.repo_root", "."), "R", "colors_manuscript.R"), local = TRUE)
    } else {
      task_colors <- get("task_colors", envir = .GlobalEnv)
    }
  }

  adt_col <- unname(task_colors["ADT"])
  vdt_col <- unname(task_colors["VDT"])
  excl_fill <- "#F3F4F6"
  excl_border <- "#9CA3AF"
  pf_fill <- "#EEF6FF"
  pupil_fill <- "#F4F0FA"
  pooled_fill <- "#FFFBEB"
  text_col <- "#1F2937"

  trial_n <- function(task) {
    row <- flow$primary_trials[flow$primary_trials$task == task, , drop = FALSE]
    if (!nrow(row)) {
      return(NA_integer_)
    }
    as.integer(row$n_trials[1])
  }

  glmm_task <- function(task) {
    if (is.null(flow$glmm) || is.null(flow$glmm$by_task)) {
      return(list(n_sub = NA_integer_, n_trials = NA_integer_))
    }
    row <- flow$glmm$by_task[flow$glmm$by_task$task == task, , drop = FALSE]
    if (!nrow(row)) {
      return(list(n_sub = NA_integer_, n_trials = NA_integer_))
    }
    list(n_sub = as.integer(row$n_sub[1]), n_trials = as.integer(row$n_trials[1]))
  }

  coupling_n <- function(task) {
    if (is.null(flow$coupling) || !nrow(flow$coupling)) {
      return(NA_integer_)
    }
    row <- flow$coupling[flow$coupling$task == task, , drop = FALSE]
    if (!nrow(row)) {
      return(NA_integer_)
    }
    as.integer(row$n_sub[1])
  }

  # Layout: parallel PF (left) and pupil-QC (right) sub-columns per modality.
  adt_pf_x <- 0.75
  adt_pup_x <- 1.75
  vdt_pf_x <- 2.75
  vdt_pup_x <- 3.75
  bw <- 0.82
  bh_sm <- 0.52
  bh_md <- 0.62
  bh_lg <- 0.72

  y_rec <- 10.0
  y_excl <- 8.35
  y_valid <- 6.55
  y_trials <- 4.55
  y_glmm <- 2.55
  y_couple <- 0.85

  adt_center <- (adt_pf_x + adt_pup_x) / 2
  vdt_center <- (vdt_pf_x + vdt_pup_x) / 2
  pooled_x <- (adt_center + vdt_center) / 2

  vdt_pf_label <- if (flow$pf_threshold_excluded["VDT"] > 0L) {
    sprintf("PF-valid\nN = %s", format_flow_n(flow$pf_valid_both_effort["VDT"]))
  } else {
    sprintf("PF-valid\nN = %s\n(no exclusions)", format_flow_n(flow$pf_valid_both_effort["VDT"]))
  }

  boxes <- dplyr::bind_rows(
    flow_box(
      "adt_rec", adt_center, y_rec, 1.35, bh_sm,
      sprintf("ADT recruited\nN = %s", format_flow_n(flow$recruited["ADT"])),
      adt_col, adt_col, 3.35
    ),
    flow_box(
      "vdt_rec", vdt_center, y_rec, 1.35, bh_sm,
      sprintf("VDT recruited\nN = %s", format_flow_n(flow$recruited["VDT"])),
      vdt_col, vdt_col, 3.35
    ),
    flow_box(
      "adt_pf_ex", adt_pf_x, y_excl, bw, bh_lg,
      sprintf(
        "Excluded (PF)\nn = %s\n(threshold > 64 Hz)",
        format_flow_n(flow$pf_threshold_excluded["ADT"])
      ),
      excl_fill, excl_border, 2.85
    ),
    flow_box(
      "adt_pup_ex", adt_pup_x, y_excl, bw, bh_lg,
      sprintf(
        "Excluded (pupil QC)\nn = %s\n(%s PF + %s track loss/QC)",
        format_flow_n(flow$pupil_qc_excluded["ADT"]),
        format_flow_n(flow$pf_threshold_excluded["ADT"]),
        format_flow_n(flow$pupil_qc_only_excluded["ADT"])
      ),
      excl_fill, excl_border, 2.85
    ),
    flow_box(
      "vdt_pup_ex", vdt_pup_x, y_excl, bw, bh_lg,
      sprintf(
        "Excluded (pupil QC)\nn = %s\n(track loss/QC)",
        format_flow_n(flow$pupil_qc_excluded["VDT"])
      ),
      excl_fill, excl_border, 2.85
    ),
    flow_box(
      "adt_pf", adt_pf_x, y_valid, bw, bh_sm,
      sprintf("PF-valid\nN = %s", format_flow_n(flow$pf_valid_both_effort["ADT"])),
      pf_fill, adt_col, 3.05
    ),
    flow_box(
      "adt_pup", adt_pup_x, y_valid, bw, bh_sm,
      sprintf("Pupil-QC final\nN = %s", format_flow_n(flow$pupil_qc_final["ADT"])),
      pupil_fill, adt_col, 3.05
    ),
    flow_box(
      "vdt_pf", vdt_pf_x, y_valid, bw, bh_sm,
      vdt_pf_label,
      pf_fill, vdt_col, 2.95
    ),
    flow_box(
      "vdt_pup", vdt_pup_x, y_valid, bw, bh_sm,
      sprintf("Pupil-QC final\nN = %s", format_flow_n(flow$pupil_qc_final["VDT"])),
      pupil_fill, vdt_col, 3.05
    ),
    flow_box(
      "adt_trials", adt_pup_x, y_trials, bw, bh_md,
      sprintf(
        "Primary-tier trials\nn = %s\n(%s participants)",
        format_flow_n(trial_n("ADT")),
        format_flow_n(glmm_task("ADT")$n_sub)
      ),
      pupil_fill, adt_col, 2.95
    ),
    flow_box(
      "vdt_trials", vdt_pup_x, y_trials, bw, bh_md,
      sprintf(
        "Primary-tier trials\nn = %s\n(%s participants)",
        format_flow_n(trial_n("VDT")),
        format_flow_n(glmm_task("VDT")$n_sub)
      ),
      pupil_fill, vdt_col, 2.95
    ),
    flow_box(
      "glmm_pooled", pooled_x, y_glmm, 1.65, bh_lg,
      sprintf(
        "Pooled primary GLMM\nN = %s participants\nn = %s trials",
        format_flow_n(flow$glmm$n_sub),
        format_flow_n(flow$glmm$n_trials)
      ),
      pooled_fill, "#B45309", 3.1
    ),
    flow_box(
      "coupling", pooled_x, y_couple, 1.65, bh_lg,
      sprintf(
        "Subject-level coupling\n(PF-valid & pupil-QC, both effort)\nADT N = %s  |  VDT N = %s",
        format_flow_n(coupling_n("ADT")),
        format_flow_n(coupling_n("VDT"))
      ),
      pooled_fill, "#B45309", 3.0
    )
  )

  edge_pairs <- tibble::tibble(
    from = c(
      "adt_rec", "adt_rec",
      "adt_pf_ex", "adt_pup_ex",
      "adt_pup",
      "vdt_rec", "vdt_rec", "vdt_pup_ex",
      "adt_trials", "vdt_pup", "vdt_trials",
      "glmm_pooled"
    ),
    to = c(
      "adt_pf_ex", "adt_pup_ex",
      "adt_pf", "adt_pup",
      "adt_trials",
      "vdt_pf", "vdt_pup_ex", "vdt_pup",
      "glmm_pooled", "vdt_trials", "glmm_pooled",
      "coupling"
    ),
    style = c(
      "straight", "straight",
      "straight", "straight",
      "straight",
      "straight", "straight", "straight",
      "merge", "straight", "merge",
      "straight"
    )
  )

  edges <- flow_edges_for(edge_pairs, boxes)

  stream_labels <- tibble::tibble(
    x = c(adt_pf_x, adt_pup_x, vdt_pf_x, vdt_pup_x, pooled_x),
    y = c(y_excl + 0.95, y_excl + 0.95, y_excl + 0.95, y_excl + 0.95, y_trials - 0.95),
    label = c(
      "PF stream", "Pupil-QC stream",
      "PF stream", "Pupil-QC stream",
      "Trial-level analyses"
    )
  )

  column_headers <- tibble::tibble(
    x = c(adt_center, vdt_center),
    y = c(y_rec + 0.72, y_rec + 0.72),
    label = c("Auditory (ADT)", "Visual (VDT)")
  )

  edge_layers <- draw_flow_edges(edges)

  p <- ggplot2::ggplot()
  for (layer in edge_layers) {
    p <- p + layer
  }
  p +
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(
        xmin = x - w / 2,
        xmax = x + w / 2,
        ymin = y - h / 2,
        ymax = y + h / 2,
        fill = fill,
        colour = border
      ),
      linewidth = 0.55
    ) +
    ggplot2::geom_text(
      data = boxes,
      ggplot2::aes(x = x, y = y, label = label, size = size),
      lineheight = 0.92,
      colour = text_col
    ) +
    ggplot2::geom_text(
      data = stream_labels,
      ggplot2::aes(x = x, y = y, label = label),
      size = 2.9,
      colour = "#4B5563",
      fontface = "italic"
    ) +
    ggplot2::geom_text(
      data = column_headers,
      ggplot2::aes(x = x, y = y, label = label),
      size = 3.6,
      colour = text_col,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::coord_cartesian(xlim = c(-0.05, 4.55), ylim = c(0, 11.0), expand = FALSE) +
    ggplot2::labs(
      title = "Participant and Analytic Sample Flow",
      subtitle = "Parallel PF-valid and pupil-QC streams by modality; pooled trial-level GLMM and subject-level coupling"
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 13, hjust = 0.5, margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.5, hjust = 0.5, colour = "#4B5563", margin = ggplot2::margin(b = 8)
      ),
      plot.margin = ggplot2::margin(10, 12, 10, 12)
    )
}
