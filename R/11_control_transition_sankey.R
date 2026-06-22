# R/11_control_transition_sankey.R
# Aggregated Sankey-style figures for observed disease-control transitions from
# baseline to 12 months, using the anonymized publication dataset.

source("R/utils.R")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required to draw the control-transition Sankey diagrams.")
}

pipeline_started <- pipeline_phase_start(
  "11_control_transition_sankey",
  "drawing baseline-to-12-month control transition diagrams"
)

input_csv <- file.path(DATA_PROCESSED_DIR, "bofe_publication_anonymized_long.csv")
if (!file.exists(input_csv)) {
  stop("Missing ", input_csv, ". Run R/01b_publication_long_dataset.R before drawing Sankey diagrams.")
}

publication_long <- read.csv(input_csv, na.strings = "NA", stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("anon_patient_id", "timepoint_month", "trial_arm", "condition", "disease_controlled")
missing_columns <- setdiff(required_columns, names(publication_long))
if (length(missing_columns) > 0) {
  stop(
    "Sankey input is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

status_from_control <- function(x) {
  out <- rep(NA_character_, length(x))
  out[is.na(x)] <- "Missing"
  out[!is.na(x) & as.numeric(x) == 1] <- "Controlled"
  out[!is.na(x) & as.numeric(x) == 0] <- "Uncontrolled"
  out
}

baseline <- publication_long[publication_long$timepoint_month == 0, required_columns, drop = FALSE]
endpoint <- publication_long[publication_long$timepoint_month == 12, required_columns, drop = FALSE]

if (anyDuplicated(baseline$anon_patient_id) || anyDuplicated(endpoint$anon_patient_id)) {
  stop("Expected one baseline and one 12-month row per anonymized patient.")
}

names(baseline)[names(baseline) == "disease_controlled"] <- "baseline_controlled"
names(endpoint)[names(endpoint) == "disease_controlled"] <- "month12_controlled"
endpoint <- endpoint[, c("anon_patient_id", "month12_controlled"), drop = FALSE]

transitions <- merge(baseline, endpoint, by = "anon_patient_id", all = FALSE)
if (nrow(transitions) != length(unique(publication_long$anon_patient_id))) {
  stop("Baseline-to-12-month transition merge did not retain all anonymized patients.")
}

transitions$baseline_status <- status_from_control(transitions$baseline_controlled)
transitions$month12_status <- status_from_control(transitions$month12_controlled)
transitions$transition <- paste(transitions$baseline_status, "to", transitions$month12_status)

status_levels <- c("Controlled", "Uncontrolled", "Missing")
transition_levels <- as.vector(outer(status_levels, status_levels, paste, sep = " to "))
transitions$baseline_status <- factor(transitions$baseline_status, levels = status_levels)
transitions$month12_status <- factor(transitions$month12_status, levels = status_levels)
transitions$transition <- factor(transitions$transition, levels = transition_levels)

safe_filename <- function(x) {
  gsub("[^a-z0-9]+", "_", tolower(x))
}

make_segment_positions <- function(status_values, status_levels, total_n, gap = 0.045) {
  counts <- table(factor(status_values, levels = status_levels))
  usable_height <- 1 - gap * (length(status_levels) - 1)
  heights <- as.numeric(counts) / total_n * usable_height
  y_top <- 1
  segments <- vector("list", length(status_levels))
  for (i in seq_along(status_levels)) {
    y_max <- y_top
    y_min <- y_max - heights[[i]]
    segments[[i]] <- data.frame(
      status = status_levels[[i]],
      count = as.integer(counts[[i]]),
      ymin = y_min,
      ymax = y_max,
      ymid = (y_min + y_max) / 2,
      stringsAsFactors = FALSE
    )
    y_top <- y_min - gap
  }
  do.call(rbind, segments)
}

assign_patient_positions <- function(df, status_col, segments, y_col) {
  df[[y_col]] <- NA_real_
  for (status in status_levels) {
    idx <- which(as.character(df[[status_col]]) == status)
    if (length(idx) == 0) next
    idx <- idx[order(df$anon_patient_id[idx])]
    segment <- segments[segments$status == status, , drop = FALSE]
    if (nrow(segment) != 1 || segment$count == 0) next
    df[[y_col]][idx] <- seq(segment$ymin, segment$ymax, length.out = length(idx) + 2)[-c(1, length(idx) + 2)]
  }
  df
}

status_fill <- c(
  Controlled = "#2f7d5c",
  Uncontrolled = "#bf5b30",
  Missing = "#8a8f98"
)

transition_colors <- c(
  "Controlled to Controlled" = "#2f7d5c",
  "Controlled to Uncontrolled" = "#d78342",
  "Controlled to Missing" = "#9aa1aa",
  "Uncontrolled to Controlled" = "#3f78b5",
  "Uncontrolled to Uncontrolled" = "#bf5b30",
  "Uncontrolled to Missing" = "#70757d",
  "Missing to Controlled" = "#86b79c",
  "Missing to Uncontrolled" = "#cc8f73",
  "Missing to Missing" = "#8a8f98"
)

allocate_flow_ranges <- function(flow_counts, start_segments, end_segments) {
  flow_counts <- flow_counts[order(flow_counts$baseline_status, flow_counts$month12_status), , drop = FALSE]
  flow_counts$baseline_status <- as.character(flow_counts$baseline_status)
  flow_counts$month12_status <- as.character(flow_counts$month12_status)

  flow_counts$start_ymin <- NA_real_
  flow_counts$start_ymax <- NA_real_
  flow_counts$end_ymin <- NA_real_
  flow_counts$end_ymax <- NA_real_

  for (status in status_levels) {
    idx <- which(flow_counts$baseline_status == status)
    if (length(idx) > 0) {
      segment <- start_segments[start_segments$status == status, , drop = FALSE]
      cursor <- segment$ymin
      scale <- ifelse(segment$count > 0, (segment$ymax - segment$ymin) / segment$count, 0)
      for (i in idx) {
        height <- flow_counts$n_patients[[i]] * scale
        flow_counts$start_ymin[[i]] <- cursor
        flow_counts$start_ymax[[i]] <- cursor + height
        cursor <- cursor + height
      }
    }

    idx <- which(flow_counts$month12_status == status)
    if (length(idx) > 0) {
      idx <- idx[order(match(flow_counts$baseline_status[idx], status_levels))]
      segment <- end_segments[end_segments$status == status, , drop = FALSE]
      cursor <- segment$ymin
      scale <- ifelse(segment$count > 0, (segment$ymax - segment$ymin) / segment$count, 0)
      for (i in idx) {
        height <- flow_counts$n_patients[[i]] * scale
        flow_counts$end_ymin[[i]] <- cursor
        flow_counts$end_ymax[[i]] <- cursor + height
        cursor <- cursor + height
      }
    }
  }

  flow_counts$transition <- paste(flow_counts$baseline_status, "to", flow_counts$month12_status)
  flow_counts
}

make_flow_polygons <- function(flow_ranges, x_start = 0.10, x_end = 0.90, n_points = 80) {
  pieces <- vector("list", nrow(flow_ranges))
  smooth <- function(t) t * t * (3 - 2 * t)

  for (i in seq_len(nrow(flow_ranges))) {
    row <- flow_ranges[i, , drop = FALSE]
    t <- seq(0, 1, length.out = n_points)
    s <- smooth(t)
    x <- x_start + (x_end - x_start) * t
    y_top <- row$start_ymax * (1 - s) + row$end_ymax * s
    y_bottom <- row$start_ymin * (1 - s) + row$end_ymin * s
    pieces[[i]] <- data.frame(
      flow_id = i,
      trial_arm = row$trial_arm,
      baseline_status = row$baseline_status,
      month12_status = row$month12_status,
      transition = row$transition,
      n_patients = row$n_patients,
      x = c(x, rev(x)),
      y = c(y_top, rev(y_bottom)),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, pieces)
}

plot_arm_sankey <- function(arm_data, arm_label) {
  arm_data <- arm_data[order(
    arm_data$baseline_status,
    arm_data$month12_status,
    arm_data$anon_patient_id
  ), , drop = FALSE]
  total_n <- nrow(arm_data)
  start_segments <- make_segment_positions(arm_data$baseline_status, status_levels, total_n)
  end_segments <- make_segment_positions(arm_data$month12_status, status_levels, total_n)

  arm_data <- assign_patient_positions(arm_data, "baseline_status", start_segments, "y_start")
  arm_data <- assign_patient_positions(arm_data, "month12_status", end_segments, "y_end")

  start_segments$side <- "Baseline"
  start_segments$xmin <- 0.03
  start_segments$xmax <- 0.10
  end_segments$side <- "12 months"
  end_segments$xmin <- 0.90
  end_segments$xmax <- 0.97
  segments <- rbind(start_segments, end_segments)
  segments <- segments[segments$count > 0, , drop = FALSE]
  segments$label <- paste0(segments$status, "\nn = ", segments$count)
  segments$label_x <- ifelse(segments$side == "Baseline", -0.03, 1.03)
  segments$hjust <- ifelse(segments$side == "Baseline", 1, 0)

  transition_counts <- as.data.frame(table(
    baseline_status = arm_data$baseline_status,
    month12_status = arm_data$month12_status
  ), stringsAsFactors = FALSE)
  transition_counts <- transition_counts[transition_counts$Freq > 0, , drop = FALSE]
  names(transition_counts)[names(transition_counts) == "Freq"] <- "n_patients"
  transition_counts$trial_arm <- arm_label
  transition_counts <- transition_counts[, c("trial_arm", "baseline_status", "month12_status", "n_patients")]

  flow_ranges <- allocate_flow_ranges(transition_counts, start_segments, end_segments)
  flow_polygons <- make_flow_polygons(flow_ranges)
  flow_labels <- transform(
    flow_ranges,
    x = 0.50,
    y = ((start_ymin + start_ymax) / 2 + (end_ymin + end_ymax) / 2) / 2,
    label = paste0("n = ", n_patients)
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = flow_polygons,
      ggplot2::aes(x = x, y = y, group = flow_id, fill = transition),
      alpha = 0.72,
      colour = "white",
      linewidth = 0.25
    ) +
    ggplot2::geom_rect(
      data = segments,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = status),
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = flow_labels,
      ggplot2::aes(x = x, y = y, label = label),
      size = 3.2,
      colour = "#1f2328"
    ) +
    ggplot2::geom_text(
      data = segments,
      ggplot2::aes(x = label_x, y = ymid, label = label, hjust = hjust),
      size = 3.8,
      lineheight = 0.92,
      colour = "#1f2328"
    ) +
    ggplot2::annotate("text", x = 0.065, y = 1.065, label = "Baseline", size = 4.2, fontface = "bold") +
    ggplot2::annotate("text", x = 0.935, y = 1.065, label = "12 months", size = 4.2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c(status_fill, transition_colors), drop = FALSE) +
    ggplot2::coord_cartesian(xlim = c(-0.30, 1.30), ylim = c(-0.04, 1.09), clip = "off") +
    ggplot2::labs(
      title = paste0("Disease-control transition: ", arm_label),
      subtitle = paste0("Observed baseline to 12-month status, N = ", total_n, ". Each band is one transition group."),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(18, 80, 18, 80)
    )

  list(plot = p, transition_counts = transition_counts, patient_traces = arm_data)
}

ensure_artifact_dirs()
arms <- sort(unique(transitions$trial_arm))
plot_results <- vector("list", length(arms))
names(plot_results) <- arms

for (arm in arms) {
  arm_data <- transitions[transitions$trial_arm == arm, , drop = FALSE]
  result <- plot_arm_sankey(arm_data, arm)
  plot_results[[arm]] <- result
  output_png <- file.path(RESULTS_DIR, paste0("sankey_control_transition_", safe_filename(arm), ".png"))
  ggplot2::ggsave(output_png, result$plot, width = 10.5, height = 6.2, dpi = 300, bg = "white")
  pipeline_phase_info("11_control_transition_sankey", paste("Wrote", output_png))
}

transition_counts <- do.call(rbind, lapply(plot_results, `[[`, "transition_counts"))
transition_counts <- transition_counts[order(
  transition_counts$trial_arm,
  transition_counts$baseline_status,
  transition_counts$month12_status
), , drop = FALSE]
write_result_csv(transition_counts, "sankey_control_transition_counts.csv")

patient_traces <- do.call(rbind, lapply(plot_results, `[[`, "patient_traces"))
patient_traces <- patient_traces[, c(
  "anon_patient_id", "trial_arm", "condition",
  "baseline_controlled", "month12_controlled",
  "baseline_status", "month12_status", "transition"
), drop = FALSE]
patient_traces <- patient_traces[order(patient_traces$trial_arm, patient_traces$anon_patient_id), , drop = FALSE]
write_result_csv(patient_traces, "sankey_control_transition_patient_traces.csv")

pipeline_phase_info(
  "11_control_transition_sankey",
  "Wrote results/sankey_control_transition_counts.csv and results/sankey_control_transition_patient_traces.csv"
)
pipeline_phase_end(
  "11_control_transition_sankey",
  pipeline_started,
  "control transition Sankey diagrams complete"
)
