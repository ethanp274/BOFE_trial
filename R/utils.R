# R/utils.R
# Shared helper functions and constants for BOFE project cleaning/refactor.

TIMEPOINTS <- c(0, 3, 6, 9, 12)
GROUP_LEVELS <- c("cg (control group)", "ig (intervention group)")
IMPUTATION_REPLICATES <- 20L
INTERVENTION_COST_PER_CONSULTATION <- 40
WTP_THRESHOLD_EUR_PER_QALY <- 25000
BOOTSTRAP_ITERATIONS <- 5000
PIPELINE_PROGRESS_LOG <- file.path("outputs", "pipeline_progress.log")

COST_MONTHS_FIRST_HALF <- c("2022_06", "2022_07", "2022_08", "2022_09", "2022_10", "2022_11")
COST_MONTHS_SECOND_HALF <- c("2022_12", "2023_01", "2023_02", "2023_03", "2023_04", "2023_05")
COST_SUMMARY_COLUMNS <- c(
  "cost_C6", "cost_C12",
  "cost_M6", "cost_M12",
  "cost_F6", "cost_F12",
  "cost_H6", "cost_H12",
  "cost_O6", "cost_O12"
)

# Central structural-zero rules used during cleaning.
# These preserve the legacy questionnaire logic without scraping legacy scripts.
STRUCTURAL_ZERO_RULES <- list(
  T0 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
  T3 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
  T6 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
  T9 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
  T12 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\.")
)

# Rename non-id vars by appending suffix (0, 3, 6, 9, 12).
rename_vars <- function(data, suffix) {
  id_vars <- c("D1.1", "D1.2", "D1.3", "D1.4")
  vars_to_rename <- setdiff(names(data), id_vars)
  new_names <- paste0(vars_to_rename, "_", suffix)
  names(data)[names(data) %in% vars_to_rename] <- new_names
  data
}

# Replace exact 0 values with NA for columns matching any of the provided regex patterns.
replace_zeros_with_na_patterns <- function(df, patterns) {
  matched <- sapply(patterns, function(p) grepl(p, names(df)))
  if (is.matrix(matched)) cols <- names(df)[apply(matched, 1, any)] else cols <- names(df)[matched]
  if (length(cols) == 0) cols <- names(df)[sapply(df, is.numeric)]
  for (col in cols) {
    if (col %in% names(df) && is.numeric(df[[col]])) {
      df[[col]] <- ifelse(df[[col]] == 0, NA, df[[col]])
    }
  }
  df
}

apply_structural_zero_rules <- function(df, timepoint_tag) {
  patterns <- STRUCTURAL_ZERO_RULES[[timepoint_tag]]
  if (is.null(patterns)) return(df)
  replace_zeros_with_na_patterns(df, patterns)
}

# Remove labelled attributes safely.
remove_labels <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "labelled") && requireNamespace("labelled", quietly = TRUE)) {
      x <- labelled::remove_val_labels(x)
    }
    if (inherits(x, "haven_labelled")) {
      x <- as.numeric(x)
    }
    x
  })
  df
}

as_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) x <- as.numeric(x)
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  suppressWarnings(as.numeric(x))
}

ensure_numeric_columns <- function(df, cols) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- as_numeric_safe(df[[col]])
  }
  df
}

present_cols <- function(df, cols) {
  cols[cols %in% names(df)]
}

pipeline_log_line <- function(phase, status, detail = NULL, elapsed = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pieces <- c(
    sprintf("[%s]", timestamp),
    sprintf("[%s]", toupper(status)),
    phase
  )
  if (!is.null(detail) && nzchar(detail)) {
    pieces <- c(pieces, "-", detail)
  }
  if (!is.null(elapsed) && is.finite(elapsed)) {
    pieces <- c(pieces, sprintf("(elapsed %.1fs)", elapsed))
  }
  line <- paste(pieces, collapse = " ")
  cat(line, "\n")
  try(flush.console(), silent = TRUE)

  if (!is.null(log_file) && nzchar(log_file)) {
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    cat(line, "\n", file = log_file, append = TRUE)
  }

  invisible(line)
}

pipeline_phase_start <- function(phase, detail = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  pipeline_log_line(phase, "start", detail = detail, log_file = log_file)
  Sys.time()
}

pipeline_phase_info <- function(phase, detail, log_file = PIPELINE_PROGRESS_LOG) {
  pipeline_log_line(phase, "info", detail = detail, log_file = log_file)
}

pipeline_phase_end <- function(phase, started_at = NULL, detail = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  elapsed <- if (!is.null(started_at)) as.numeric(difftime(Sys.time(), started_at, units = "secs")) else NA_real_
  pipeline_log_line(phase, "done", detail = detail, elapsed = elapsed, log_file = log_file)
}

row_sums_strict <- function(df, cols) {
  cols <- present_cols(df, cols)
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  rowSums(df[, cols, drop = FALSE])
}

recode_height_bmi <- function(df) {
  if (all(c("D3.1_0", "D3.2_0") %in% names(df))) {
    df[["D3.1_0"]] <- as_numeric_safe(df[["D3.1_0"]])
    df[["D3.2_0"]] <- as_numeric_safe(df[["D3.2_0"]])
    df[["D3.1_0"]] <- ifelse(df[["D3.1_0"]] > 100, df[["D3.1_0"]] / 100, df[["D3.1_0"]])
    df[["D3.3_0"]] <- df[["D3.2_0"]] / (df[["D3.1_0"]] ^ 2)
  }
  df
}

derive_controlled_outcomes <- function(df, timepoints = TIMEPOINTS) {
  for (tp in timepoints) {
    disease_col <- paste0("D1.3_", tp)
    disease <- if (disease_col %in% names(df)) {
      as_numeric_safe(df[[disease_col]])
    } else {
      as_numeric_safe(df[["D1.3"]])
    }

    act_score <- paste0("ACT.SCORE_", tp)
    ccq_score <- paste0("CCQ.SCORE_", tp)
    act_items <- paste0("ACT.", 1:5, "_", tp)
    ccq_items <- paste0("CCQ.", 1:10, "_", tp)

    df <- ensure_numeric_columns(df, c(act_score, ccq_score, act_items, ccq_items))

    if (act_score %in% names(df)) {
      df[[act_score]][disease == 2] <- NA
      if (tp == 3) {
        df[[act_score]] <- ifelse(df[[act_score]] == 0, NA, df[[act_score]])
      }
    }
    for (col in present_cols(df, act_items)) {
      df[[col]][disease == 2] <- NA
      if (tp == 3 && act_score %in% names(df)) {
        df[[col]] <- ifelse(is.na(df[[act_score]]), NA, df[[col]])
      }
    }

    if (ccq_score %in% names(df)) {
      df[[ccq_score]][disease == 1] <- NA
      if (max(df[[ccq_score]], na.rm = TRUE) > 6.5) {
        df[[ccq_score]] <- df[[ccq_score]] / 10
      }
      if (tp == 3) {
        df[[ccq_score]][df[["D1.2"]] %in% c("KK1A", "KJ2A")] <- NA
      }
    }
    for (col in present_cols(df, ccq_items)) {
      df[[col]][disease == 1] <- NA
      if (tp == 3) {
        df[[col]][df[["D1.2"]] %in% c("KK1A", "KJ2A")] <- NA
      }
    }

    act_controlled <- paste0("ACT_controlled_", tp)
    ccq_controlled <- paste0("CCQ_controlled_", tp)
    controlled <- paste0("controlled_", tp)

    df[[act_controlled]] <- if (act_score %in% names(df)) {
      ifelse(is.na(df[[act_score]]), NA, ifelse(df[[act_score]] >= 20, 1, 0))
    } else {
      NA_real_
    }

    df[[ccq_controlled]] <- if (ccq_score %in% names(df)) {
      ifelse(is.na(df[[ccq_score]]), NA, ifelse(df[[ccq_score]] < 2, 1, 0))
    } else {
      NA_real_
    }

    asthma_idx <- !is.na(disease) & disease == 1
    copd_idx <- !is.na(disease) & disease == 2

    df[[controlled]] <- NA_real_
    df[[controlled]][asthma_idx] <- df[[act_controlled]][asthma_idx]
    df[[controlled]][copd_idx] <- df[[ccq_controlled]][copd_idx]
  }

  df
}

eq5d_disutility_lookup <- list(
  mobility = c("1" = 0, "2" = 0.051, "3" = 0.064, "4" = 0.244, "5" = 0.329),
  selfcare = c("1" = 0, "2" = 0.046, "3" = 0.056, "4" = 0.216, "5" = 0.257),
  activity = c("1" = 0, "2" = 0.050, "3" = 0.064, "4" = 0.225, "5" = 0.255),
  pain = c("1" = 0, "2" = 0.047, "3" = 0.088, "4" = 0.353, "5" = 0.408),
  anxiety = c("1" = 0, "2" = 0.044, "3" = 0.109, "4" = 0.318, "5" = 0.322)
)

score_eq5d_dimension <- function(values, dimension) {
  values <- as.character(as_numeric_safe(values))
  unname(eq5d_disutility_lookup[[dimension]][values])
}

derive_eqindex <- function(df, timepoints = TIMEPOINTS) {
  dimensions <- c(
    mobility = "EQ5D5L.1",
    selfcare = "EQ5D5L.2",
    activity = "EQ5D5L.3",
    pain = "EQ5D5L.4",
    anxiety = "EQ5D5L.5"
  )

  for (tp in timepoints) {
    disutility_cols <- character(0)
    all_dimension_cols <- paste0(dimensions, "_", tp)
    if (!all(all_dimension_cols %in% names(df))) next

    for (dimension in names(dimensions)) {
      source_col <- paste0(dimensions[[dimension]], "_", tp)
      scored_col <- paste0("disut_", dimension, "_", tp)
      df[[source_col]] <- as_numeric_safe(df[[source_col]])
      df[[scored_col]] <- score_eq5d_dimension(df[[source_col]], dimension)
      disutility_cols <- c(disutility_cols, scored_col)
    }

    total_col <- paste0("total_disut_", tp)
    eqindex_col <- paste0("EQindex_", tp)
    df[[total_col]] <- row_sums_strict(df, disutility_cols)
    df[[eqindex_col]] <- round(1 - df[[total_col]], 3)

    all_zero_profile <- Reduce(`&`, lapply(all_dimension_cols, function(col) df[[col]] == 0))
    all_zero_profile[is.na(all_zero_profile)] <- FALSE
    df[[eqindex_col]][all_zero_profile] <- NA
  }

  df
}

derive_baseline_ccq_domains <- function(df) {
  disease <- if ("D1.3_0" %in% names(df)) as_numeric_safe(df[["D1.3_0"]]) else as_numeric_safe(df[["D1.3"]])
  ccq_items <- paste0("CCQ.", 1:10, "_0")
  df <- ensure_numeric_columns(df, ccq_items)

  if (all(c("CCQ.1_0", "CCQ.2_0", "CCQ.5_0", "CCQ.6_0") %in% names(df))) {
    df[["CCQ.symptom_0"]] <- NA_real_
    df[["CCQ.symptom_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.1_0", "CCQ.2_0", "CCQ.5_0", "CCQ.6_0"), drop = FALSE] / 4,
      na.rm = TRUE
    )
  }
  if (all(c("CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0") %in% names(df))) {
    df[["CCQ.functional_0"]] <- NA_real_
    df[["CCQ.functional_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0"), drop = FALSE] / 4,
      na.rm = TRUE
    )
  }
  if (all(c("CCQ.3_0", "CCQ.4_0") %in% names(df))) {
    df[["CCQ.mental_0"]] <- NA_real_
    df[["CCQ.mental_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.3_0", "CCQ.4_0"), drop = FALSE] / 2.27,
      na.rm = TRUE
    )
  }

  df
}

add_analysis_derivations <- function(df) {
  df <- remove_labels(df)
  df <- recode_height_bmi(df)
  df <- derive_controlled_outcomes(df)
  df <- derive_baseline_ccq_domains(df)
  df <- derive_eqindex(df)
  df
}

read_cost_file <- function(path, suffix) {
  df <- read.csv(path, header = TRUE, check.names = FALSE)
  cost_cols <- setdiff(names(df), "PAZIENTE")
  names(df)[names(df) %in% cost_cols] <- paste0(cost_cols, "_", suffix)
  for (col in setdiff(names(df), "PAZIENTE")) {
    df[[col]] <- as_numeric_safe(df[[col]])
    df[[col]][df[[col]] < 0] <- NA
  }
  df
}

sum_cost_period <- function(df, months, suffix) {
  cols <- paste0(months, "_", suffix)
  row_sums_strict(df, cols)
}

build_economic_data <- function(raw_dir = "raw_data") {
  cost_M <- read_cost_file(file.path(raw_dir, "bofe_cost_outpatient_clean.csv"), "M")
  cost_C <- read_cost_file(file.path(raw_dir, "bofe_cost_labs_clean.csv"), "C")
  cost_F <- read_cost_file(file.path(raw_dir, "bofe_cost_medication_clean.csv"), "F")
  cost_H <- read_cost_file(file.path(raw_dir, "bofe_cost_delivery_clean.csv"), "H")
  cost_O <- read_cost_file(file.path(raw_dir, "bofe_cost_inpatient_clean.csv"), "O")

  economic_data <- Reduce(
    function(x, y) merge(x, y, by = "PAZIENTE", all = TRUE),
    list(cost_M, cost_C, cost_F, cost_H, cost_O)
  )
  names(economic_data)[names(economic_data) == "PAZIENTE"] <- "D1.2"

  economic_data[["cost_C6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "C")
  economic_data[["cost_C12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "C")
  economic_data[["cost_M6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "M")
  economic_data[["cost_M12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "M")
  economic_data[["cost_F6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "F")
  economic_data[["cost_F12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "F")
  economic_data[["cost_H6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "H")
  economic_data[["cost_H12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "H")
  economic_data[["cost_O6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "O")
  economic_data[["cost_O12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "O")

  economic_data
}

attach_cost_summaries <- function(df, economic_data) {
  keep_cols <- c("D1.2", COST_SUMMARY_COLUMNS)
  keep_cols <- present_cols(economic_data, keep_cols)
  merge(df, economic_data[, keep_cols, drop = FALSE], by = "D1.2", all.x = TRUE, sort = FALSE)
}

add_completeness_flags <- function(df) {
  disease <- if ("D1.3_0" %in% names(df)) as_numeric_safe(df[["D1.3_0"]]) else as_numeric_safe(df[["D1.3"]])
  act_cols <- paste0("ACT.SCORE_", TIMEPOINTS)
  ccq_cols <- paste0("CCQ.SCORE_", TIMEPOINTS)
  eq_cols <- paste0("EQindex_", TIMEPOINTS)

  has_act <- all(act_cols %in% names(df))
  has_ccq <- all(ccq_cols %in% names(df))
  has_eq <- all(eq_cols %in% names(df))

  df[["complete_q"]] <- 0L
  if (has_act && has_eq) {
    df[["complete_q"]][disease == 1 & complete.cases(df[, c(act_cols, eq_cols), drop = FALSE])] <- 1L
  }
  if (has_ccq && has_eq) {
    df[["complete_q"]][disease == 2 & complete.cases(df[, c(ccq_cols, eq_cols), drop = FALSE])] <- 1L
  }
  df[["complete_q"]][df[["D1.2"]] %in% c("JR4B", "LP5B", "PJ8A", "QF8A", "QX7A", "SV3B", "VB4A", "XY5A", "KJ2A", "KK1A")] <- 0L

  df[["complete_c"]] <- 0L
  cost_cols <- present_cols(df, COST_SUMMARY_COLUMNS)
  if (length(cost_cols) > 0) {
    has_any_cost <- rowSums(!is.na(df[, cost_cols, drop = FALSE])) > 0
    df[["complete_c"]][has_any_cost] <- 1L
  }

  df[["complete"]] <- ifelse(df[["complete_q"]] == 1L & df[["complete_c"]] == 1L, 1L, 0L)
  df
}

make_longitudinal_analysis_data <- function(df, timepoints = TIMEPOINTS) {
  rows <- lapply(timepoints, function(tp) {
    controlled_col <- paste0("controlled_", tp)
    eq_col <- paste0("EQindex_", tp)
    data.frame(
      patient = df[["D1.2"]],
      pharmacy = df[["D1.1"]],
      time = tp,
      condition = if (paste0("D1.3_", tp) %in% names(df)) as_numeric_safe(df[[paste0("D1.3_", tp)]]) else as_numeric_safe(df[["D1.3"]]),
      group = if (paste0("D1.4_", tp) %in% names(df)) as.character(df[[paste0("D1.4_", tp)]]) else as.character(df[["D1.4"]]),
      gender = as_numeric_safe(df[["D2.2_0"]]),
      age = as_numeric_safe(df[["D2.3_0"]]),
      controlled_0 = as_numeric_safe(df[["controlled_0"]]),
      controlled_t = if (controlled_col %in% names(df)) as_numeric_safe(df[[controlled_col]]) else NA_real_,
      EQindex_t = if (eq_col %in% names(df)) as_numeric_safe(df[[eq_col]]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  long_df <- do.call(rbind, rows)

  for (tp in timepoints) {
    idx <- long_df$time == tp
    if (tp == 0) {
      long_df$qaly_interval[idx] <- 0
    } else {
      prev_tp <- timepoints[match(tp, timepoints) - 1]
      prev_col <- paste0("EQindex_", prev_tp)
      curr_col <- paste0("EQindex_", tp)
      if (all(c(prev_col, curr_col) %in% names(df))) {
        long_df$qaly_interval[idx] <- 0.25 * (as_numeric_safe(df[[prev_col]]) + as_numeric_safe(df[[curr_col]])) / 2
      } else {
        long_df$qaly_interval[idx] <- NA_real_
      }
    }
  }

  resource_map <- c(
    GP_visits = "D3.10_1",
    nurse_visits = "D3.10_2",
    therapist_visits = "D3.10_3",
    AE_visits = "D3.10_4",
    outpatient_visits = "D3.10_5",
    inpatient_visits = "D3.10_6",
    inpatient_days = "D3.10_7",
    sw_visits = "D3.11_1",
    carecentre_visits_pw = "D3.11_2"
  )

  for (resource_name in names(resource_map)) {
    long_df[[resource_name]] <- NA_real_
    for (tp in timepoints) {
      col <- paste0(resource_map[[resource_name]], "_", tp)
      if (col %in% names(df)) {
        long_df[[resource_name]][long_df$time == tp] <- as_numeric_safe(df[[col]])
      }
    }
  }

  long_df$interv_cost <- ifelse(
    long_df$group == "ig (intervention group)" & long_df$time %in% c(0, 6),
    INTERVENTION_COST_PER_CONSULTATION,
    0
  )

  cost_time_map <- list(
    outpatient_cost = c("6" = "cost_M6", "12" = "cost_M12"),
    lab_cost = c("6" = "cost_C6", "12" = "cost_C12"),
    med_cost = c("6" = "cost_F6", "12" = "cost_F12"),
    delivery_cost = c("6" = "cost_H6", "12" = "cost_H12"),
    inpatient_cost = c("6" = "cost_O6", "12" = "cost_O12")
  )

  for (cost_name in names(cost_time_map)) {
    long_df[[cost_name]] <- NA_real_
    for (time_name in names(cost_time_map[[cost_name]])) {
      col <- cost_time_map[[cost_name]][[time_name]]
      if (col %in% names(df)) {
        long_df[[cost_name]][long_df$time == as.numeric(time_name)] <- as_numeric_safe(df[[col]])
      }
    }
  }

  long_df$group <- factor(long_df$group, levels = GROUP_LEVELS)
  long_df$time <- factor(long_df$time, levels = timepoints)
  long_df$patient <- factor(long_df$patient)
  long_df$gender <- factor(long_df$gender)
  long_df$age <- factor(long_df$age)
  long_df$controlled_0 <- factor(long_df$controlled_0, levels = c(0, 1))
  long_df$controlled_t <- as.numeric(long_df$controlled_t)

  long_df
}

legacy_cost_complete_patient_ids <- function(df, economic_data) {
  if (is.null(economic_data) || !("D1.2" %in% names(economic_data))) {
    stop("legacy_cost_complete_patient_ids: economic_data must contain D1.2.")
  }

  keep_cols <- setdiff(names(economic_data), c("D1.2", "D1.3_0", "D1.4_0"))
  keep_cols <- keep_cols[keep_cols %in% names(economic_data)]
  if (length(keep_cols) == 0) {
    return(unique(as.character(df[["D1.2"]])))
  }

  econ_no_missing <- economic_data[rowSums(is.na(economic_data[, keep_cols, drop = FALSE])) != length(keep_cols), , drop = FALSE]
  intersect(
    unique(as.character(df[["D1.2"]])),
    unique(as.character(econ_no_missing$D1.2))
  )
}

make_legacy_cea_longitudinal_data <- function(df, timepoints = TIMEPOINTS) {
  if (!all(c("D1.2", "D1.3_0", "D1.4_0", "D2.2_0", "D2.3_0") %in% names(df))) {
    stop("make_legacy_cea_longitudinal_data: missing required baseline columns.")
  }

  resource_map <- c(
    GP_visits = "D3.10_1",
    nurse_visits = "D3.10_2",
    therapist_visits = "D3.10_3",
    AE_visits = "D3.10_4",
    outpatient_visits = "D3.10_5",
    inpatient_visits = "D3.10_6",
    inpatient_days = "D3.10_7",
    sw_visits = "D3.11_1",
    carecentre_visits_pw = "D3.11_2"
  )

  rows <- lapply(timepoints, function(tp) {
    prev_tp <- if (tp == min(timepoints)) NA_integer_ else timepoints[match(tp, timepoints) - 1]
    qalys <- if (tp == min(timepoints)) {
      rep(0, nrow(df))
    } else {
      prev_col <- paste0("EQindex_", prev_tp)
      curr_col <- paste0("EQindex_", tp)
      0.25 * (as_numeric_safe(df[[prev_col]]) + as_numeric_safe(df[[curr_col]])) / 2
    }

    outpatient_cost <- rep(0, nrow(df))
    lab_cost <- rep(0, nrow(df))
    med_cost <- rep(0, nrow(df))
    delivery_cost <- rep(0, nrow(df))
    inpatient_cost <- rep(0, nrow(df))
    if (tp == 6) {
      outpatient_cost <- as_numeric_safe(df[["cost_M6"]])
      lab_cost <- as_numeric_safe(df[["cost_C6"]])
      med_cost <- as_numeric_safe(df[["cost_F6"]])
      delivery_cost <- as_numeric_safe(df[["cost_H6"]])
      inpatient_cost <- as_numeric_safe(df[["cost_O6"]])
    } else if (tp == 12) {
      outpatient_cost <- as_numeric_safe(df[["cost_M12"]])
      lab_cost <- as_numeric_safe(df[["cost_C12"]])
      med_cost <- as_numeric_safe(df[["cost_F12"]])
      delivery_cost <- as_numeric_safe(df[["cost_H12"]])
      inpatient_cost <- as_numeric_safe(df[["cost_O12"]])
    }

    data.frame(
      patient = df[["D1.2"]],
      time = tp,
      condition = as_numeric_safe(df[["D1.3_0"]]),
      group = as.character(df[["D1.4_0"]]),
      gender = as_numeric_safe(df[["D2.2_0"]]),
      age = as_numeric_safe(df[["D2.3_0"]]),
      controlled_0 = as_numeric_safe(df[["controlled_0"]]),
      controlled_t = as_numeric_safe(df[[paste0("controlled_", tp)]]),
      qalys = qalys,
      interv_cost = ifelse(
        tp %in% c(0, 6) & as.character(df[["D1.4_0"]]) == "ig (intervention group)",
        INTERVENTION_COST_PER_CONSULTATION,
        0
      ),
      outpatient_cost = outpatient_cost,
      lab_cost = lab_cost,
      med_cost = med_cost,
      delivery_cost = delivery_cost,
      inpatient_cost = inpatient_cost,
      GP_visits = if (paste0(resource_map[["GP_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["GP_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      nurse_visits = if (paste0(resource_map[["nurse_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["nurse_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      therapist_visits = if (paste0(resource_map[["therapist_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["therapist_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      AE_visits = if (paste0(resource_map[["AE_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["AE_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      outpatient_visits = if (paste0(resource_map[["outpatient_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["outpatient_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      inpatient_visits = if (paste0(resource_map[["inpatient_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["inpatient_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      inpatient_days = if (paste0(resource_map[["inpatient_days"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["inpatient_days"]], "_", tp)]])
      } else {
        NA_real_
      },
      sw_visits = if (paste0(resource_map[["sw_visits"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["sw_visits"]], "_", tp)]])
      } else {
        NA_real_
      },
      carecentre_visits_pw = if (paste0(resource_map[["carecentre_visits_pw"]], "_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0(resource_map[["carecentre_visits_pw"]], "_", tp)]])
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })

  long_df <- do.call(rbind, rows)
  long_df$group <- factor(long_df$group, levels = GROUP_LEVELS)
  long_df$time <- factor(long_df$time, levels = timepoints)
  long_df$patient <- factor(long_df$patient)
  long_df$gender <- factor(long_df$gender)
  long_df$age <- factor(long_df$age)
  long_df$controlled_0 <- factor(long_df$controlled_0, levels = c(0, 1))

  long_df
}

prepare_legacy_cea_patient_level <- function(df, economic_data = NULL) {
  if (!all(COST_SUMMARY_COLUMNS %in% names(df))) {
    if (is.null(economic_data)) {
      stop("prepare_legacy_cea_patient_level: cost summaries are missing and no economic_data was supplied.")
    }
    df <- attach_cost_summaries(df, economic_data)
  }

  long_df <- make_legacy_cea_longitudinal_data(df)
  cost_complete_ids <- if (is.null(economic_data)) {
    unique(as.character(df[["D1.2"]]))
  } else {
    legacy_cost_complete_patient_ids(df, economic_data)
  }

  long_df <- long_df[as.character(long_df$patient) %in% cost_complete_ids, , drop = FALSE]
  if (nrow(long_df) == 0) {
    stop("prepare_legacy_cea_patient_level: no patients matched the legacy cost-complete cohort.")
  }

  patient_level <- stats::aggregate(
    long_df[, c("qalys", "interv_cost", "outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost"), drop = FALSE],
    by = list(
      patient = long_df$patient,
      condition = long_df$condition,
      group = long_df$group,
      gender = long_df$gender,
      age = long_df$age,
      controlled_0 = long_df$controlled_0
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  names(patient_level)[names(patient_level) == "qalys"] <- "QALY"
  patient_level$total_cost <- rowSums(
    patient_level[, c("interv_cost", "outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost"), drop = FALSE],
    na.rm = TRUE
  )
  patient_level$total_cost_gamma <- patient_level$total_cost + 0.001
  patient_level$QALY_model <- ifelse(patient_level$QALY > 0, patient_level$QALY, 0.0001)
  patient_level$group <- factor(patient_level$group, levels = GROUP_LEVELS)
  patient_level$gender <- factor(patient_level$gender)
  patient_level$age <- factor(patient_level$age)
  patient_level$controlled_0 <- factor(patient_level$controlled_0, levels = c(0, 1))

  if (anyDuplicated(patient_level$patient)) {
    stop("prepare_legacy_cea_patient_level: duplicated patient IDs were created in the legacy CEA cohort.")
  }

  patient_level
}

prepare_cea_patient_level <- function(df) {
  long_df <- make_longitudinal_analysis_data(df)
  cost_cols <- c("interv_cost", "outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost")

  observed_cost_flags <- as.data.frame(!is.na(long_df[, setdiff(cost_cols, "interv_cost"), drop = FALSE]))
  has_any_cost <- stats::aggregate(
    observed_cost_flags,
    by = list(patient = long_df$patient),
    FUN = any
  )
  has_any_cost$has_cost_data <- rowSums(has_any_cost[, -1, drop = FALSE]) > 0

  patient_level <- stats::aggregate(
    long_df[, c("qaly_interval", cost_cols), drop = FALSE],
    by = list(
      patient = long_df$patient,
      condition = long_df$condition,
      group = long_df$group,
      gender = long_df$gender,
      age = long_df$age,
      controlled_0 = long_df$controlled_0
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  names(patient_level)[names(patient_level) == "qaly_interval"] <- "QALY"
  patient_level$total_cost <- rowSums(patient_level[, cost_cols, drop = FALSE], na.rm = TRUE)
  patient_level <- merge(patient_level, has_any_cost[, c("patient", "has_cost_data")], by = "patient", all.x = TRUE)
  patient_level <- patient_level[patient_level$has_cost_data, ]
  patient_level$total_cost_gamma <- patient_level$total_cost + 0.001
  patient_level$QALY_model <- ifelse(patient_level$QALY > 0, patient_level$QALY, 0.0001)
  patient_level$group <- factor(patient_level$group, levels = GROUP_LEVELS)
  patient_level$gender <- factor(patient_level$gender)
  patient_level$age <- factor(patient_level$age)
  patient_level
}

summarise_model_terms <- function(fit, model_name, exponentiate = FALSE) {
  coef_df <- as.data.frame(summary(fit)$coefficients)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  names(coef_df) <- sub("^Estimate$", "estimate", names(coef_df))
  names(coef_df) <- sub("^Std\\. Error$|^Std\\.err$", "std_error", names(coef_df))
  names(coef_df) <- sub("^Pr\\(>\\|z\\|\\)$|^Pr\\(>\\|W\\|\\)$|^Pr\\(>\\|t\\|\\)$", "p_value", names(coef_df))
  coef_df$model <- model_name
  if (exponentiate && "estimate" %in% names(coef_df)) {
    coef_df$estimate_exp <- exp(coef_df$estimate)
  }
  coef_df[, c("model", "term", setdiff(names(coef_df), c("model", "term"))), drop = FALSE]
}
