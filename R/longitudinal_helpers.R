# R/longitudinal_helpers.R
# Wide-to-long reconstruction helpers for effectiveness analyses.

make_longitudinal_analysis_data <- function(df, timepoints = TIMEPOINTS) {
  derive_medication_adherence_event <- function(values) {
    values <- as_numeric_safe(values)
    ifelse(values == 2, 1, ifelse(values == 1, 0, NA_real_))
  }

  derive_non_recent_missed_dose_event <- function(values) {
    values <- as_numeric_safe(values)
    ifelse(values %in% c(3, 4), 1, ifelse(values %in% c(1, 2), 0, NA_real_))
  }

  get_timepoint_values <- function(frame, stem, tp) {
    col <- paste0(stem, "_", tp)
    if (col %in% names(frame)) as_numeric_safe(frame[[col]]) else rep(NA_real_, nrow(frame))
  }

  patient_vec <- if ("patient" %in% names(df)) {
    df[["patient"]]
  } else {
    df[["D1.2"]]
  }
  condition_vec <- if ("condition" %in% names(df)) {
    df[["condition"]]
  } else if ("D1.3_0" %in% names(df)) {
    as_numeric_safe(df[["D1.3_0"]])
  } else {
    as_numeric_safe(df[["D1.3"]])
  }
  group_vec <- if ("group" %in% names(df)) {
    as.character(df[["group"]])
  } else if ("D1.4_0" %in% names(df)) {
    as.character(df[["D1.4_0"]])
  } else {
    as.character(df[["D1.4"]])
  }
  gender_vec <- if ("gender" %in% names(df)) {
    as_numeric_safe(df[["gender"]])
  } else {
    as_numeric_safe(df[["D2.2_0"]])
  }
  age_vec <- if ("age" %in% names(df)) {
    as_numeric_safe(df[["age"]])
  } else {
    as_numeric_safe(df[["D2.3_0"]])
  }
  controlled_0_vec <- if ("controlled_0" %in% names(df)) {
    as_numeric_safe(df[["controlled_0"]])
  } else {
    as_numeric_safe(df[["controlled_0"]])
  }
  medication_adherence_0_vec <- derive_medication_adherence_event(
    get_timepoint_values(df, "med_adherence", 0)
  )
  non_recent_missed_dose_0_vec <- derive_non_recent_missed_dose_event(
    get_timepoint_values(df, "last_missed_dose", 0)
  )
  rows <- lapply(timepoints, function(tp) {
    controlled_col <- paste0("controlled_", tp)
    eq_col <- paste0("EQindex_", tp)
    med_adherence_raw <- get_timepoint_values(df, "med_adherence", tp)
    last_missed_raw <- get_timepoint_values(df, "last_missed_dose", tp)
    data.frame(
      patient = patient_vec,
      pharmacy = if ("pharmacy" %in% names(df)) {
        df[["pharmacy"]]
      } else if ("D1.1" %in% names(df)) {
        df[["D1.1"]]
      } else {
        rep(NA_character_, length(patient_vec))
      },
      time = tp,
      condition = if (paste0("D1.3_", tp) %in% names(df)) {
        as_numeric_safe(df[[paste0("D1.3_", tp)]])
      } else {
        condition_vec
      },
      group = if (paste0("D1.4_", tp) %in% names(df)) {
        as.character(df[[paste0("D1.4_", tp)]])
      } else {
        group_vec
      },
      gender = gender_vec,
      age = age_vec,
      controlled_0 = controlled_0_vec,
      controlled_t = if (controlled_col %in% names(df)) as_numeric_safe(df[[controlled_col]]) else NA_real_,
      EQindex_t = if (eq_col %in% names(df)) as_numeric_safe(df[[eq_col]]) else NA_real_,
      med_adherence_t = med_adherence_raw,
      last_missed_dose_t = last_missed_raw,
      medication_adherence_0 = medication_adherence_0_vec,
      medication_adherence_t = derive_medication_adherence_event(med_adherence_raw),
      non_recent_missed_dose_0 = non_recent_missed_dose_0_vec,
      non_recent_missed_dose_t = derive_non_recent_missed_dose_event(last_missed_raw),
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
  long_df$medication_adherence_0 <- factor(long_df$medication_adherence_0, levels = c(0, 1))
  long_df$medication_adherence_t <- as.numeric(long_df$medication_adherence_t)
  long_df$non_recent_missed_dose_0 <- factor(long_df$non_recent_missed_dose_0, levels = c(0, 1))
  long_df$non_recent_missed_dose_t <- as.numeric(long_df$non_recent_missed_dose_t)

  long_df
}

wide_to_analysis_long <- function(df, analysis = c("effectiveness"), timepoints = TIMEPOINTS) {
  analysis <- match.arg(analysis)
  df <- standardize_core_identifiers(df)
  if (analysis == "effectiveness") {
    long_df <- make_longitudinal_analysis_data(df, timepoints = timepoints)
    assert_data_contract(long_df, "effectiveness_long", require_unique_key = FALSE)
    return(long_df)
  }
  stop("wide_to_analysis_long: only effectiveness long-form reconstruction is supported.")
}

prepare_effectiveness_long_sets <- function(imputation, timepoints = TIMEPOINTS) {
  if (inherits(imputation, "mids")) {
    imputed_sets <- mice::complete(imputation, action = "all", include = FALSE)
  } else {
    imputed_sets <- list(imputation)
  }

  long_sets <- vector("list", length(imputed_sets))
  for (i in seq_along(imputed_sets)) {
    frame <- as.data.frame(imputed_sets[[i]])
    long_data <- wide_to_analysis_long(frame, analysis = "effectiveness", timepoints = timepoints)
    long_data <- long_data[order(
      as.character(long_data$patient),
      as_numeric_safe(as.character(long_data$time))
    ), , drop = FALSE]
    long_data$time <- as.factor(long_data$time)
    levels(long_data$time) <- c("0mo", "3mo", "6mo", "9mo", "12mo")
    long_data$time <- as.character(long_data$time)
    long_data$group <- factor(long_data$group, levels = GROUP_LEVELS)
    long_data$age <- as.factor(long_data$age)
    long_sets[[i]] <- long_data
  }

  list(
    imputed_sets = imputed_sets,
    long_sets = long_sets,
    n_imputations = length(imputed_sets),
    imputation_is_mids = inherits(imputation, "mids")
  )
}
