# R/utils.R
# Shared helper functions and constants for BOFE project cleaning/refactor.

TIMEPOINTS <- c(0, 3, 6, 9, 12)
FOLLOWUP_TIMEPOINTS <- c(3, 6, 9, 12)
GROUP_LEVELS <- c("cg (control group)", "ig (intervention group)")
IMPUTATION_REPLICATES <- 20L
INTERVENTION_COST_PER_CONSULTATION <- 40
WTP_THRESHOLD_EUR_PER_QALY <- 29000
BOOTSTRAP_ITERATIONS <- 5000
MODELS_DIR <- "models"
RESULTS_DIR <- "results"
AUDIT_DIR <- "audit"
LOGS_DIR <- "logs"
PIPELINE_PROGRESS_LOG <- file.path(LOGS_DIR, "pipeline_progress.log")

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

standardize_core_identifiers <- function(df) {
  if ("D1.2" %in% names(df) && !"patient" %in% names(df)) {
    df[["patient"]] <- as.character(df[["D1.2"]])
  }
  if ("D1.1" %in% names(df) && !"pharmacy" %in% names(df)) {
    df[["pharmacy"]] <- as.character(df[["D1.1"]])
  }
  if ("D1.3_0" %in% names(df) && !"condition" %in% names(df)) {
    df[["condition"]] <- as_numeric_safe(df[["D1.3_0"]])
  }
  if ("D1.4_0" %in% names(df) && !"group" %in% names(df)) {
    df[["group"]] <- as.character(df[["D1.4_0"]])
  }
  if ("D2.2_0" %in% names(df) && !"gender" %in% names(df)) {
    df[["gender"]] <- as_numeric_safe(df[["D2.2_0"]])
  }
  if ("D2.3_0" %in% names(df) && !"age" %in% names(df)) {
    df[["age"]] <- as_numeric_safe(df[["D2.3_0"]])
  }
  df
}

build_wide_analysis_alias_map <- function() {
  c(
    patient = "D1.2",
    condition = "D1.3_0",
    group = "D1.4_0",
    location = "D2.1_0",
    gender = "D2.2_0",
    age = "D2.3_0",
    ethnicity = "D2.4_0",
    education = "D2.5_0",
    selection = "D2.6_0",
    live_alone = "D2.7_0",
    FVC_0 = "D2.8_0",
    FEV1_0 = "D2.9_0",
    height = "D3.1_0",
    weight = "D3.2_0",
    BMI = "D3.3_0",
    BMI_range = "D3.4_0",
    smoking = "D3.6_0",
    diabetes = "D3.7_1_0",
    ihd = "D3.7_2_0",
    employed = "D3.12_0",
    num_meds = "D5.2_0",
    gp_0 = "D3.10_1_0",
    nurse_0 = "D3.10_2_0",
    therapist_0 = "D3.10_3_0",
    ae_0 = "D3.10_4_0",
    outpatient_0 = "D3.10_5_0",
    inpatient_0 = "D3.10_6_0",
    inpatient_days_0 = "D3.10_7_0",
    sw_0 = "D3.11_1_0",
    daycare_0 = "D3.11_2_0",
    gp_3 = "D3.10_1_3",
    nurse_3 = "D3.10_2_3",
    therapist_3 = "D3.10_3_3",
    ae_3 = "D3.10_4_3",
    outpatient_3 = "D3.10_5_3",
    inpatient_3 = "D3.10_6_3",
    inpatient_days_3 = "D3.10_7_3",
    sw_3 = "D3.11_1_3",
    daycare_3 = "D3.11_2_3",
    gp_6 = "D3.10_1_6",
    nurse_6 = "D3.10_2_6",
    therapist_6 = "D3.10_3_6",
    ae_6 = "D3.10_4_6",
    outpatient_6 = "D3.10_5_6",
    inpatient_6 = "D3.10_6_6",
    inpatient_days_6 = "D3.10_7_6",
    sw_6 = "D3.11_1_6",
    daycare_6 = "D3.11_2_6",
    gp_9 = "D3.10_1_9",
    nurse_9 = "D3.10_2_9",
    therapist_9 = "D3.10_3_9",
    ae_9 = "D3.10_4_9",
    outpatient_9 = "D3.10_5_9",
    inpatient_9 = "D3.10_6_9",
    inpatient_days_9 = "D3.10_7_9",
    sw_9 = "D3.11_1_9",
    daycare_9 = "D3.11_2_9",
    gp_12 = "D3.10_1_12",
    nurse_12 = "D3.10_2_12",
    therapist_12 = "D3.10_3_12",
    ae_12 = "D3.10_4_12",
    outpatient_12 = "D3.10_5_12",
    inpatient_12 = "D3.10_6_12",
    inpatient_days_12 = "D3.10_7_12",
    sw_12 = "D3.11_1_12",
    daycare_12 = "D3.11_2_12",
    med_adherence_0 = "D5.9_0",
    last_missed_dose_0 = "D5.10_0",
    med_adherence_3 = "D5.9_3",
    last_missed_dose_3 = "D5.10_3",
    med_adherence_6 = "D5.9_6",
    last_missed_dose_6 = "D5.10_6",
    med_adherence_9 = "D5.9_9",
    last_missed_dose_9 = "D5.10_9",
    med_adherence_12 = "D5.9_12",
    last_missed_dose_12 = "D5.10_12"
  )
}

rename_by_aliases <- function(df, alias_map) {
  if (is.null(alias_map) || length(alias_map) == 0) {
    return(df)
  }

  for (alias in names(alias_map)) {
    source_name <- unname(alias_map[[alias]])
    if (source_name %in% names(df)) {
      names(df)[names(df) == source_name] <- alias
    }
  }
  df
}

present_cols <- function(df, cols) {
  cols[cols %in% names(df)]
}

infer_timepoint_from_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    return(NA_integer_)
  }
  match_obj <- regexec("([0-9]+)$", name)
  reg <- regmatches(name, match_obj)[[1]]
  if (length(reg) == 0) return(NA_integer_)
  as.integer(reg[2])
}

primary_patient_col <- function(df) {
  if ("patient" %in% names(df)) "patient" else "D1.2"
}

primary_group_col <- function(df) {
  if ("group" %in% names(df)) "group" else "D1.4"
}

primary_condition_col <- function(df) {
  if ("condition" %in% names(df)) "condition" else "D1.3_0"
}

# Allow only same-time or earlier variables to predict each target.
build_time_aware_mice_predictors <- function(df, id_col = "patient", group_col = "group") {
  pred <- mice::make.predictorMatrix(df)
  pred[,] <- 0

  var_times <- vapply(names(df), infer_timepoint_from_name, integer(1))
  var_times[is.na(var_times)] <- 0L
  cost_cols <- intersect(COST_SUMMARY_COLUMNS, names(df))

  for (target in names(df)) {
    if (target %in% c(id_col, group_col)) {
      next
    }

    target_time <- var_times[[target]]
    allowed <- names(df)[var_times <= target_time]
    allowed <- setdiff(allowed, c(target, id_col, group_col))
    if (!target %in% cost_cols) {
      allowed <- setdiff(allowed, cost_cols)
    }
    pred[target, allowed] <- 1
  }

  if (id_col %in% colnames(pred)) {
    pred[, id_col] <- 0
    pred[id_col, ] <- 0
  }
  if (group_col %in% colnames(pred)) {
    pred[, group_col] <- 0
    pred[group_col, ] <- 0
  }

  diag(pred) <- 0
  pred
}

# Keep the legacy predictor chain as the minimal sensitivity baseline.
build_basic_mice_predictors <- function(df, id_col = "patient", group_col = "group") {
  pred <- matrix(0, nrow = ncol(df), ncol = ncol(df), dimnames = list(names(df), names(df)))

  baseline_predictors <- intersect(c("gender", "age", "controlled_0", "EQindex_0"), names(df))
  if (length(baseline_predictors) > 0) {
    pred[, baseline_predictors] <- 1
  }

  lagged_control_pairs <- list(
    c("controlled_6", "controlled_3"),
    c("controlled_9", "controlled_6"),
    c("controlled_12", "controlled_9")
  )
  for (pair in lagged_control_pairs) {
    target <- pair[[1]]
    source <- pair[[2]]
    if (all(c(target, source) %in% names(df))) {
      pred[target, source] <- 1
    }
  }

  # Allow the observed 6-month costs to inform the 12-month cost cells.
  lagged_cost_pairs <- list(
    c("cost_C12", "cost_C6"),
    c("cost_M12", "cost_M6"),
    c("cost_F12", "cost_F6"),
    c("cost_H12", "cost_H6"),
    c("cost_O12", "cost_O6")
  )
  for (pair in lagged_cost_pairs) {
    target <- pair[[1]]
    source <- pair[[2]]
    if (all(c(target, source) %in% names(df))) {
      pred[target, source] <- 1
    }
  }

  if (id_col %in% colnames(pred)) {
    pred[, id_col] <- 0
    pred[id_col, ] <- 0
  }
  if (group_col %in% colnames(pred)) {
    pred[, group_col] <- 0
    pred[group_col, ] <- 0
  }

  diag(pred) <- 0
  pred
}

summarise_mice_predictors <- function(df, predictor_matrix, methods = NULL, id_col = "patient", group_col = "group") {
  if (is.null(predictor_matrix)) {
    stop("summarise_mice_predictors: predictor_matrix is required.")
  }
  if (nrow(predictor_matrix) == 0 || ncol(predictor_matrix) == 0) {
    return(data.frame())
  }

  var_times <- vapply(names(df), infer_timepoint_from_name, integer(1))
  var_times[is.na(var_times)] <- 0L

  imputed_vars <- names(df)
  if (!is.null(methods)) {
    method_names <- intersect(names(methods), imputed_vars)
    imputed_vars <- method_names[methods[method_names] != ""]
  }

  imputed_vars <- setdiff(imputed_vars, c(id_col, group_col))

  rows <- lapply(imputed_vars, function(target) {
    if (!target %in% rownames(predictor_matrix)) {
      return(NULL)
    }

    predictor_flags <- predictor_matrix[target, ]
    predictor_names <- names(predictor_flags)[predictor_flags != 0]
    predictor_names <- setdiff(predictor_names, c(id_col, group_col, target))
    predictor_names <- predictor_names[predictor_names %in% names(df)]

    target_time <- unname(var_times[[target]])
    predictor_times <- var_times[predictor_names]
    predictor_times <- predictor_times[!is.na(predictor_times)]

    data.frame(
      variable = target,
      timepoint = target_time,
      method = if (!is.null(methods) && target %in% names(methods)) methods[[target]] else NA_character_,
      n_predictors = length(predictor_names),
      predictor_columns = if (length(predictor_names) > 0) paste(predictor_names, collapse = "; ") else "",
      predictor_timepoints = if (length(predictor_times) > 0) paste(sort(unique(predictor_times)), collapse = "; ") else "",
      min_predictor_timepoint = if (length(predictor_times) > 0) min(predictor_times) else NA_integer_,
      max_predictor_timepoint = if (length(predictor_times) > 0) max(predictor_times) else NA_integer_,
      uses_future_timepoint = if (length(predictor_times) > 0) any(predictor_times > target_time) else FALSE,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

groupwise_simple_imputation <- function(df, group_col = "group", id_col = "patient") {
  if (!group_col %in% names(df)) {
    stop("groupwise_simple_imputation: missing grouping column ", group_col, ".")
  }

  impute_mode <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    tab <- sort(table(x), decreasing = TRUE)
    names(tab)[1]
  }

  fill_vector <- function(x, fallback) {
    if (is.numeric(x)) {
      replacement <- if (all(is.na(x))) NA_real_ else fallback
      x[is.na(x)] <- replacement
      return(x)
    }

    if (is.factor(x)) {
      replacement <- if (all(is.na(x))) NA_character_ else as.character(fallback)
      x_chr <- as.character(x)
      x_chr[is.na(x_chr)] <- replacement
      return(factor(x_chr, levels = levels(x)))
    }

    if (is.character(x)) {
      replacement <- if (all(is.na(x))) NA_character_ else as.character(fallback)
      x[is.na(x)] <- replacement
      return(x)
    }

    x
  }

  out <- df
  group_values <- unique(as.character(out[[group_col]]))
  for (grp in group_values) {
    idx <- as.character(out[[group_col]]) == grp
    grp_df <- out[idx, , drop = FALSE]
    for (nm in setdiff(names(grp_df), c(id_col, group_col))) {
      x <- grp_df[[nm]]
      if (all(!is.na(x))) next
      if (is.numeric(x)) {
        if (grepl("^EQ5D5L\\.[1-5]_", nm)) {
          fallback <- as_numeric_safe(impute_mode(x))
          if (is.na(fallback)) fallback <- as_numeric_safe(impute_mode(out[[nm]]))
        } else {
          fallback <- mean(x, na.rm = TRUE)
          if (!is.finite(fallback)) fallback <- mean(out[[nm]], na.rm = TRUE)
        }
      } else {
        fallback <- impute_mode(x)
        if (is.na(fallback)) fallback <- impute_mode(out[[nm]])
      }
      out[[nm]][idx] <- fill_vector(x, fallback)
    }
  }
  out
}

ensure_artifact_dirs <- function() {
  dir.create(MODELS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(AUDIT_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(LOGS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create("clean_data", showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

result_path <- function(...) {
  file.path(RESULTS_DIR, ...)
}

model_path <- function(...) {
  file.path(MODELS_DIR, ...)
}

audit_path <- function(...) {
  file.path(AUDIT_DIR, ...)
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
    } else if ("condition" %in% names(df)) {
      as_numeric_safe(df[["condition"]])
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

# Tariff coefficients are held here so QALYs can be rescored consistently.
eq5d_tariff_lookup <- list( 
  italian = list( #Source: Finch et al, 2022
    mobility = c("1" = 0, "2" = 0.051, "3" = 0.064, "4" = 0.244, "5" = 0.329),
    selfcare = c("1" = 0, "2" = 0.046, "3" = 0.056, "4" = 0.216, "5" = 0.257),
    activity = c("1" = 0, "2" = 0.050, "3" = 0.064, "4" = 0.225, "5" = 0.255),
    pain = c("1" = 0, "2" = 0.047, "3" = 0.088, "4" = 0.353, "5" = 0.408),
    anxiety = c("1" = 0, "2" = 0.044, "3" = 0.109, "4" = 0.318, "5" = 0.322)
  ),
  uk = list( #Source: Rowen et al, 2026
    mobility = c("1" = 0, "2" = 0.032, "3" = 0.058, "4" = 0.179, "5" = 0.279),
    selfcare = c("1" = 0, "2" = 0.038, "3" = 0.060, "4" = 0.162, "5" = 0.206),
    activity = c("1" = 0, "2" = 0.049, "3" = 0.086, "4" = 0.184, "5" = 0.212),
    pain = c("1" = 0, "2" = 0.056, "3" = 0.066, "4" = 0.371, "5" = 0.479),
    anxiety = c("1" = 0, "2" = 0.041, "3" = 0.126, "4" = 0.313, "5" = 0.391)
  )
)

score_eq5d_dimension <- function(values, dimension, tariff = c("italian", "uk")) {
  tariff <- match.arg(tariff)
  lookup <- eq5d_tariff_lookup[[tariff]]
  if (is.null(lookup)) {
    stop(
      "score_eq5d_dimension: tariff '", tariff, "' is not configured yet. ",
      "Populate eq5d_tariff_lookup[['", tariff, "']] with the corresponding coefficients."
    )
  }
  values <- as.character(as_numeric_safe(values))
  unname(lookup[[dimension]][values])
}

derive_eqindex <- function(df, timepoints = TIMEPOINTS, tariff = c("italian", "uk")) {
  tariff <- match.arg(tariff)
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
      df[[scored_col]] <- score_eq5d_dimension(df[[source_col]], dimension, tariff = tariff)
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
  disease <- if ("D1.3_0" %in% names(df)) {
    as_numeric_safe(df[["D1.3_0"]])
  } else if ("condition" %in% names(df)) {
    as_numeric_safe(df[["condition"]])
  } else {
    as_numeric_safe(df[["D1.3"]])
  }
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

add_analysis_derivations <- function(df, tariff = c("italian", "uk")) {
  tariff <- match.arg(tariff)
  df <- remove_labels(df)
  df <- standardize_core_identifiers(df)
  df <- recode_height_bmi(df)
  df <- derive_controlled_outcomes(df)
  df <- derive_baseline_ccq_domains(df)
  df <- derive_eqindex(df, tariff = tariff)
  df
}

read_cost_file <- function(path, suffix) {
  df <- read.csv(path, header = TRUE, check.names = FALSE)
  cost_cols <- setdiff(names(df), "PAZIENTE")
  names(df)[names(df) %in% cost_cols] <- paste0(cost_cols, "_", suffix)
  for (col in setdiff(names(df), "PAZIENTE")) {
    df[[col]] <- as_numeric_safe(df[[col]])
    df[[col]][df[[col]] < 0 | df[[col]] == 9999] <- NA
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
  economic_data[["patient"]] <- economic_data[["D1.2"]]

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

  # Keep the canonical economic frame lean: downstream code only uses the
  # patient identifier plus the half-year summary columns.
  economic_data[, c("D1.2", "patient", COST_SUMMARY_COLUMNS), drop = FALSE]
}

attach_cost_summaries <- function(df, economic_data) {
  keep_cols <- c(primary_patient_col(economic_data), COST_SUMMARY_COLUMNS)
  keep_cols <- present_cols(economic_data, keep_cols)
  merge(df, economic_data[, keep_cols, drop = FALSE], by.x = primary_patient_col(df), by.y = primary_patient_col(economic_data), all.x = TRUE, sort = FALSE)
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
  patient_id_col <- primary_patient_col(df)
  df[["complete_q"]][df[[patient_id_col]] %in% c("JR4B", "LP5B", "PJ8A", "QF8A", "QX7A", "SV3B", "VB4A", "XY5A", "KJ2A", "KK1A")] <- 0L

  df[["complete_c"]] <- 0L
  cost_cols <- present_cols(df, COST_SUMMARY_COLUMNS)
  if (length(cost_cols) > 0) {
    has_complete_cost <- complete.cases(df[, cost_cols, drop = FALSE])
    df[["complete_c"]][has_complete_cost] <- 1L
  }

  df[["complete"]] <- ifelse(df[["complete_q"]] == 1L & df[["complete_c"]] == 1L, 1L, 0L)
  df
}

make_longitudinal_analysis_data <- function(df, timepoints = TIMEPOINTS) {
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
  rows <- lapply(timepoints, function(tp) {
    controlled_col <- paste0("controlled_", tp)
    eq_col <- paste0("EQindex_", tp)
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

wide_to_analysis_long <- function(df, analysis = c("effectiveness"), timepoints = TIMEPOINTS) {
  analysis <- match.arg(analysis)
  df <- standardize_core_identifiers(df)
  if (analysis == "effectiveness") {
    return(make_longitudinal_analysis_data(df, timepoints = timepoints))
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

pool_bootstrap_uncertainty <- function(df, estimate_col, variance_col = NULL, alpha = 0.05) {
  if (!estimate_col %in% names(df)) {
    stop("pool_bootstrap_uncertainty: missing estimate column ", estimate_col, ".")
  }

  estimates <- as.numeric(df[[estimate_col]])
  keep <- is.finite(estimates)
  estimates <- estimates[keep]
  if (length(estimates) == 0) {
    return(data.frame())
  }

  within_variance <- 0
  if (!is.null(variance_col) && variance_col %in% names(df)) {
    variances <- as.numeric(df[[variance_col]])[keep]
    variances <- variances[is.finite(variances)]
    if (length(variances) > 0) {
      within_variance <- mean(pmax(variances, 0), na.rm = TRUE)
    }
  }

  between_variance <- if (length(estimates) > 1) stats::var(estimates) else 0
  pooled_variance <- within_variance + between_variance
  pooled_std_error <- sqrt(max(pooled_variance, 0))
  z_value <- qnorm(1 - alpha / 2)

  data.frame(
    estimate = mean(estimates),
    within_variance = within_variance,
    between_variance = between_variance,
    pooled_variance = pooled_variance,
    pooled_std_error = pooled_std_error,
    lower_95 = mean(estimates) - z_value * pooled_std_error,
    upper_95 = mean(estimates) + z_value * pooled_std_error,
    bootstrap_lower_95 = as.numeric(stats::quantile(estimates, probs = alpha / 2, na.rm = TRUE, names = FALSE)),
    bootstrap_upper_95 = as.numeric(stats::quantile(estimates, probs = 1 - alpha / 2, na.rm = TRUE, names = FALSE)),
    n_boot = length(estimates),
    uncertainty_method = if (!is.null(variance_col) && variance_col %in% names(df)) {
      "bootstrap_total_variance"
    } else {
      "bootstrap_percentile"
    },
    stringsAsFactors = FALSE
  )
}

pool_mi_uncertainty <- function(df, estimate_col, variance_col = NULL, alpha = 0.05) {
  if (!estimate_col %in% names(df)) {
    stop("pool_mi_uncertainty: missing estimate column ", estimate_col, ".")
  }

  estimates <- as.numeric(df[[estimate_col]])
  keep <- is.finite(estimates)
  estimates <- estimates[keep]
  if (length(estimates) == 0) {
    return(data.frame())
  }

  within_variance <- 0
  if (!is.null(variance_col) && variance_col %in% names(df)) {
    variances <- as.numeric(df[[variance_col]])[keep]
    variances <- variances[is.finite(variances)]
    if (length(variances) > 0) {
      within_variance <- mean(pmax(variances, 0), na.rm = TRUE)
    }
  }

  m <- length(estimates)
  between_variance <- if (m > 1) stats::var(estimates) else 0
  total_variance <- within_variance + (1 + 1 / m) * between_variance
  total_std_error <- sqrt(max(total_variance, 0))
  z_value <- qnorm(1 - alpha / 2)

  data.frame(
    estimate = mean(estimates),
    within_variance = within_variance,
    between_variance = between_variance,
    pooled_variance = total_variance,
    pooled_std_error = total_std_error,
    lower_95 = mean(estimates) - z_value * total_std_error,
    upper_95 = mean(estimates) + z_value * total_std_error,
    bootstrap_lower_95 = as.numeric(stats::quantile(estimates, probs = alpha / 2, na.rm = TRUE, names = FALSE)),
    bootstrap_upper_95 = as.numeric(stats::quantile(estimates, probs = 1 - alpha / 2, na.rm = TRUE, names = FALSE)),
    n_boot = m,
    uncertainty_method = "rubin_total_variance",
    stringsAsFactors = FALSE
  )
}

# Collapse the wide analysis frame to one row per patient for CEA.
prepare_cea_patient_level <- function(
    df,
    require_cost_data = TRUE,
    patient_ids = NULL,
    economic_data = NULL,
    intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    tariff = c("italian", "uk"),
    allow_duplicate_patients = FALSE
) {
  tariff <- match.arg(tariff)
  df <- standardize_core_identifiers(df)

  if (!all(COST_SUMMARY_COLUMNS %in% names(df))) {
    if (is.null(economic_data)) {
      stop("prepare_cea_patient_level: cost summaries are missing and no economic_data was supplied.")
    }
    df <- attach_cost_summaries(df, economic_data)
  }

  if (!is.null(patient_ids)) {
    patient_ids <- unique(as.character(patient_ids))
    df <- df[as.character(df[[primary_patient_col(df)]]) %in% patient_ids, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("prepare_cea_patient_level: no patients were available after filtering.")
  }

  eq5d_raw_cols <- unlist(
    lapply(TIMEPOINTS, function(tp) {
      paste0(c("EQ5D5L.1", "EQ5D5L.2", "EQ5D5L.3", "EQ5D5L.4", "EQ5D5L.5"), "_", tp)
    }),
    use.names = FALSE
  )
  has_raw_eq5d <- all(eq5d_raw_cols %in% names(df))
  if (tariff == "uk" && !has_raw_eq5d) {
    stop(
      "prepare_cea_patient_level: UK tariff requested but raw EQ-5D item columns are missing. ",
      "Ensure the wide analysis frame retains the EQ5D5L.* columns before imputation."
    )
  }
  if (has_raw_eq5d) {
    df <- derive_eqindex(df, tariff = tariff)
  }

  required_cols <- c(
    "EQindex_0", "EQindex_3", "EQindex_6", "EQindex_9", "EQindex_12",
    "cost_C6", "cost_C12", "cost_M6", "cost_M12", "cost_F6", "cost_F12",
    "cost_H6", "cost_H12", "cost_O6", "cost_O12",
    "controlled_0"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "prepare_cea_patient_level: missing required wide columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  patient_level <- data.frame(
    patient = as.character(df[[primary_patient_col(df)]]),
    condition = as_numeric_safe(df[[primary_condition_col(df)]]),
    group = as.character(df[[primary_group_col(df)]]),
    gender = as_numeric_safe(df[["gender"]]),
    age = as_numeric_safe(df[["age"]]),
    controlled_0 = as_numeric_safe(df[["controlled_0"]]),
    QALY = 0.25 * (as_numeric_safe(df[["EQindex_0"]]) + as_numeric_safe(df[["EQindex_3"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_3"]]) + as_numeric_safe(df[["EQindex_6"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_6"]]) + as_numeric_safe(df[["EQindex_9"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_9"]]) + as_numeric_safe(df[["EQindex_12"]])) / 2,
    interv_cost = ifelse(
      as.character(df[[primary_group_col(df)]]) == "ig (intervention group)",
      intervention_cost_per_consultation * 2,
      0
    ),
    outpatient_cost = as_numeric_safe(df[["cost_M6"]]) + as_numeric_safe(df[["cost_M12"]]),
    lab_cost = as_numeric_safe(df[["cost_C6"]]) + as_numeric_safe(df[["cost_C12"]]),
    med_cost = as_numeric_safe(df[["cost_F6"]]) + as_numeric_safe(df[["cost_F12"]]),
    delivery_cost = as_numeric_safe(df[["cost_H6"]]) + as_numeric_safe(df[["cost_H12"]]),
    inpatient_cost = as_numeric_safe(df[["cost_O6"]]) + as_numeric_safe(df[["cost_O12"]]),
    stringsAsFactors = FALSE
  )

  cost_component_cols <- c("outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost")
  patient_level$has_cost_data <- complete.cases(patient_level[, cost_component_cols, drop = FALSE])
  patient_level$total_cost <- rowSums(
    patient_level[, c("interv_cost", cost_component_cols), drop = FALSE],
    na.rm = FALSE
  )
  if (isTRUE(require_cost_data)) {
    patient_level <- patient_level[patient_level$has_cost_data, , drop = FALSE]
  }
  patient_level$total_cost_gamma <- patient_level$total_cost + 0.001
  patient_level$QALY_model <- patient_level$QALY
  patient_level$group <- factor(patient_level$group, levels = GROUP_LEVELS)
  patient_level$gender <- factor(patient_level$gender)
  patient_level$age <- factor(patient_level$age)
  patient_level$controlled_0 <- factor(patient_level$controlled_0, levels = c(0, 1))

  if (!isTRUE(allow_duplicate_patients) && anyDuplicated(patient_level$patient)) {
    stop("prepare_cea_patient_level: duplicated patient IDs were created.")
  }

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
