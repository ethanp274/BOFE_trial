# R/validation_helpers.R
# Fast validation checks for canonical BOFE pipeline artifacts.

validation_row <- function(check, status, detail = "", elapsed = NA_real_) {
  data.frame(
    check = check,
    status = status,
    detail = as.character(detail),
    elapsed_seconds = elapsed,
    checked_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
}

run_validation_check <- function(check, expr) {
  expr <- substitute(expr)
  started <- Sys.time()
  tryCatch(
    {
      detail <- eval(expr, parent.frame())
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      if (is.null(detail) || length(detail) == 0) detail <- "ok"
      validation_row(check, "pass", paste(as.character(detail), collapse = "; "), elapsed)
    },
    error = function(e) {
      elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      validation_row(check, "fail", conditionMessage(e), elapsed)
    }
  )
}

skip_validation_check <- function(check, detail) {
  validation_row(check, "skip", detail)
}

combine_validation_reports <- function(...) {
  reports <- Filter(
    function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
    list(...)
  )
  if (length(reports) == 0) {
    return(data.frame(
      check = character(0),
      status = character(0),
      detail = character(0),
      elapsed_seconds = numeric(0),
      checked_at = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, reports)
}

validation_has_failures <- function(report) {
  is.data.frame(report) && nrow(report) > 0 && any(report$status == "fail")
}

validation_summary <- function(report) {
  if (!is.data.frame(report) || nrow(report) == 0) {
    return(data.frame(status = character(0), n = integer(0)))
  }
  counts <- table(report$status)
  data.frame(
    status = names(counts),
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

validate_no_extra_economic_columns <- function(economic_data) {
  allowed <- c("D1.2", "patient", "cost_complete", "cost_medication_file_present", COST_SUMMARY_COLUMNS)
  extra <- setdiff(names(economic_data), allowed)
  if (length(extra) > 0) {
    stop("economic_data contains noncanonical columns: ", paste(extra, collapse = ", "))
  }
  invisible(TRUE)
}

validate_cost_completeness_policy <- function(all_cases) {
  assert_required_columns(all_cases, c("cost_complete", COST_SUMMARY_COLUMNS), "all_cases")
  complete_rows <- as.logical(all_cases[["cost_complete"]])
  complete_rows[is.na(complete_rows)] <- FALSE
  no_source_rows <- !complete_rows
  if (any(no_source_rows)) {
    missing_counts <- rowSums(is.na(all_cases[no_source_rows, COST_SUMMARY_COLUMNS, drop = FALSE]))
    if (any(missing_counts != length(COST_SUMMARY_COLUMNS))) {
      stop("patients without raw cost-source rows should retain all cost summaries as missing.")
    }
  }
  paste(
    sum(complete_rows), "patients with raw cost-source rows;",
    sum(no_source_rows), "patients without raw cost-source rows;",
    sum(is.na(all_cases[complete_rows, COST_SUMMARY_COLUMNS, drop = FALSE])), "source-present period summaries still require imputation"
  )
}

validate_cost_predictor_exclusion <- function(predictor_matrix) {
  cost_cols <- intersect(COST_SUMMARY_COLUMNS, colnames(predictor_matrix))
  if (length(cost_cols) == 0) {
    stop("predictor matrix has no canonical cost-summary columns.")
  }
  non_cost_targets <- setdiff(rownames(predictor_matrix), COST_SUMMARY_COLUMNS)
  if (length(non_cost_targets) == 0) {
    return(invisible(TRUE))
  }
  pred_slice <- predictor_matrix[non_cost_targets, cost_cols, drop = FALSE]
  bad <- which(pred_slice != 0, arr.ind = TRUE)
  if (nrow(bad) > 0) {
    examples <- paste0(
      rownames(pred_slice)[bad[, "row"]],
      " <- ",
      colnames(pred_slice)[bad[, "col"]]
    )
    stop(
      "cost summaries are predicting non-cost targets: ",
      paste(unique(examples)[seq_len(min(length(unique(examples)), 10))], collapse = "; ")
    )
  }
  invisible(TRUE)
}

validate_no_future_predictors <- function(predictor_audit) {
  if (!"uses_future_timepoint" %in% names(predictor_audit)) {
    stop("predictor audit is missing uses_future_timepoint.")
  }
  future_flag <- as.logical(predictor_audit$uses_future_timepoint)
  future_flag[is.na(future_flag)] <- FALSE
  bad <- predictor_audit[future_flag, , drop = FALSE]
  if (nrow(bad) > 0) {
    stop("future-timepoint predictors found for: ", paste(head(bad$variable, 10), collapse = ", "))
  }
  invisible(TRUE)
}

validate_eq5d_item_levels <- function(df) {
  eq_cols <- grep("^EQ5D5L\\.[1-5]_[0-9]+$", names(df), value = TRUE)
  if (length(eq_cols) == 0) {
    return(invisible(TRUE))
  }

  bad <- do.call(rbind, lapply(eq_cols, function(col) {
    values <- suppressWarnings(as.numeric(as.character(df[[col]])))
    bad_values <- sort(unique(values[!is.na(values) & !values %in% 1:5]))
    if (length(bad_values) == 0) return(NULL)
    data.frame(
      column = col,
      invalid_values = paste(bad_values, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))

  if (!is.null(bad) && nrow(bad) > 0) {
    stop(
      "EQ-5D item columns contain values outside 1-5: ",
      paste(paste0(bad$column, "=", bad$invalid_values), collapse = "; ")
    )
  }
  invisible(TRUE)
}

validate_cleaning_artifact <- function(artifact = read_canonical_artifact("cleaning")) {
  combine_validation_reports(
    run_validation_check("cleaning: required bundle fields", {
      required <- c("all_cases", "complete_cases", "economic_data")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check("cleaning: all_cases contract", {
      assert_data_contract(artifact$all_cases, "analysis_wide")
      paste(nrow(artifact$all_cases), "rows")
    }),
    run_validation_check("cleaning: complete_cases contract", {
      assert_data_contract(artifact$complete_cases, "analysis_wide")
      paste(nrow(artifact$complete_cases), "rows")
    }),
    run_validation_check("cleaning: economic_data contract", {
      assert_data_contract(artifact$economic_data, "economic_cost_summary")
      validate_no_extra_economic_columns(artifact$economic_data)
      paste(nrow(artifact$economic_data), "rows")
    }),
    run_validation_check("cleaning: cost completeness policy", {
      validate_cost_completeness_policy(artifact$all_cases)
    }),
    run_validation_check("cleaning: EQ-5D item levels", {
      validate_eq5d_item_levels(artifact$all_cases)
      "all raw EQ-5D item values are 1-5 or missing"
    })
  )
}

validate_imputation_frame <- function(df_impute) {
  combine_validation_reports(
    run_validation_check("imputation frame: contract", {
      assert_data_contract(df_impute, "imputation_wide")
      paste(nrow(df_impute), "rows and", ncol(df_impute), "columns")
    }),
    run_validation_check("imputation frame: group levels", {
      missing_groups <- setdiff(GROUP_LEVELS, unique(as.character(df_impute$group)))
      if (length(missing_groups) > 0) stop("missing groups: ", paste(missing_groups, collapse = ", "))
      "both trial arms present"
    }),
    run_validation_check("imputation frame: EQ-5D item levels", {
      validate_eq5d_item_levels(df_impute)
      "all raw EQ-5D item values are 1-5 or missing"
    })
  )
}

validate_imputation_branch_frame <- function(df, branch = c("effectiveness", "cea")) {
  branch <- match.arg(branch)
  combine_validation_reports(
    run_validation_check(paste0("imputation ", branch, " frame: required columns"), {
      assert_data_contract(df, paste0(branch, "_imputation_wide"))
      paste(nrow(df), "rows and", ncol(df), "columns")
    }),
    run_validation_check(paste0("imputation ", branch, " frame: no resource-use auxiliaries"), {
      resource_cols <- grep(imputation_resource_pattern(), names(df), value = TRUE)
      if (length(resource_cols) > 0) {
        stop("resource-use columns found: ", paste(head(resource_cols, 10), collapse = ", "))
      }
      "no resource-use columns"
    }),
    run_validation_check(paste0("imputation ", branch, " frame: branch-specific cost policy"), {
      cost_cols <- intersect(COST_SUMMARY_COLUMNS, names(df))
      if (branch == "effectiveness" && length(cost_cols) > 0) {
        stop("effectiveness imputation contains cost columns: ", paste(cost_cols, collapse = ", "))
      }
      if (branch == "cea" && length(cost_cols) != length(COST_SUMMARY_COLUMNS)) {
        stop("CEA imputation is missing cost columns: ", paste(setdiff(COST_SUMMARY_COLUMNS, cost_cols), collapse = ", "))
      }
      "cost-column policy satisfied"
    })
  )
}

validate_imputation_artifact <- function(artifact = read_canonical_artifact("imputation")) {
  combine_validation_reports(
    run_validation_check("imputation artifact: required bundle fields", {
      required <- c(
        "df_impute", "selection_profile",
        "effectiveness_df_impute", "effectiveness_mids", "effectiveness_predictor_matrix",
        "effectiveness_methods", "effectiveness_predictor_audit", "effectiveness_diagnostics",
        "cea_df_impute", "cea_mids", "cea_predictor_matrix",
        "cea_methods", "cea_predictor_audit", "cea_diagnostics",
        "quickpred_summary", "quickpred_comparison", "missingness_report"
      )
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    validate_imputation_frame(artifact$df_impute),
    validate_imputation_branch_frame(artifact$effectiveness_df_impute, "effectiveness"),
    validate_imputation_branch_frame(artifact$cea_df_impute, "cea"),
    run_validation_check("imputation artifact: effectiveness MICE object", {
      if (!inherits(artifact$effectiveness_mids, "mids")) stop("effectiveness_mids is not a mice mids object.")
      paste(artifact$effectiveness_mids$m, "imputations")
    }),
    run_validation_check("imputation artifact: CEA MICE object", {
      if (!inherits(artifact$cea_mids, "mids")) stop("cea_mids is not a mice mids object.")
      paste(artifact$cea_mids$m, "imputations")
    }),
    run_validation_check("imputation artifact: effectiveness has no cost predictors", {
      if (any(COST_SUMMARY_COLUMNS %in% colnames(artifact$effectiveness_predictor_matrix))) {
        stop("effectiveness predictor matrix contains cost columns.")
      }
      "cost summaries excluded from effectiveness imputation"
    }),
    run_validation_check("imputation artifact: effectiveness no future predictors", {
      validate_no_future_predictors(artifact$effectiveness_predictor_audit)
      "no future-timepoint predictors"
    }),
    run_validation_check("imputation artifact: CEA no future predictors", {
      validate_no_future_predictors(artifact$cea_predictor_audit)
      "no future-timepoint predictors"
    })
  )
}

validate_descriptives_artifact <- function(artifact = read_canonical_artifact("descriptives")) {
  combine_validation_reports(
    run_validation_check("descriptives artifact: required bundle fields", {
      required <- c("table1_analyzed", "missingness_summary", "cost_summary", "resource_use_summary")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check("descriptives artifact: nonempty tables", {
      table_names <- c("table1_analyzed", "missingness_summary", "cost_summary", "resource_use_summary")
      counts <- vapply(table_names, function(name) {
        table <- artifact[[name]]
        if (is.null(table) || !is.data.frame(table)) return(0L)
        nrow(table)
      }, integer(1))
      if (any(counts == 0)) stop("empty tables: ", paste(names(counts)[counts == 0], collapse = ", "))
      paste(paste(names(counts), counts, sep = "="), collapse = "; ")
    })
  )
}

validate_effectiveness_gee_artifact <- function(artifact = read_canonical_artifact("effectiveness_gee")) {
  combine_validation_reports(
    run_validation_check("GEE artifact: required bundle fields", {
      required <- c("gee_pooled_summary", "gee_timepoint_effects", "gee_manuscript_style_12mo")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check("GEE artifact: adjusted 12-month contrast", {
      df <- artifact$gee_timepoint_effects
      assert_required_columns(df, c("time", "adjustment", "odds_ratio", "ci_low", "ci_high"), "GEE timepoint effects")
      row <- df[df$time == 12 & df$adjustment == "adjusted", , drop = FALSE]
      if (nrow(row) == 0) stop("missing adjusted 12-month row.")
      if (any(!is.finite(c(row$odds_ratio, row$ci_low, row$ci_high)))) {
        stop("adjusted 12-month row has nonfinite estimate or CI.")
      }
      paste("adjusted 12-month OR", format(row$odds_ratio[1], digits = 4))
    })
  )
}

validate_cea_artifact <- function(
    artifact = read_canonical_artifact("cea"),
    check_prefix = "CEA artifact",
    max_abs_incremental_cost = method_config("validation", "max_abs_incremental_cost")) {
  combine_validation_reports(
    run_validation_check(paste0(check_prefix, ": required bundle fields"), {
      required <- c("patient_level", "summary", "model_summaries", "cea_model_comparison", "bootstrap_results")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check(paste0(check_prefix, ": patient-level contract"), {
      assert_data_contract(artifact$patient_level, "cea_patient_level")
      paste(nrow(artifact$patient_level), "patients")
    }),
    run_validation_check(paste0(check_prefix, ": summary metrics"), {
      required_metrics <- c(
        "incremental_cost",
        "incremental_qaly",
        "ICER",
        paste0("probability_acceptable_at_", WTP_THRESHOLD_EUR_PER_QALY)
      )
      missing <- setdiff(required_metrics, artifact$summary$metric)
      if (length(missing) > 0) stop("missing metrics: ", paste(missing, collapse = ", "))
      "all primary CEA metrics present"
    }),
    run_validation_check(paste0(check_prefix, ": bootstrap costs finite and bounded"), {
      df <- artifact$bootstrap_results
      assert_required_columns(df, c("incremental_cost", "incremental_qaly", "n_imputations"), "CEA bootstrap results")
      costs <- as.numeric(df$incremental_cost)
      if (any(!is.finite(costs))) stop("nonfinite incremental costs found.")
      if (any(abs(costs) > max_abs_incremental_cost)) {
        stop("incremental costs exceed ", max_abs_incremental_cost, " in absolute value.")
      }
      paste(nrow(df), "bootstrap rows")
    })
  )
}

validate_manuscript_outputs_artifact <- function(artifact = read_canonical_artifact("manuscript_outputs")) {
  combine_validation_reports(
    run_validation_check("manuscript outputs artifact: required bundle fields", {
      required <- c("manuscript_results_summary", "complete_cases_long")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check("manuscript outputs artifact: summary nonempty", {
      if (nrow(artifact$manuscript_results_summary) == 0) stop("summary table is empty.")
      paste(nrow(artifact$manuscript_results_summary), "summary rows")
    })
  )
}

validate_sensitivity_artifact <- function(artifact = read_canonical_artifact("sensitivity")) {
  combine_validation_reports(
    run_validation_check("sensitivity artifact: required bundle fields", {
      required <- c("imputations", "effectiveness_sensitivity", "cea_sensitivity_summary", "cea_cost_sensitivity", "uk_tariff_summary")
      missing <- setdiff(required, names(artifact))
      if (length(missing) > 0) stop("missing fields: ", paste(missing, collapse = ", "))
      "required fields present"
    }),
    run_validation_check("sensitivity artifact: nonempty summaries", {
      required_tables <- c("effectiveness_sensitivity", "cea_sensitivity_summary", "cea_cost_sensitivity", "uk_tariff_summary")
      counts <- vapply(artifact[required_tables], nrow, integer(1))
      if (any(counts == 0)) stop("empty tables: ", paste(names(counts)[counts == 0], collapse = ", "))
      paste(paste(names(counts), counts, sep = "="), collapse = "; ")
    })
  )
}
