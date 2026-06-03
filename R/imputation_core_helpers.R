# R/imputation_core_helpers.R
# Shared imputation predictor-matrix and simple-imputation helpers.

imputation_resource_pattern <- function() {
  "^(gp|nurse|therapist|ae|outpatient|inpatient|inpatient_days|sw|daycare)_"
}

classify_imputation_variable <- function(name) {
  if (name %in% c("patient", "group")) {
    return("id_design")
  }
  if (name %in% COST_SUMMARY_COLUMNS) {
    return("cost")
  }
  if (grepl("^controlled_[0-9]+$", name)) {
    return("effectiveness_outcome")
  }
  if (grepl("^EQindex_[0-9]+$", name)) {
    return("utility_index")
  }
  if (grepl("^EQ5D5L\\.[1-5]_[0-9]+$", name)) {
    return("utility_item")
  }
  if (grepl(imputation_resource_pattern(), name)) {
    return("resource_use")
  }
  if (grepl("^(med_adherence|last_missed_dose)_", name)) {
    return("adherence")
  }
  "baseline_covariate"
}

adherence_imputation_columns <- function(df = NULL) {
  cols <- c(
    paste0("med_adherence_", TIMEPOINTS),
    paste0("last_missed_dose_", TIMEPOINTS)
  )
  if (is.null(df)) return(cols)
  intersect(cols, names(df))
}

recode_adherence_unknowns_for_imputation <- function(df) {
  med_cols <- intersect(paste0("med_adherence_", TIMEPOINTS), names(df))
  missed_cols <- intersect(paste0("last_missed_dose_", TIMEPOINTS), names(df))

  for (col in med_cols) {
    values <- as_numeric_safe(df[[col]])
    values[values == method_config("outcomes", "adherence_unknown_codes", "med_adherence")] <- NA_real_
    values[!is.na(values) & !values %in% c(1, 2)] <- NA_real_
    df[[col]] <- values
  }

  for (col in missed_cols) {
    values <- as_numeric_safe(df[[col]])
    values[values == method_config("outcomes", "adherence_unknown_codes", "last_missed_dose")] <- NA_real_
    values[!is.na(values) & !values %in% 1:4] <- NA_real_
    df[[col]] <- values
  }

  df
}

build_imputation_variable_profile <- function(df) {
  n <- nrow(df)
  data.frame(
    variable = names(df),
    role = vapply(names(df), classify_imputation_variable, character(1)),
    timepoint = vapply(names(df), function(x) {
      tp <- infer_timepoint_from_name(x)
      ifelse(is.na(tp), 0L, tp)
    }, integer(1)),
    storage_class = vapply(df, function(x) class(x)[1], character(1)),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    missing_fraction = vapply(df, function(x) if (n == 0) NA_real_ else mean(is.na(x)), numeric(1)),
    n_observed = vapply(df, function(x) sum(!is.na(x)), integer(1)),
    n_unique_observed = vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1)),
    stringsAsFactors = FALSE
  )
}

imputation_predictor_selection_config <- function(branch = c("effectiveness", "secondary_effectiveness", "cea")) {
  branch <- match.arg(branch)
  selection <- method_config("imputation", "predictor_selection")
  branch_config <- selection[[branch]]
  if (is.null(branch_config)) {
    stop("Missing imputation predictor-selection config for branch '", branch, "'.")
  }
  list(global = selection, branch = branch_config)
}

branch_requested_imputation_variables <- function(df, branch = c("effectiveness", "secondary_effectiveness", "cea")) {
  branch <- match.arg(branch)
  cfg <- imputation_predictor_selection_config(branch)
  baseline_predictors <- cfg$global$baseline_predictors
  include_patterns <- cfg$branch$include_patterns

  requested <- c("patient", "group", baseline_predictors)
  for (pattern in include_patterns) {
    requested <- c(requested, grep(pattern, names(df), value = TRUE))
  }

  unique(requested[requested %in% names(df)])
}

imputation_branch_required_columns <- function(branch = c("effectiveness", "secondary_effectiveness", "cea")) {
  branch <- match.arg(branch)
  if (branch == "effectiveness") {
    return(c(
      "patient", "group", "condition", "gender", "age",
      paste0("controlled_", TIMEPOINTS),
      paste0("EQindex_", TIMEPOINTS)
    ))
  }

  if (branch == "secondary_effectiveness") {
    return(c(
      "patient", "group", "condition", "gender", "age",
      paste0("controlled_", TIMEPOINTS),
      paste0("EQindex_", TIMEPOINTS),
      paste0("med_adherence_", TIMEPOINTS),
      paste0("last_missed_dose_", TIMEPOINTS)
    ))
  }

  c(
    "patient", "group", "condition", "gender", "age", "controlled_0",
    unlist(
      lapply(TIMEPOINTS, function(tp) {
        paste0(c("EQ5D5L.1", "EQ5D5L.2", "EQ5D5L.3", "EQ5D5L.4", "EQ5D5L.5"), "_", tp)
      }),
      use.names = FALSE
    ),
    COST_SUMMARY_COLUMNS
  )
}

build_imputation_selection_profile <- function(df) {
  profile <- build_imputation_variable_profile(df)
  cfg <- method_config("imputation", "predictor_selection")
  excluded_baseline <- cfg$excluded_baseline_predictors

  for (branch in c("effectiveness", "secondary_effectiveness", "cea")) {
    branch_cfg <- cfg[[branch]]
    requested <- branch_requested_imputation_variables(df, branch)
    selected_col <- paste0("selected_", branch)
    reason_col <- paste0("selection_note_", branch)

    selected <- profile$variable %in% requested &
      profile$role %in% branch_cfg$include_roles &
      !profile$role %in% branch_cfg$exclude_roles &
      !profile$variable %in% excluded_baseline &
      profile$n_observed > 0

    notes <- rep("not requested for this branch", nrow(profile))
    notes[profile$variable %in% requested] <- "selected by configured role/pattern"
    notes[profile$variable %in% requested & !profile$role %in% branch_cfg$include_roles] <- "excluded: role is not allowed"
    notes[profile$variable %in% requested & profile$role %in% branch_cfg$exclude_roles] <- "excluded: role is explicitly blocked"
    notes[profile$variable %in% excluded_baseline] <- "excluded: redundant or high-missing baseline predictor"
    notes[profile$variable %in% requested & profile$n_observed == 0] <- "excluded: no observed values"
    notes[selected] <- "selected"

    profile[[selected_col]] <- selected
    profile[[reason_col]] <- notes
  }

  profile
}

build_imputation_branch_frame <- function(df, branch = c("effectiveness", "secondary_effectiveness", "cea")) {
  branch <- match.arg(branch)
  profile <- build_imputation_selection_profile(df)
  selected_col <- paste0("selected_", branch)
  selected_vars <- profile$variable[profile[[selected_col]]]
  required <- imputation_branch_required_columns(branch)
  missing_required <- setdiff(required, selected_vars)
  if (length(missing_required) > 0) {
    stop(
      "build_imputation_branch_frame: branch '", branch, "' is missing required selected columns: ",
      paste(missing_required, collapse = ", ")
    )
  }

  out <- df[, selected_vars, drop = FALSE]
  assert_unique_key(out, "patient", context = paste0(branch, " imputation frame"))
  out
}

predictor_role_allowed_for_target <- function(target_role, predictor_roles, branch = c("effectiveness", "secondary_effectiveness", "cea")) {
  branch <- match.arg(branch)

  if (branch == "effectiveness") {
    return(predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_index"))
  }

  if (branch == "secondary_effectiveness") {
    if (target_role %in% c("effectiveness_outcome", "adherence")) {
      return(predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_index"))
    }
    return(predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_index"))
  }

  if (target_role %in% c("cost", "utility_item")) {
    return(predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_item", "cost"))
  }
  if (target_role == "effectiveness_outcome") {
    return(predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_item"))
  }

  predictor_roles %in% c("baseline_covariate", "effectiveness_outcome", "utility_item")
}

build_analytic_mice_predictors <- function(df, branch = c("effectiveness", "secondary_effectiveness", "cea"), id_col = "patient", group_col = "group") {
  branch <- match.arg(branch)
  pred <- mice::make.predictorMatrix(df)
  pred[,] <- 0

  cfg <- imputation_predictor_selection_config(branch)
  profile <- build_imputation_variable_profile(df)
  var_times <- profile$timepoint
  names(var_times) <- profile$variable
  roles <- profile$role
  names(roles) <- profile$variable

  max_missing <- cfg$global$max_predictor_missing_fraction
  eligible_predictors <- profile$variable[
    profile$n_observed > 0 &
      (profile$missing_fraction <= max_missing | profile$role %in% c("id_design", "baseline_covariate", "cost"))
  ]
  eligible_predictors <- setdiff(eligible_predictors, c(id_col, group_col))

  for (target in names(df)) {
    if (target %in% c(id_col, group_col)) {
      next
    }

    target_time <- var_times[[target]]
    target_role <- roles[[target]]
    allowed <- setdiff(names(df), c(target, id_col, group_col))
    allowed <- allowed[allowed %in% eligible_predictors]
    allowed <- allowed[var_times[allowed] <= target_time]
    allowed <- allowed[predictor_role_allowed_for_target(target_role, roles[allowed], branch)]

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

build_quickpred_matrix <- function(df, id_col = "patient", group_col = "group") {
  cfg <- method_config("imputation", "predictor_selection")
  quick <- tryCatch(
    mice::quickpred(
      df,
      mincor = cfg$quickpred_min_correlation,
      minpuc = cfg$quickpred_min_usable_cases,
      exclude = intersect(c(id_col, group_col), names(df))
    ),
    error = function(e) {
      attr(e, "quickpred_failed") <- TRUE
      e
    }
  )

  if (inherits(quick, "error")) {
    empty <- matrix(0, nrow = ncol(df), ncol = ncol(df), dimnames = list(names(df), names(df)))
    attr(empty, "quickpred_error") <- conditionMessage(quick)
    return(empty)
  }

  if (id_col %in% colnames(quick)) {
    quick[, id_col] <- 0
    quick[id_col, ] <- 0
  }
  if (group_col %in% colnames(quick)) {
    quick[, group_col] <- 0
    quick[group_col, ] <- 0
  }
  diag(quick) <- 0
  quick
}

compare_imputation_predictor_matrices <- function(analytic_matrix, quickpred_matrix, df, branch) {
  profile <- build_imputation_variable_profile(df)
  role_lookup <- profile$role
  names(role_lookup) <- profile$variable
  time_lookup <- profile$timepoint
  names(time_lookup) <- profile$variable

  analytic_flag <- analytic_matrix != 0
  quick_flag <- quickpred_matrix != 0
  pair_idx <- which(analytic_flag | quick_flag, arr.ind = TRUE)

  if (nrow(pair_idx) == 0) {
    return(data.frame(
      branch = character(0),
      target = character(0),
      predictor = character(0),
      analytic_selected = logical(0),
      quickpred_selected = logical(0),
      selection_status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  target <- rownames(analytic_matrix)[pair_idx[, "row"]]
  predictor <- colnames(analytic_matrix)[pair_idx[, "col"]]
  analytic_selected <- analytic_flag[pair_idx]
  quickpred_selected <- quick_flag[pair_idx]

  status <- ifelse(
    analytic_selected & quickpred_selected,
    "both",
    ifelse(analytic_selected, "analytic_only", "quickpred_only")
  )

  data.frame(
    branch = branch,
    target = target,
    target_role = unname(role_lookup[target]),
    target_timepoint = unname(time_lookup[target]),
    predictor = predictor,
    predictor_role = unname(role_lookup[predictor]),
    predictor_timepoint = unname(time_lookup[predictor]),
    analytic_selected = as.logical(analytic_selected),
    quickpred_selected = as.logical(quickpred_selected),
    selection_status = status,
    quickpred_uses_future_timepoint = unname(time_lookup[predictor] > time_lookup[target]),
    stringsAsFactors = FALSE
  )
}

summarise_predictor_matrix_comparison <- function(comparison_df, analytic_matrix, quickpred_matrix, branch) {
  quickpred_error <- attr(quickpred_matrix, "quickpred_error")
  if (is.null(quickpred_error)) {
    quickpred_error <- ""
  }
  data.frame(
    branch = branch,
    analytic_pairs = sum(analytic_matrix != 0),
    quickpred_pairs = sum(quickpred_matrix != 0),
    overlap_pairs = sum((analytic_matrix != 0) & (quickpred_matrix != 0)),
    analytic_only_pairs = sum((analytic_matrix != 0) & !(quickpred_matrix != 0)),
    quickpred_only_pairs = sum(!(analytic_matrix != 0) & (quickpred_matrix != 0)),
    quickpred_future_pairs = if (nrow(comparison_df) == 0) 0L else sum(comparison_df$quickpred_selected & comparison_df$quickpred_uses_future_timepoint, na.rm = TRUE),
    quickpred_error = quickpred_error,
    stringsAsFactors = FALSE
  )
}

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
