# R/imputation_core_helpers.R
# Shared imputation predictor-matrix and simple-imputation helpers.

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

build_basic_mice_predictors <- function(df, id_col = "patient", group_col = "group") {
  pred <- matrix(0, nrow = ncol(df), ncol = ncol(df), dimnames = list(names(df), names(df)))

  baseline_predictors <- intersect(method_config("imputation", "basic_predictors"), names(df))
  if (length(baseline_predictors) > 0) {
    pred[, baseline_predictors] <- 1
  }

  lagged_control_pairs <- method_config("imputation", "basic_lagged_pairs")
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
