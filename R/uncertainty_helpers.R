# R/uncertainty_helpers.R
# Bootstrap and multiple-imputation uncertainty pooling helpers.

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

