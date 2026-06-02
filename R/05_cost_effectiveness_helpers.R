###########################################################################
# R/05_cost_effectiveness_helpers.R
# Shared helpers for the BOFE cost-effectiveness analyses.
###########################################################################

source("R/utils.R")

library(dplyr)

# Drop rows with missing model inputs before fitting.
clean_model_data <- function(formula, data) {
  vars <- unique(all.vars(formula))
  vars <- vars[vars %in% names(data)]
  if (length(vars) == 0) return(NULL)

  keep <- complete.cases(data[, vars, drop = FALSE])
  numeric_vars <- vars[vapply(data[, vars, drop = FALSE], is.numeric, logical(1))]
  if (length(numeric_vars) > 0) {
    finite_ok <- apply(data[, numeric_vars, drop = FALSE], 1, function(row) all(is.finite(row)))
    keep <- keep & finite_ok
  }

  cleaned <- data[keep, , drop = FALSE]
  if (nrow(cleaned) == 0) return(NULL)
  cleaned
}

# Wrap glm() so the cost and QALY fits fail cleanly when data are incomplete.
fit_stable_glm <- function(formula, data, family, maxit = 100) {
  data <- clean_model_data(formula, data)
  if (is.null(data)) return(NULL)
  suppressWarnings(
    glm(
      formula = formula,
      data = data,
      family = family,
      control = glm.control(maxit = maxit, epsilon = 1e-08)
    )
  )
}

# Fit the same GLM across completed imputations and pool the coefficients with mice.
fit_pooled_mi_glm <- function(completed_sets, formula, family, maxit = 100) {
  fit_list <- lapply(seq_along(completed_sets), function(i) {
    fit <- fit_stable_glm(
      formula = formula,
      data = as.data.frame(completed_sets[[i]]),
      family = family,
      maxit = maxit
    )
    if (is.null(fit)) {
      stop("fit_pooled_mi_glm: model fit failed in imputation ", i, ".")
    }
    fit
  })

  mira_fit <- mice::as.mira(fit_list)
  pooled_fit <- mice::pool(mira_fit)
  pooled_summary <- summary(pooled_fit)
  pooled_summary$odds_ratio <- exp(pooled_summary$estimate)
  pooled_summary$ci_low <- exp(pooled_summary$estimate - qt(0.975, pooled_summary$df) * pooled_summary$std.error)
  pooled_summary$ci_high <- exp(pooled_summary$estimate + qt(0.975, pooled_summary$df) * pooled_summary$std.error)

  list(
    fits = fit_list,
    mira = mira_fit,
    pooled_fit = pooled_fit,
    pooled_summary = pooled_summary
  )
}

cost_model_spec <- function(cost_family = c("gamma_log", "gaussian_identity")) {
  cost_family <- match.arg(cost_family)
  if (cost_family == "gamma_log") {
    return(list(
      cost_family = cost_family,
      response = "total_cost_gamma",
      family = Gamma(link = "log"),
      model = "GLM_Gamma_log_cost",
      scale = "ratio",
      exponentiate_summary = TRUE
    ))
  }

  list(
    cost_family = cost_family,
    response = "total_cost",
    family = gaussian(link = "identity"),
    model = "GLM_Gaussian_identity_cost",
    scale = "difference",
    exponentiate_summary = FALSE
  )
}

extract_summary_term_row <- function(summary_df, pattern = "^group") {
  if (is.null(summary_df) || nrow(summary_df) == 0 || !"term" %in% names(summary_df)) {
    return(NULL)
  }
  idx <- grep(pattern, summary_df$term)
  if (length(idx) == 0) return(NULL)
  summary_df[idx[1], , drop = FALSE]
}

# Convert the cost-model group coefficient into an incremental cost estimate.
extract_incremental_cost_delta <- function(cost_fit, cost_family = c("gamma_log", "gaussian_identity")) {
  cost_family <- match.arg(cost_family)
  coef_names <- names(coef(cost_fit))
  group_term <- grep("^group", coef_names, value = TRUE)[1]
  if (is.na(group_term) || !nzchar(group_term)) return(NULL)

  coef_subset <- c("(Intercept)", group_term)
  if (!all(coef_subset %in% coef_names)) return(NULL)

  beta <- coef(cost_fit)[coef_subset]
  vc <- vcov(cost_fit)[coef_subset, coef_subset, drop = FALSE]
  if (any(!is.finite(beta)) || any(!is.finite(vc))) return(NULL)

  if (cost_family == "gamma_log") {
    baseline_cost <- exp(unname(beta[["(Intercept)"]]))
    intervention_cost <- exp(unname(beta[["(Intercept)"]]) + unname(beta[[group_term]]))
    incremental_cost <- intervention_cost - baseline_cost

    grad <- c(
      baseline_cost * (exp(unname(beta[[group_term]])) - 1),
      intervention_cost
    )
    incremental_cost_var <- as.numeric(t(grad) %*% vc %*% grad)
  } else {
    baseline_cost <- unname(beta[["(Intercept)"]])
    intervention_cost <- unname(beta[["(Intercept)"]]) + unname(beta[[group_term]])
    incremental_cost <- unname(beta[[group_term]])
    incremental_cost_var <- as.numeric(vcov(cost_fit)[group_term, group_term, drop = TRUE])
  }
  if (!is.finite(incremental_cost_var) || incremental_cost_var < 0) {
    incremental_cost_var <- NA_real_
  }

  list(
    baseline_cost = baseline_cost,
    intervention_cost = intervention_cost,
    incremental_cost = incremental_cost,
    incremental_cost_var = incremental_cost_var
  )
}

# Extract the adjusted mean QALY difference from the Gaussian model.
extract_incremental_qaly <- function(qaly_fit) {
  coef_names <- names(coef(qaly_fit))
  group_term <- grep("^group", coef_names, value = TRUE)[1]
  if (is.na(group_term) || !nzchar(group_term)) return(NULL)
  if (!group_term %in% coef_names) return(NULL)

  incremental_qaly <- unname(coef(qaly_fit)[group_term])
  incremental_qaly_var <- as.numeric(vcov(qaly_fit)[group_term, group_term, drop = FALSE])
  if (!is.finite(incremental_qaly_var) || incremental_qaly_var < 0) {
    incremental_qaly_var <- NA_real_
  }

  list(
    incremental_qaly = incremental_qaly,
    incremental_qaly_var = incremental_qaly_var
  )
}

# Pool a small coefficient set across imputations using Rubin's rules.
pool_glm_terms_rubin <- function(fit_list, terms) {
  if (length(fit_list) == 0 || length(terms) == 0) {
    return(NULL)
  }

  coef_mat <- lapply(fit_list, function(fit) {
    cf <- coef(fit)[terms]
    if (any(!is.finite(cf)) || anyNA(cf)) {
      return(NULL)
    }
    cf
  })
  if (any(vapply(coef_mat, is.null, logical(1)))) {
    return(NULL)
  }
  coef_mat <- do.call(rbind, coef_mat)
  rownames(coef_mat) <- NULL

  vcov_list <- lapply(fit_list, function(fit) {
    vc <- tryCatch(vcov(fit)[terms, terms, drop = FALSE], error = function(e) NULL)
    if (is.null(vc) || any(!is.finite(vc))) {
      return(NULL)
    }
    vc
  })
  if (any(vapply(vcov_list, is.null, logical(1)))) {
    return(NULL)
  }

  m <- nrow(coef_mat)
  qbar <- colMeans(coef_mat)
  ubar <- Reduce(`+`, vcov_list) / m
  centered <- sweep(coef_mat, 2, qbar, "-")
  bmat <- if (m > 1) {
    stats::cov(coef_mat)
  } else {
    matrix(0, nrow = length(terms), ncol = length(terms), dimnames = list(terms, terms))
  }
  total_cov <- ubar + (1 + 1 / m) * bmat

  list(
    qbar = qbar,
    total_cov = total_cov,
    terms = terms,
    n_imputations = m
  )
}

# Stratified bootstrap sample using the same patient IDs across imputations.
sample_bootstrap_patient_ids <- function(patient_level) {
  if (!"group" %in% names(patient_level) || !"patient" %in% names(patient_level)) {
    stop("sample_bootstrap_patient_ids: patient_level must contain patient and group columns.")
  }

  sample_arm_ids <- function(df_arm) {
    if (nrow(df_arm) == 0) {
      return(character(0))
    }
    as.character(df_arm$patient[sample(seq_len(nrow(df_arm)), nrow(df_arm), replace = TRUE)])
  }

  ig_ids <- sample_arm_ids(patient_level[patient_level$group == "ig (intervention group)", , drop = FALSE])
  cg_ids <- sample_arm_ids(patient_level[patient_level$group == "cg (control group)", , drop = FALSE])
  c(ig_ids, cg_ids)
}

# Fit the MI CEA models within one bootstrap sample and pool across imputations.
# cea_sets must already be patient-level frames, one per completed imputation.
bootstrap_mi_iteration_result <- function(
  i,
  cea_sets,
  base_patient_level,
  intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
  tariff = c("italian", "uk"),
  cost_family = c("gamma_log", "gaussian_identity"),
  model_family = "mi_bootstrap"
) {
  tariff <- match.arg(tariff)
  cost_family <- match.arg(cost_family)
  cost_spec <- cost_model_spec(cost_family)
  set.seed(i * 37)

  sampled_ids <- sample_bootstrap_patient_ids(base_patient_level)
  if (length(sampled_ids) == 0) {
    return(NULL)
  }

  boot_patient_levels <- lapply(cea_sets, function(set) {
    set <- as.data.frame(set)
    if (!"patient" %in% names(set)) {
      stop("bootstrap_mi_iteration_result: each CEA set must contain a patient column.")
    }
    set_ids <- as.character(set$patient)
    boot_idx <- match(sampled_ids, set_ids)
    if (anyNA(boot_idx)) {
      stop("bootstrap_mi_iteration_result: sampled patient IDs were not found in a CEA patient-level set.")
    }
    set[boot_idx, , drop = FALSE]
  })
  if (any(vapply(boot_patient_levels, nrow, integer(1)) == 0)) {
    return(NULL)
  }

  cost_fits <- lapply(seq_along(boot_patient_levels), function(idx) {
    fit_stable_glm(
      as.formula(paste0(cost_spec$response, " ~ group + age + gender")),
      boot_patient_levels[[idx]],
      cost_spec$family,
      maxit = 200
    )
  })
  qaly_fits <- lapply(seq_along(boot_patient_levels), function(idx) {
    fit_stable_glm(
      QALY_model ~ group + age + gender,
      boot_patient_levels[[idx]],
      gaussian(link = "identity"),
      maxit = 200
    )
  })

  if (any(vapply(cost_fits, is.null, logical(1))) || any(vapply(qaly_fits, is.null, logical(1)))) {
    return(NULL)
  }

  cost_group_term <- grep("^group", names(coef(cost_fits[[1]])), value = TRUE)[1]
  qaly_group_term <- grep("^group", names(coef(qaly_fits[[1]])), value = TRUE)[1]
  if (is.na(cost_group_term) || is.na(qaly_group_term) || !nzchar(cost_group_term) || !nzchar(qaly_group_term)) {
    return(NULL)
  }

  cost_pool <- pool_glm_terms_rubin(cost_fits, c("(Intercept)", cost_group_term))
  qaly_pool <- pool_glm_terms_rubin(qaly_fits, c("(Intercept)", qaly_group_term))
  if (is.null(cost_pool) || is.null(qaly_pool)) {
    return(NULL)
  }

  cost_intercept <- unname(cost_pool$qbar[["(Intercept)"]])
  cost_group <- unname(cost_pool$qbar[[cost_group_term]])
  if (cost_family == "gamma_log") {
    intervention_cost <- exp(cost_intercept + cost_group)
    baseline_cost <- exp(cost_intercept)
    incremental_cost <- intervention_cost - baseline_cost
    cost_cov <- cost_pool$total_cov
    cost_grad <- c(
      baseline_cost * (exp(cost_group) - 1),
      intervention_cost
    )
    names(cost_grad) <- c("(Intercept)", cost_group_term)
    incremental_cost_var <- as.numeric(t(cost_grad) %*% cost_cov %*% cost_grad)
  } else {
    intervention_cost <- cost_intercept + cost_group
    baseline_cost <- cost_intercept
    incremental_cost <- cost_group
    incremental_cost_var <- as.numeric(cost_pool$total_cov[cost_group_term, cost_group_term, drop = TRUE])
  }
  if (!is.finite(incremental_cost_var) || incremental_cost_var < 0) {
    incremental_cost_var <- NA_real_
  }

  qaly_intercept <- unname(qaly_pool$qbar[["(Intercept)"]])
  qaly_group <- unname(qaly_pool$qbar[[qaly_group_term]])
  qaly_cov <- qaly_pool$total_cov
  incremental_qaly_var <- as.numeric(qaly_cov[qaly_group_term, qaly_group_term, drop = TRUE])
  if (!is.finite(incremental_qaly_var) || incremental_qaly_var < 0) {
    incremental_qaly_var <- NA_real_
  }

  data.frame(
    iteration = i,
    model_family = model_family,
    cost_intercept = cost_intercept,
    cost_group_effect = cost_group,
    qaly_intercept = qaly_intercept,
    qaly_group = qaly_group,
    incremental_qaly = qaly_group,
    incremental_qaly_var = incremental_qaly_var,
    incremental_qaly_se = sqrt(pmax(incremental_qaly_var, 0)),
    baseline_cost = baseline_cost,
    intervention_cost = intervention_cost,
    incremental_cost = incremental_cost,
    incremental_cost_var = incremental_cost_var,
    incremental_cost_se = sqrt(pmax(incremental_cost_var, 0)),
    n_imputations = length(boot_patient_levels),
    stringsAsFactors = FALSE
  )
}

# Run the nested MI bootstrap sequentially.
run_mi_bootstrap_serial <- function(
  cea_sets,
  base_patient_level,
  num_iter = BOOTSTRAP_ITERATIONS,
  intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
  tariff = c("italian", "uk"),
  cost_family = c("gamma_log", "gaussian_identity"),
  model_family = "mi_bootstrap"
) {
  tariff <- match.arg(tariff)
  cost_family <- match.arg(cost_family)
  empty_out <- data.frame(
    iteration = integer(0),
    model_family = character(0),
    cost_intercept = numeric(0),
    cost_group_effect = numeric(0),
    qaly_intercept = numeric(0),
    qaly_group = numeric(0),
    incremental_qaly = numeric(0),
    incremental_qaly_var = numeric(0),
    incremental_qaly_se = numeric(0),
    baseline_cost = numeric(0),
    intervention_cost = numeric(0),
    incremental_cost = numeric(0),
    incremental_cost_var = numeric(0),
    incremental_cost_se = numeric(0),
    n_imputations = integer(0),
    stringsAsFactors = FALSE
  )
  rows <- vector("list", num_iter)
  n_rows <- 0L

  for (i in seq_len(num_iter)) {
    if (i == 1 || i %% 500 == 0 || i == num_iter) {
      pipeline_phase_info(
        "05_cost_effectiveness",
        sprintf("bootstrap iteration %d/%d", i, num_iter)
      )
    }

    boot_row <- bootstrap_mi_iteration_result(
      i = i,
      cea_sets = cea_sets,
      base_patient_level = base_patient_level,
      intervention_cost_per_consultation = intervention_cost_per_consultation,
      tariff = tariff,
      cost_family = cost_family,
      model_family = model_family
    )
    if (!is.null(boot_row)) {
      n_rows <- n_rows + 1L
      rows[[n_rows]] <- boot_row
    }
  }

  if (n_rows == 0) {
    return(empty_out)
  }
  out <- bind_rows(rows[seq_len(n_rows)])
  out$icer <- out$incremental_cost / out$incremental_qaly
  out
}

# Resample patients within arm, then refit the CEA models for one bootstrap draw.
bootstrap_iteration_result <- function(
  i,
  patient_level,
  model_family = "glm",
  intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
  cost_family = c("gamma_log", "gaussian_identity")
) {
  cost_family <- match.arg(cost_family)
  cost_spec <- cost_model_spec(cost_family)
  set.seed(i * 37)

  sample_arm <- function(df_arm) {
    if (nrow(df_arm) == 0) return(df_arm)
    df_arm[sample(seq_len(nrow(df_arm)), nrow(df_arm), replace = TRUE), , drop = FALSE]
  }

  sample_ig <- sample_arm(patient_level[patient_level$group == "ig (intervention group)", , drop = FALSE])
  sample_cg <- sample_arm(patient_level[patient_level$group == "cg (control group)", , drop = FALSE])
  sample_patient <- rbind(sample_ig, sample_cg)

  boot_cost <- tryCatch(
    fit_stable_glm(
      as.formula(paste0(cost_spec$response, " ~ group + age + gender")),
      sample_patient,
      cost_spec$family,
      maxit = 200
    ),
    error = function(e) NULL
  )
  boot_qaly <- tryCatch(
    fit_stable_glm(QALY_model ~ group + age + gender, sample_patient, gaussian(link = "identity"), maxit = 100),
    error = function(e) NULL
  )

  if (is.null(boot_cost) || is.null(boot_qaly)) return(NULL)
  if (!isTRUE(boot_cost$converged)) return(NULL)

  group_term <- grep("^groupig", names(coef(boot_cost)), value = TRUE)
  qaly_group_term <- grep("^groupig", names(coef(boot_qaly)), value = TRUE)
  if (length(group_term) == 0 || length(qaly_group_term) == 0) return(NULL)

  cost_delta <- extract_incremental_cost_delta(boot_cost, cost_family = cost_family)
  qaly_delta <- extract_incremental_qaly(boot_qaly)
  if (is.null(cost_delta) || is.null(qaly_delta)) return(NULL)

  data.frame(
    iteration = i,
    model_family = model_family,
    cost_intercept = unname(coef(boot_cost)[["(Intercept)"]]),
    cost_group_effect = unname(coef(boot_cost)[group_term[1]]),
    qaly_intercept = unname(coef(boot_qaly)[["(Intercept)"]]),
    incremental_qaly = qaly_delta$incremental_qaly,
    incremental_qaly_var = qaly_delta$incremental_qaly_var,
    incremental_qaly_se = sqrt(pmax(qaly_delta$incremental_qaly_var, 0)),
    baseline_cost = cost_delta$baseline_cost,
    intervention_cost = cost_delta$intervention_cost,
    incremental_cost = cost_delta$incremental_cost,
    incremental_cost_var = cost_delta$incremental_cost_var,
    incremental_cost_se = sqrt(pmax(cost_delta$incremental_cost_var, 0)),
    stringsAsFactors = FALSE
  )
}

# Run the bootstrap sequentially.
run_bootstrap_serial <- function(
    patient_level,
    num_iter = BOOTSTRAP_ITERATIONS,
    model_family = "glm",
    cost_family = c("gamma_log", "gaussian_identity")) {
  cost_family <- match.arg(cost_family)
  out <- data.frame(
    iteration = integer(0),
    model_family = character(0),
    cost_intercept = numeric(0),
    cost_group_effect = numeric(0),
    qaly_intercept = numeric(0),
    incremental_qaly = numeric(0),
    incremental_qaly_var = numeric(0),
    incremental_qaly_se = numeric(0),
    baseline_cost = numeric(0),
    intervention_cost = numeric(0),
    incremental_cost = numeric(0),
    incremental_cost_var = numeric(0),
    incremental_cost_se = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(num_iter)) {
    if (i == 1 || i %% 500 == 0 || i == num_iter) {
      pipeline_phase_info(
        "05_cost_effectiveness",
        sprintf("bootstrap iteration %d/%d", i, num_iter)
      )
    }

    boot_row <- bootstrap_iteration_result(
      i,
      patient_level,
      model_family = model_family,
      cost_family = cost_family
    )
    if (!is.null(boot_row)) {
      out <- rbind(out, boot_row)
    }
  }

  if (nrow(out) == 0) return(out)
  out$icer <- out$incremental_cost / out$incremental_qaly
  out
}

# Combine point estimates, uncertainty, and acceptability into one summary table.
build_cea_summary_rows <- function(
    bootstrap_results,
    wtp_threshold = WTP_THRESHOLD_EUR_PER_QALY,
    use_rubin = FALSE) {
  thresholds <- seq(0, 40000, by = 100)
  if (is.null(bootstrap_results) || nrow(bootstrap_results) == 0) {
    return(list(summary = data.frame(), acceptability = data.frame()))
  }

  acceptability_curve <- do.call(rbind, lapply(split(bootstrap_results, bootstrap_results$model_family), function(df) {
    data.frame(
      model_family = unique(df$model_family),
      threshold_eur_per_qaly = thresholds,
      probability_acceptable = vapply(
        thresholds,
        function(threshold) mean((df$incremental_qaly * threshold - df$incremental_cost) > 0, na.rm = TRUE),
        numeric(1)
      ),
      stringsAsFactors = FALSE
    )
  }))

  summary_rows <- do.call(rbind, lapply(split(bootstrap_results, bootstrap_results$model_family), function(df) {
    pooled_cost <- if (isTRUE(use_rubin)) {
      pool_mi_uncertainty(df, "incremental_cost", "incremental_cost_var")
    } else {
      pool_bootstrap_uncertainty(df, "incremental_cost", "incremental_cost_var")
    }
    pooled_qaly <- if (isTRUE(use_rubin)) {
      pool_mi_uncertainty(df, "incremental_qaly", "incremental_qaly_var")
    } else {
      pool_bootstrap_uncertainty(df, "incremental_qaly", "incremental_qaly_var")
    }
    icer_point <- pooled_cost$estimate / pooled_qaly$estimate
    if (!is.finite(icer_point)) icer_point <- NA_real_
    icer_low <- as.numeric(stats::quantile(df$icer, probs = 0.025, na.rm = TRUE, names = FALSE))
    icer_high <- as.numeric(stats::quantile(df$icer, probs = 0.975, na.rm = TRUE, names = FALSE))
    acceptability <- acceptability_curve$probability_acceptable[
      acceptability_curve$model_family == unique(df$model_family) &
        acceptability_curve$threshold_eur_per_qaly == wtp_threshold
    ]
    if (length(acceptability) == 0) acceptability <- NA_real_

    data.frame(
      model_family = unique(df$model_family),
      metric = c("incremental_cost", "incremental_qaly", "ICER", paste0("probability_acceptable_at_", wtp_threshold)),
      estimate = c(
        pooled_cost$estimate,
        pooled_qaly$estimate,
        icer_point,
        acceptability
      ),
      pooled_ci_lower = c(
        pooled_cost$lower_95,
        pooled_qaly$lower_95,
        NA_real_,
        NA_real_
      ),
      pooled_ci_upper = c(
        pooled_cost$upper_95,
        pooled_qaly$upper_95,
        NA_real_,
        NA_real_
      ),
      bootstrap_ci_lower = c(
        pooled_cost$bootstrap_lower_95,
        pooled_qaly$bootstrap_lower_95,
        icer_low,
        NA_real_
      ),
      bootstrap_ci_upper = c(
        pooled_cost$bootstrap_upper_95,
        pooled_qaly$bootstrap_upper_95,
        icer_high,
        NA_real_
      ),
      within_variance = c(
        pooled_cost$within_variance,
        pooled_qaly$within_variance,
        NA_real_,
        NA_real_
      ),
      between_variance = c(
        pooled_cost$between_variance,
        pooled_qaly$between_variance,
        NA_real_,
        NA_real_
      ),
      pooled_variance = c(
        pooled_cost$pooled_variance,
        pooled_qaly$pooled_variance,
        NA_real_,
        NA_real_
      ),
      pooled_std_error = c(
        pooled_cost$pooled_std_error,
        pooled_qaly$pooled_std_error,
        NA_real_,
        NA_real_
      ),
      n_boot = c(
        pooled_cost$n_boot,
        pooled_qaly$n_boot,
        nrow(df),
        nrow(df)
      ),
      uncertainty_method = c(
        pooled_cost$uncertainty_method,
        pooled_qaly$uncertainty_method,
        "bootstrap_percentile",
        "bootstrap_probability"
      ),
      stringsAsFactors = FALSE
    )
  }))

  list(summary = summary_rows, acceptability = acceptability_curve)
}

adjust_nested_cea_for_intervention_cost <- function(
    cea_results,
    intervention_cost_per_consultation,
    base_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    branch_label = paste0("full_mice_cost_", intervention_cost_per_consultation)) {
  if (is.null(cea_results) || is.null(cea_results$bootstrap_results)) {
    stop("adjust_nested_cea_for_intervention_cost: expected a nested CEA result object.")
  }

  delta <- 2 * (intervention_cost_per_consultation - base_cost_per_consultation)
  boot <- cea_results$bootstrap_results
  boot$model_family <- branch_label
  boot$intervention_cost <- boot$intervention_cost + delta
  boot$incremental_cost <- boot$incremental_cost + delta
  if ("cost_group_effect" %in% names(boot)) {
    boot$cost_group_effect <- boot$cost_group_effect + delta
  }
  boot$icer <- boot$incremental_cost / boot$incremental_qaly

  pooled <- build_cea_summary_rows(boot, use_rubin = TRUE)
  list(
    bootstrap_results = boot,
    acceptability_curve = pooled$acceptability,
    summary = pooled$summary,
    branch = branch_label
  )
}

# Build the main MI CEA branch with a bootstrap over the same sampled patients in each imputation.
run_nested_mi_cea_branch <- function(
    mids_path,
    branch_label,
    intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    economic_data = NULL,
    tariff = c("italian", "uk"),
    bootstrap_iterations = BOOTSTRAP_ITERATIONS,
    cost_family = c("gamma_log", "gaussian_identity")) {
  tariff <- match.arg(tariff)
  cost_family <- match.arg(cost_family)
  cost_spec <- cost_model_spec(cost_family)
  if (!file.exists(mids_path)) {
    pipeline_phase_info(
      "05_cost_effectiveness",
      sprintf("skipping %s MI bootstrap branch because %s is missing", branch_label, mids_path)
    )
    return(NULL)
  }

  pipeline_phase_info(
    "05_cost_effectiveness",
    sprintf("running %s MI bootstrap branch from %s", branch_label, basename(mids_path))
  )

  mids_obj <- readRDS(mids_path)
  completed_sets <- mice::complete(mids_obj, action = "all", include = FALSE)
  if (length(completed_sets) == 0) {
    stop("05_cost_effectiveness: no completed imputations were available for ", branch_label, ".")
  }

  cea_sets <- lapply(seq_along(completed_sets), function(i) {
    cea_i <- prepare_cea_patient_level(
      as.data.frame(completed_sets[[i]]),
      require_cost_data = FALSE,
      economic_data = economic_data,
      intervention_cost_per_consultation = intervention_cost_per_consultation,
      tariff = tariff
    )
    if (nrow(cea_i) == 0) {
      stop("05_cost_effectiveness: empty CEA cohort in ", branch_label, " imputation ", i, ".")
    }
    cea_i
  })

  cost_bundle <- fit_pooled_mi_glm(
    completed_sets = cea_sets,
    formula = as.formula(paste0(cost_spec$response, " ~ group + age + gender")),
    family = cost_spec$family,
    maxit = 200
  )
  if (!isTRUE(cost_spec$exponentiate_summary)) {
    cost_bundle$pooled_summary$odds_ratio <- NA_real_
    cost_bundle$pooled_summary$ci_low <- cost_bundle$pooled_summary$estimate - qt(0.975, cost_bundle$pooled_summary$df) * cost_bundle$pooled_summary$std.error
    cost_bundle$pooled_summary$ci_high <- cost_bundle$pooled_summary$estimate + qt(0.975, cost_bundle$pooled_summary$df) * cost_bundle$pooled_summary$std.error
  }
  qaly_bundle <- fit_pooled_mi_glm(
    completed_sets = cea_sets,
    formula = QALY_model ~ group + age + gender,
    family = gaussian(link = "identity"),
    maxit = 200
  )

  bootstrap_results <- run_mi_bootstrap_serial(
    cea_sets = cea_sets,
    base_patient_level = cea_sets[[1]],
    num_iter = bootstrap_iterations,
    intervention_cost_per_consultation = intervention_cost_per_consultation,
    tariff = tariff,
    cost_family = cost_family,
    model_family = branch_label
  )
  if (nrow(bootstrap_results) == 0) {
    stop("05_cost_effectiveness: nested MI bootstrap produced no usable iterations for ", branch_label, ".")
  }

  pooled <- build_cea_summary_rows(bootstrap_results, use_rubin = TRUE)
  cost_summary <- cost_bundle$pooled_summary
  cost_summary$estimate_exp <- if (isTRUE(cost_spec$exponentiate_summary)) exp(cost_summary$estimate) else NA_real_
  cost_summary$odds_ratio <- if (isTRUE(cost_spec$exponentiate_summary)) exp(cost_summary$estimate) else NA_real_
  cost_summary <- cost_summary %>%
    mutate(
      model = cost_spec$model,
      model_family = branch_label,
      outcome = "cost",
      estimate_exp = estimate_exp
    )

  qaly_summary <- qaly_bundle$pooled_summary
  if (!"estimate_exp" %in% names(qaly_summary)) {
    qaly_summary$estimate_exp <- NA_real_
  }
  qaly_summary <- qaly_summary %>%
    mutate(
      model = "GLM_Gaussian_identity_QALY",
      model_family = branch_label,
      outcome = "qaly",
      estimate_exp = NA_real_
    )

  model_summaries <- bind_rows(cost_summary, qaly_summary)
  cost_term_row <- extract_summary_term_row(cost_bundle$pooled_summary, "^group")
  qaly_term_row <- extract_summary_term_row(qaly_bundle$pooled_summary, "^group")
  cost_estimate_exp <- if (isTRUE(cost_spec$exponentiate_summary) && !is.null(cost_term_row)) {
    cost_term_row$odds_ratio
  } else {
    NA_real_
  }

  list(
    patient_level = cea_sets[[1]],
    imputation_results = bootstrap_results,
    acceptability_curve = pooled$acceptability,
    summary = pooled$summary,
    model_summaries = model_summaries,
    cea_model_comparison = data.frame(
      contrast = "intervention_vs_control",
      model_family = branch_label,
      outcome = c("cost", "qaly"),
      model = c(cost_spec$model, "GLM_Gaussian_identity_QALY"),
      term = c("group", "group"),
      scale = c(cost_spec$scale, "difference"),
      estimate = c(
        if (!is.null(cost_term_row)) cost_term_row$estimate else NA_real_,
        if (!is.null(qaly_term_row)) qaly_term_row$estimate else NA_real_
      ),
      estimate_exp = c(
        cost_estimate_exp,
        NA_real_
      ),
      std_error = c(
        if (!is.null(cost_term_row)) cost_term_row$std.error else NA_real_,
        if (!is.null(qaly_term_row)) qaly_term_row$std.error else NA_real_
      ),
      p_value = c(
        if (!is.null(cost_term_row) && "p.value" %in% names(cost_term_row)) cost_term_row$p.value else NA_real_,
        if (!is.null(qaly_term_row) && "p.value" %in% names(qaly_term_row)) qaly_term_row$p.value else NA_real_
      ),
      stringsAsFactors = FALSE
    ),
    bootstrap_results = bootstrap_results,
    branch = branch_label
  )
}

# Build the sensitivity CEA branch directly from a wide patient-level frame.
run_wide_cea_branch <- function(
    wide_df,
    branch_label,
    intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    model_family = "wide",
    num_iter = BOOTSTRAP_ITERATIONS,
    economic_data = NULL,
    tariff = c("italian", "uk"),
    cost_family = c("gamma_log", "gaussian_identity")) {
  tariff <- match.arg(tariff)
  cost_family <- match.arg(cost_family)
  cost_spec <- cost_model_spec(cost_family)
  cea_df <- prepare_cea_patient_level(
    wide_df,
    require_cost_data = TRUE,
    economic_data = economic_data,
    intervention_cost_per_consultation = intervention_cost_per_consultation,
    tariff = tariff
  )

  cost_model <- fit_stable_glm(
    as.formula(paste0(cost_spec$response, " ~ group + age + gender")),
    cea_df,
    cost_spec$family,
    maxit = 200
  )
  qaly_model <- fit_stable_glm(
    QALY_model ~ group + age + gender,
    cea_df,
    gaussian(link = "identity"),
    maxit = 200
  )

  if (is.null(cost_model) || is.null(qaly_model)) {
    stop("run_wide_cea_branch: failed to fit GLM models for ", branch_label, ".")
  }

  model_summaries <- bind_rows(
    summarise_model_terms(cost_model, cost_spec$model, exponentiate = cost_spec$exponentiate_summary) %>%
      mutate(model_family = model_family, outcome = "cost"),
    summarise_model_terms(qaly_model, "GLM_Gaussian_identity_QALY", exponentiate = FALSE) %>%
      mutate(model_family = model_family, outcome = "qaly")
  )
  if (!"estimate_exp" %in% names(model_summaries)) {
    model_summaries$estimate_exp <- NA_real_
  }

  cea_model_comparison <- model_summaries %>%
    filter(grepl("^group", term)) %>%
    mutate(
      contrast = "intervention_vs_control",
      scale = ifelse(outcome == "cost", cost_spec$scale, "difference")
    ) %>%
    select(
      contrast,
      model_family,
      outcome,
      model,
      term,
      scale,
      estimate,
      estimate_exp,
      std_error,
      p_value
    )

  bootstrap_results <- run_bootstrap_serial(
    cea_df,
    num_iter = num_iter,
    model_family = model_family,
    cost_family = cost_family
  )

  if (is.null(bootstrap_results) || nrow(bootstrap_results) == 0) {
    stop("run_wide_cea_branch: bootstrap produced no usable iterations for ", branch_label, ".")
  }

  pooled <- build_cea_summary_rows(bootstrap_results, use_rubin = FALSE)

  list(
    patient_level = cea_df,
    cost_model = cost_model,
    qaly_model = qaly_model,
    model_summaries = model_summaries,
    cea_model_comparison = cea_model_comparison,
    bootstrap_results = bootstrap_results,
    acceptability_curve = pooled$acceptability,
    summary = pooled$summary,
    branch = branch_label
  )
}
