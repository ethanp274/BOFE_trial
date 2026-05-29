###########################################################################
# R/05_cost_effectiveness.R
# Purpose: Legacy-faithful complete-case cost-effectiveness analysis.
#
# This rewrite reconstructs the original CEA cohort logic:
#   - baseline group and disease are fixed from D1.4_0 / D1.3_0
#   - the cost-complete cohort is defined as in the legacy script
#   - one patient-level row is retained per patient
#   - bootstrap resampling is stratified by treatment arm only
#   - GLM models are used for CEA inference; GEE is not fitted here
#
# Inputs:
#   - data_processed/complete_cases.rds
#   - data_processed/economic_data.rds
#   - outputs/models_mixed_imputed.rds
#   - outputs/models_gee_imputed.rds
# Outputs:
#   - outputs/cea_patient_level.csv
#   - outputs/cea_longitudinal.csv
#   - outputs/cea_model_summaries.csv
#   - outputs/cea_model_comparison.csv
#   - outputs/cea_bootstrap_results.csv
#   - outputs/cea_acceptability_curve.csv
#   - outputs/cea_summary.csv
#   - outputs/effectiveness_model_comparison.csv
#   - outputs/effectiveness_12mo_comparison.csv
#   - outputs/manuscript_results_summary.csv
#   - outputs/cea_models.rds
###########################################################################

source("R/utils.R")

library(dplyr)

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

pipeline_started <- pipeline_phase_start(
  "05_cost_effectiveness",
  "reconstructing the legacy cost-complete cohort and bootstrap summaries"
)

complete_cases <- readRDS("data_processed/complete_cases.rds")
economic_data_path <- "data_processed/economic_data.rds"
if (!file.exists(economic_data_path)) {
  stop("Missing ", economic_data_path, ". Run R/01_cleaning.R first.")
}
economic_data <- readRDS(economic_data_path)

complete_cases <- add_analysis_derivations(complete_cases)
if (!all(COST_SUMMARY_COLUMNS %in% names(complete_cases))) {
  complete_cases <- attach_cost_summaries(complete_cases, economic_data)
}

legacy_cost_complete_ids <- legacy_cost_complete_patient_ids(complete_cases, economic_data)
cea_df <- prepare_legacy_cea_patient_level(complete_cases, economic_data)
cea_long <- make_legacy_cea_longitudinal_data(complete_cases) %>%
  filter(as.character(patient) %in% legacy_cost_complete_ids)

if (nrow(cea_df) == 0) {
  stop("05_cost_effectiveness: the reconstructed legacy CEA cohort is empty.")
}
if (anyDuplicated(cea_df$patient)) {
  stop("05_cost_effectiveness: duplicated patient IDs were created in the CEA cohort.")
}
if (nrow(cea_df) != length(legacy_cost_complete_ids)) {
  stop("05_cost_effectiveness: reconstructed CEA cohort size does not match the legacy patient set.")
}

pipeline_phase_info(
  "05_cost_effectiveness",
  sprintf(
    "legacy CEA cohort rebuilt: %d patients (%d intervention, %d control)",
    nrow(cea_df),
    sum(cea_df$group == "ig (intervention group)", na.rm = TRUE),
    sum(cea_df$group == "cg (control group)", na.rm = TRUE)
  )
)

cea_df$group <- factor(cea_df$group, levels = GROUP_LEVELS)
cea_df$age <- factor(cea_df$age)
cea_df$gender <- factor(cea_df$gender)

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

mixed_model_path <- "outputs/models_mixed_imputed.rds"
gee_model_path <- "outputs/models_gee_imputed.rds"
if (!file.exists(mixed_model_path)) stop("Missing ", mixed_model_path, ". Run R/04_models.R first.")
if (!file.exists(gee_model_path)) stop("Missing ", gee_model_path, ". Run R/04b_gee.R first.")

mixed_results <- readRDS(mixed_model_path)
gee_results <- readRDS(gee_model_path)
pipeline_phase_info("05_cost_effectiveness", "loading mixed-effects and GEE effectiveness outputs")

effectiveness_mixed <- mixed_results$timepoint_effects %>%
  mutate(model = "mixed_effects", analysis = "effectiveness")
effectiveness_gee <- gee_results$gee_timepoint_effects %>%
  mutate(model = "gee", analysis = "effectiveness")

effectiveness_comparison <- bind_rows(effectiveness_mixed, effectiveness_gee) %>%
  select(analysis, model, time, log_or, odds_ratio, ci_low, ci_high, n_imputations)

effectiveness_12mo_comparison <- effectiveness_comparison %>%
  filter(time == 12)

cost_model <- glm(
  total_cost_gamma ~ group + age + gender,
  data = cea_df,
  family = Gamma(link = "log"),
  control = glm.control(maxit = 100, epsilon = 1e-08)
)

qaly_model <- glm(
  QALY_model ~ group + age + gender,
  data = cea_df,
  family = gaussian(link = "identity"),
  control = glm.control(maxit = 100, epsilon = 1e-08)
)

glm_cost_summary <- summarise_model_terms(cost_model, "GLM_Gamma_log_cost", exponentiate = TRUE) %>%
  mutate(model_family = "glm", outcome = "cost")
glm_qaly_summary <- summarise_model_terms(qaly_model, "GLM_Gaussian_identity_QALY", exponentiate = FALSE) %>%
  mutate(model_family = "glm", outcome = "qaly")

model_summaries <- bind_rows(glm_cost_summary, glm_qaly_summary) %>%
  mutate(
    estimate_exp = ifelse(
      is.na(estimate_exp) & outcome == "cost",
      exp(estimate),
      estimate_exp
    )
  )

pipeline_phase_info("05_cost_effectiveness", "fitting legacy GLM cost-effectiveness models")

cea_model_comparison <- model_summaries %>%
  filter(grepl("^group", term)) %>%
  mutate(
    contrast = "intervention_vs_control",
    scale = ifelse(outcome == "cost", "ratio", "difference")
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

bootstrap_iteration_result <- function(i, patient_level) {
  set.seed(i * 37)

  sample_arm <- function(df_arm) {
    if (nrow(df_arm) == 0) return(df_arm)
    df_arm[sample(seq_len(nrow(df_arm)), nrow(df_arm), replace = TRUE), , drop = FALSE]
  }

  sample_ig <- sample_arm(patient_level[patient_level$group == "ig (intervention group)", , drop = FALSE])
  sample_cg <- sample_arm(patient_level[patient_level$group == "cg (control group)", , drop = FALSE])
  sample_patient <- rbind(sample_ig, sample_cg)

  boot_cost <- tryCatch(
    fit_stable_glm(total_cost_gamma ~ group + age + gender, sample_patient, Gamma(link = "log"), maxit = 200),
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

  cost_intercept <- unname(coef(boot_cost)[["(Intercept)"]])
  cost_group_log_ratio <- unname(coef(boot_cost)[group_term[1]])
  qaly_intercept <- unname(coef(boot_qaly)[["(Intercept)"]])
  incremental_qaly <- unname(coef(boot_qaly)[qaly_group_term[1]])
  baseline_cost <- exp(cost_intercept)
  intervention_cost <- exp(cost_intercept) * exp(cost_group_log_ratio)
  incremental_cost <- intervention_cost - baseline_cost

  data.frame(
    iteration = i,
    model_family = "glm",
    cost_intercept = cost_intercept,
    cost_group_log_ratio = cost_group_log_ratio,
    qaly_intercept = qaly_intercept,
    incremental_qaly = incremental_qaly,
    baseline_cost = baseline_cost,
    intervention_cost = intervention_cost,
    incremental_cost = incremental_cost,
    stringsAsFactors = FALSE
  )
}

run_bootstrap_serial <- function(patient_level, num_iter = BOOTSTRAP_ITERATIONS) {
  out <- data.frame(
    iteration = integer(0),
    model_family = character(0),
    cost_intercept = numeric(0),
    cost_group_log_ratio = numeric(0),
    qaly_intercept = numeric(0),
    incremental_qaly = numeric(0),
    baseline_cost = numeric(0),
    intervention_cost = numeric(0),
    incremental_cost = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(num_iter)) {
    if (i == 1 || i %% 500 == 0 || i == num_iter) {
      pipeline_phase_info(
        "05_cost_effectiveness",
        sprintf("bootstrap iteration %d/%d", i, num_iter)
      )
    }

    boot_row <- bootstrap_iteration_result(i, patient_level)
    if (!is.null(boot_row)) {
      out <- rbind(out, boot_row)
    }
  }

  if (nrow(out) == 0) return(out)
  out$icer <- out$incremental_cost / out$incremental_qaly
  out
}

run_bootstrap_parallel <- function(patient_level, num_iter = BOOTSTRAP_ITERATIONS, workers = NULL) {
  if (is.null(workers)) {
    workers <- getOption("bofe.bootstrap_workers", NULL)
  }
  if (is.null(workers)) {
    env_workers <- Sys.getenv("BOFE_BOOTSTRAP_WORKERS", "")
    if (nzchar(env_workers)) {
      workers <- suppressWarnings(as.integer(env_workers))
    }
  }
  if (is.null(workers) || is.na(workers)) {
    workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  }
  workers <- min(as.integer(workers), num_iter)

  if (workers <= 1L) {
    pipeline_phase_info("05_cost_effectiveness", "parallel bootstrap disabled because only one worker is available")
    return(run_bootstrap_serial(patient_level, num_iter = num_iter))
  }

  pipeline_phase_info(
    "05_cost_effectiveness",
    sprintf("starting parallel bootstrap with %d workers", workers)
  )

  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(
    cl,
    varlist = c(
      "patient_level",
      "bootstrap_iteration_result",
      "clean_model_data",
      "fit_stable_glm"
    ),
    envir = environment()
  )

  iterations <- seq_len(num_iter)
  batch_size <- max(1L, ceiling(num_iter / (workers * 8L)))
  batches <- split(iterations, ceiling(iterations / batch_size))
  batch_results <- vector("list", length(batches))

  for (idx in seq_along(batches)) {
    batch <- batches[[idx]]
    batch_results[[idx]] <- parallel::parLapply(cl, batch, function(i) {
      bootstrap_iteration_result(i, patient_level)
    })
    batch_start <- min(batch)
    batch_end <- max(batch)
    pipeline_phase_info(
      "05_cost_effectiveness",
      sprintf("finished bootstrap iterations %d-%d/%d", batch_start, batch_end, num_iter)
    )
  }

  flat_results <- Filter(Negate(is.null), unlist(batch_results, recursive = FALSE))
  if (length(flat_results) == 0) return(data.frame())
  out <- do.call(rbind, flat_results)
  if (is.null(out) || nrow(out) == 0) return(out)
  out$icer <- out$incremental_cost / out$incremental_qaly
  out
}

use_parallel_bootstrap <- isTRUE(getOption("bofe.parallel_bootstrap", FALSE)) ||
  identical(tolower(Sys.getenv("BOFE_PARALLEL_BOOTSTRAP", "")), "true")

bootstrap_results <- if (use_parallel_bootstrap) {
  run_bootstrap_parallel(cea_df)
} else {
  run_bootstrap_serial(cea_df)
}

if (is.null(bootstrap_results) || nrow(bootstrap_results) == 0) {
  stop("05_cost_effectiveness: bootstrap produced no usable iterations.")
}

pipeline_phase_info("05_cost_effectiveness", "summarising bootstrap uncertainty and acceptability curves")

thresholds <- seq(0, 40000, by = 10)
acceptability_curve <- do.call(rbind, lapply(split(bootstrap_results, bootstrap_results$model_family), function(df) {
  data.frame(
    model_family = unique(df$model_family),
    threshold_eur_per_qaly = thresholds,
    probability_acceptable = vapply(
      thresholds,
      function(threshold) {
        mean((df$incremental_qaly * threshold - df$incremental_cost) > 0, na.rm = TRUE)
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}))

summary_rows <- do.call(rbind, lapply(split(bootstrap_results, bootstrap_results$model_family), function(df) {
  data.frame(
    model_family = unique(df$model_family),
    metric = c("incremental_cost", "incremental_qaly", "ICER", "probability_acceptable_at_25000"),
    estimate = c(
      mean(df$incremental_cost, na.rm = TRUE),
      mean(df$incremental_qaly, na.rm = TRUE),
      mean(df$icer, na.rm = TRUE),
      acceptability_curve$probability_acceptable[
        acceptability_curve$model_family == unique(df$model_family) &
          acceptability_curve$threshold_eur_per_qaly == WTP_THRESHOLD_EUR_PER_QALY
      ]
    ),
    lower_95 = c(
      mean(df$incremental_cost, na.rm = TRUE) - 1.96 * sd(df$incremental_cost, na.rm = TRUE),
      mean(df$incremental_qaly, na.rm = TRUE) - 1.96 * sd(df$incremental_qaly, na.rm = TRUE),
      mean(df$icer, na.rm = TRUE) - 1.96 * sd(df$icer, na.rm = TRUE),
      NA_real_
    ),
    upper_95 = c(
      mean(df$incremental_cost, na.rm = TRUE) + 1.96 * sd(df$incremental_cost, na.rm = TRUE),
      mean(df$incremental_qaly, na.rm = TRUE) + 1.96 * sd(df$incremental_qaly, na.rm = TRUE),
      mean(df$icer, na.rm = TRUE) + 1.96 * sd(df$icer, na.rm = TRUE),
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
}))

manuscript_results_summary <- bind_rows(
  effectiveness_12mo_comparison %>%
    transmute(
      section = "effectiveness",
      item = paste0(model, "_12mo_or"),
      estimate = odds_ratio,
      lower_95 = ci_low,
      upper_95 = ci_high
    ),
  summary_rows %>%
    transmute(
      section = "cost_effectiveness",
      item = metric,
      estimate = estimate,
      lower_95 = lower_95,
      upper_95 = upper_95
    )
)

write.csv(cea_df, "outputs/cea_patient_level.csv", row.names = FALSE)
write.csv(cea_long, "outputs/cea_longitudinal.csv", row.names = FALSE)
write.csv(model_summaries, "outputs/cea_model_summaries.csv", row.names = FALSE)
write.csv(cea_model_comparison, "outputs/cea_model_comparison.csv", row.names = FALSE)
write.csv(bootstrap_results, "outputs/cea_bootstrap_results.csv", row.names = FALSE)
write.csv(acceptability_curve, "outputs/cea_acceptability_curve.csv", row.names = FALSE)
write.csv(summary_rows, "outputs/cea_summary.csv", row.names = FALSE)
write.csv(effectiveness_comparison, "outputs/effectiveness_model_comparison.csv", row.names = FALSE)
write.csv(effectiveness_12mo_comparison, "outputs/effectiveness_12mo_comparison.csv", row.names = FALSE)
write.csv(manuscript_results_summary, "outputs/manuscript_results_summary.csv", row.names = FALSE)

saveRDS(
  list(
    patient_level = cea_df,
    longitudinal = cea_long,
    mixed_effects = mixed_results,
    gee_effects = gee_results,
    effectiveness_comparison = effectiveness_comparison,
    effectiveness_12mo_comparison = effectiveness_12mo_comparison,
    cost_model = cost_model,
    qaly_model = qaly_model,
    bootstrap_results = bootstrap_results,
    acceptability_curve = acceptability_curve,
    model_summaries = model_summaries,
    cea_model_comparison = cea_model_comparison,
    summary = summary_rows,
    manuscript_results_summary = manuscript_results_summary
  ),
  file = "outputs/cea_models.rds"
)

cat("05_cost_effectiveness: saved legacy-faithful complete-case CEA outputs.\n")
pipeline_phase_end(
  "05_cost_effectiveness",
  pipeline_started,
  "saved legacy-faithful CEA model and bootstrap outputs"
)
