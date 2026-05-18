###########################################################################
# R/05_cost_effectiveness.R
# Purpose: Complete-case cost-effectiveness analysis.
# Inputs:
#   - data_processed/complete_cases.rds
#   - data_processed/economic_data.rds
# Outputs:
#   - outputs/cea_patient_level.csv
#   - outputs/cea_model_summaries.csv
#   - outputs/cea_bootstrap_results.csv
#   - outputs/cea_acceptability_curve.csv
#   - outputs/cea_summary.csv
#   - outputs/cea_model_comparison.csv
#   - outputs/effectiveness_model_comparison.csv
#   - outputs/effectiveness_12mo_comparison.csv
#   - outputs/manuscript_results_summary.csv
#   - outputs/manuscript_results_cea_summary.csv
###########################################################################

source("R/utils.R")

library(dplyr)
library(geepack)

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

complete_cases <- readRDS("data_processed/complete_cases.rds")
complete_cases <- add_analysis_derivations(complete_cases)

if (!all(COST_SUMMARY_COLUMNS %in% names(complete_cases)) && file.exists("data_processed/economic_data.rds")) {
  complete_cases <- attach_cost_summaries(complete_cases, readRDS("data_processed/economic_data.rds"))
}

cea_df <- prepare_cea_patient_level(complete_cases)
cea_long <- make_longitudinal_analysis_data(complete_cases)
cea_long <- cea_long %>%
  filter(patient %in% cea_df$patient)

cea_long$interval_cost <- rowSums(
  cea_long[, c("interv_cost", "outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost")],
  na.rm = TRUE
)
cea_long$interval_cost_gamma <- cea_long$interval_cost + 0.001
cea_long$qaly_interval_model <- ifelse(cea_long$qaly_interval > 0, cea_long$qaly_interval, 0.0001)
cea_long$patient <- factor(cea_long$patient)
cea_long$group <- factor(cea_long$group, levels = GROUP_LEVELS)
cea_long$age <- factor(cea_long$age)
cea_long$gender <- factor(cea_long$gender)
cea_long$time <- factor(cea_long$time, levels = TIMEPOINTS)

clean_model_data <- function(formula, data, id_col = NULL) {
  vars <- unique(all.vars(formula))
  if (!is.null(id_col)) vars <- unique(c(vars, id_col))
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

fit_stable_geeglm <- function(formula, data, id_col, family) {
  data <- clean_model_data(formula, data, id_col = id_col)
  if (is.null(data)) return(NULL)
  if (n_distinct(data[[id_col]]) < 2) return(NULL)
  suppressWarnings(
    geeglm(
      formula = formula,
      data = data,
      id = data[[id_col]],
      family = family,
      corstr = "exchangeable",
      std.err = "san.se"
    )
  )
}

mixed_model_path <- "outputs/models_mixed_imputed.rds"
gee_model_path <- "outputs/models_gee_imputed.rds"
if (!file.exists(mixed_model_path)) stop("Missing ", mixed_model_path, ". Run R/04_models.R first.")
if (!file.exists(gee_model_path)) stop("Missing ", gee_model_path, ". Run R/04b_gee.R first.")

mixed_results <- readRDS(mixed_model_path)
gee_results <- readRDS(gee_model_path)

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

gee_cost_model <- fit_stable_geeglm(
  total_cost_gamma ~ group + age + gender,
  cea_df,
  id_col = "patient",
  family = Gamma(link = "log")
)

gee_qaly_model <- fit_stable_geeglm(
  QALY_model ~ group + age + gender,
  cea_df,
  id_col = "patient",
  family = gaussian(link = "identity")
)

glm_cost_summary <- summarise_model_terms(cost_model, "GLM_Gamma_log_cost", exponentiate = TRUE) %>%
  mutate(model_family = "glm", outcome = "cost")
glm_qaly_summary <- summarise_model_terms(qaly_model, "GLM_Gaussian_identity_QALY", exponentiate = FALSE) %>%
  mutate(model_family = "glm", outcome = "qaly")
gee_cost_summary <- summarise_model_terms(gee_cost_model, "GEE_Gamma_log_cost", exponentiate = TRUE) %>%
  mutate(model_family = "gee", outcome = "cost")
gee_qaly_summary <- summarise_model_terms(gee_qaly_model, "GEE_Gaussian_identity_QALY", exponentiate = FALSE) %>%
  mutate(model_family = "gee", outcome = "qaly")

model_summaries <- bind_rows(
  glm_cost_summary,
  glm_qaly_summary,
  gee_cost_summary,
  gee_qaly_summary
)

model_summaries <- model_summaries %>%
  mutate(
    estimate_exp = ifelse(
      is.na(estimate_exp) & outcome == "cost",
      exp(estimate),
      estimate_exp
    )
  )

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

run_bootstrap <- function(patient_level, num_iter = BOOTSTRAP_ITERATIONS) {
  patient_level$patient <- as.character(patient_level$patient)
  patient_ids <- unique(as.character(patient_level$patient))
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
    set.seed(i * 37)
    sample_ids <- sample(patient_ids, length(patient_ids), replace = TRUE)

    sample_patient <- patient_level[match(sample_ids, patient_level$patient), , drop = FALSE]
    sample_patient$patient <- as.character(sample_patient$patient)
    sample_patient$bootstrap_patient <- paste0(sample_patient$patient, "_", seq_along(sample_ids))

    boot_glm_cost <- tryCatch(
      fit_stable_glm(total_cost_gamma ~ group + age + gender, sample_patient, Gamma(link = "log"), maxit = 200),
      error = function(e) NULL
    )
    boot_glm_qaly <- tryCatch(
      fit_stable_glm(QALY_model ~ group + age + gender, sample_patient, gaussian(link = "identity"), maxit = 100),
      error = function(e) NULL
    )
    boot_gee_cost <- tryCatch(
      fit_stable_geeglm(total_cost_gamma ~ group + age + gender, sample_patient, id_col = "bootstrap_patient", family = Gamma(link = "log")),
      error = function(e) NULL
    )
    boot_gee_qaly <- tryCatch(
      fit_stable_geeglm(QALY_model ~ group + age + gender, sample_patient, id_col = "bootstrap_patient", family = gaussian(link = "identity")),
      error = function(e) NULL
    )

    models <- list(
      glm = list(cost = boot_glm_cost, qaly = boot_glm_qaly),
      gee = list(cost = boot_gee_cost, qaly = boot_gee_qaly)
    )

    for (model_family in names(models)) {
      boot_cost <- models[[model_family]]$cost
      boot_qaly <- models[[model_family]]$qaly

      if (is.null(boot_cost) || is.null(boot_qaly)) next
      if (model_family == "glm" && !isTRUE(boot_cost$converged)) next

      group_term <- grep("^groupig", names(coef(boot_cost)), value = TRUE)
      qaly_group_term <- grep("^groupig", names(coef(boot_qaly)), value = TRUE)

      if (length(group_term) == 0 || length(qaly_group_term) == 0) next

      out <- rbind(
        out,
        data.frame(
          iteration = i,
          model_family = model_family,
          cost_intercept = unname(coef(boot_cost)[["(Intercept)"]]),
          cost_group_log_ratio = unname(coef(boot_cost)[group_term[1]]),
          qaly_intercept = unname(coef(boot_qaly)[["(Intercept)"]]),
          incremental_qaly = unname(coef(boot_qaly)[qaly_group_term[1]]),
          baseline_cost = exp(unname(coef(boot_cost)[["(Intercept)"]])),
          intervention_cost = exp(unname(coef(boot_cost)[["(Intercept)"]])) * exp(unname(coef(boot_cost)[group_term[1]])),
          incremental_cost = exp(unname(coef(boot_cost)[["(Intercept)"]])) * exp(unname(coef(boot_cost)[group_term[1]])) - exp(unname(coef(boot_cost)[["(Intercept)"]])),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out$icer <- out$incremental_cost / out$incremental_qaly
  out
}

bootstrap_results <- run_bootstrap(cea_df)

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
    gee_cost_model = gee_cost_model,
    gee_qaly_model = gee_qaly_model,
    bootstrap_results = bootstrap_results,
    acceptability_curve = acceptability_curve,
    model_summaries = model_summaries,
    cea_model_comparison = cea_model_comparison,
    summary = summary_rows,
    manuscript_results_summary = manuscript_results_summary
  ),
  file = "outputs/cea_models.rds"
)

cat("05_cost_effectiveness: saved complete-case CEA outputs.\n")
