# R/08_sensitivity_analyses.R
# Purpose: Run all secondary analyses outside the main manuscript pipeline.
#
# Sensitivity families:
#   - missing-data handling checks for adjusted 12-month GEE
#   - 12-month model-specification comparison under the main MICE branch
#   - GLMM over all timepoints
#   - CEA intervention-cost sweep
#   - CEA EQ-5D tariff sensitivity configured in R/00_methods_config.R

source("R/02_imputation_helpers.R")
source("R/04_effectiveness_helpers.R")
source("R/05_cost_effectiveness_helpers.R")

library(dplyr)

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "08_sensitivity_analyses",
  "running secondary effectiveness and CEA analyses"
)

cleaning_artifact <- read_canonical_artifact("cleaning")
main_imputation_artifact <- read_canonical_artifact("imputation")

# Refresh the sensitivity imputation objects from the current clean wide frame.
trial_df <- cleaning_artifact$all_cases
df_impute <- build_imputation_wide_frame(trial_df)

simple_source_cols <- if (all(c("effectiveness_df_impute", "cea_df_impute") %in% names(main_imputation_artifact))) {
  unique(c(names(main_imputation_artifact$effectiveness_df_impute), names(main_imputation_artifact$cea_df_impute)))
} else {
  names(df_impute)
}
df_impute_simple <- df_impute[, intersect(simple_source_cols, names(df_impute)), drop = FALSE]
simple_branch <- run_simple_within_arm_imputation(df_impute_simple)
simple_imputed <- simple_branch$data
imputation_variant_summary <- write_imputation_variant_summary(df_impute_simple, simple_imputed)

# Keep the main effectiveness artefacts for the full variant, and compute the
# alternative variants directly in-process so the runner stays filesystem-light.
effectiveness_full_mids <- if ("effectiveness_mids" %in% names(main_imputation_artifact)) {
  main_imputation_artifact$effectiveness_mids
} else {
  main_imputation_artifact$full_mids
}
cea_full_mids <- if ("cea_mids" %in% names(main_imputation_artifact)) {
  main_imputation_artifact$cea_mids
} else {
  main_imputation_artifact$full_mids
}

variant_imputations <- list(
  full = effectiveness_full_mids,
  simple = simple_imputed,
  complete_cases = cleaning_artifact$complete_cases
)

read_variant_effectiveness <- function(variant) {
  pipeline_phase_info("08_sensitivity_analyses", sprintf("running effectiveness variant '%s'", variant))

  mixed_df <- run_mixed_effectiveness_analysis(
    imputation_variant = variant,
    write_outputs = FALSE,
    imputation_override = variant_imputations[[variant]]
  )$timepoint_effects
  gee_df <- run_gee_effectiveness_analysis(
    imputation_variant = variant,
    write_outputs = FALSE,
    imputation_override = variant_imputations[[variant]]
  )$gee_timepoint_effects

  bind_rows(mixed_df, gee_df) %>%
    filter(time == 12) %>%
    mutate(
      variant = variant,
      analysis = "effectiveness_sensitivity"
    )
}

effectiveness_variants <- method_config("imputation", "sensitivity_variants")

pipeline_phase_info("08_sensitivity_analyses", "running effectiveness sensitivity variants")
effectiveness_sensitivity <- bind_rows(lapply(effectiveness_variants, read_variant_effectiveness))

matrix_sensitivity_path <- file.path("audit", "imputation_matrix_sensitivity_gee_12mo.csv")
matrix_sensitivity_gee <- if (file.exists(matrix_sensitivity_path)) {
  read.csv(matrix_sensitivity_path, stringsAsFactors = FALSE)
} else {
  data.frame()
}

missing_data_gee_12mo <- bind_rows(
  effectiveness_sensitivity %>%
    filter(model_family == "gee", adjustment == "adjusted", variant %in% c("full", "simple", "complete_cases")) %>%
    transmute(
      source = "imputation_variant",
      variant = dplyr::recode(
        variant,
        full = "current_analytic",
        simple = "simple_within_arm",
        complete_cases = "complete_case"
      ),
      model_family,
      adjustment,
      time,
      odds_ratio,
      ci_low,
      ci_high,
      p_value
    ),
  if (nrow(matrix_sensitivity_gee) > 0) {
    matrix_sensitivity_gee %>%
      transmute(
        source = "predictor_matrix",
        variant,
        model_family,
        adjustment,
        time,
        odds_ratio,
        ci_low,
        ci_high,
        p_value = NA_real_
      )
  } else {
    data.frame()
  }
)

effectiveness_sets_full <- prepare_effectiveness_long_sets(effectiveness_full_mids)
long_sets_full <- followup_effectiveness_sets(effectiveness_sets_full$long_sets)
n_imp_full <- effectiveness_sets_full$n_imputations

pipeline_phase_info("08_sensitivity_analyses", "running 12-month model-specification comparison under the main MICE branch")
expanded_gee_formula <- controlled_t ~ controlled_0 + age + gender + condition + BMI + smoking + ihd + group * time
expanded_gee_result <- fit_and_pool_gee_models(
  long_sets = long_sets_full,
  model_formula = expanded_gee_formula,
  adjustment_label = "adjusted_expanded",
  n_imputations = n_imp_full
)

glm_unadjusted_result <- fit_single_timepoint_logistic_models(
  long_sets = long_sets_full,
  model_formula = controlled_t ~ group,
  adjustment_label = "unadjusted",
  n_imputations = n_imp_full,
  model_family = "glm_12mo"
)

glm_adjusted_result <- fit_single_timepoint_logistic_models(
  long_sets = long_sets_full,
  model_formula = controlled_t ~ controlled_0 + age + gender + group,
  adjustment_label = "adjusted",
  n_imputations = n_imp_full,
  model_family = "glm_12mo"
)

# Pharmacy-clustering sensitivity: mixed-effects model adding a pharmacy-level random intercept
pipeline_phase_info("08_sensitivity_analyses", "running pharmacy-clustering mixed-effects sensitivity (patient + pharmacy random intercepts)")
pharmacy_cluster_result <- tryCatch({
  pharmacy_model_formula <- as.formula(paste(
    method_config("effectiveness", "adjusted_formula"),
    method_config("effectiveness", "mixed_effects_formula_suffix"),
    "+ (1 | pharmacy)"
  ))

  pharmacy_fit <- fit_mixed_effects_models(
    long_sets = long_sets_full,
    model_formula = pharmacy_model_formula,
    adjustment_label = "adjusted_pharmacy_re",
    n_imputations = n_imp_full
  )

  pharmacy_tp <- pharmacy_fit$timepoint_effects
  write_result_csv(pharmacy_tp %>% filter(time == 12), "effectiveness_pharmacy_cluster_12mo.csv")

  list(success = TRUE, result = pharmacy_fit)
}, error = function(e) {
  pipeline_phase_info("08_sensitivity_analyses", paste0("pharmacy clustering sensitivity failed: ", conditionMessage(e)))
  list(success = FALSE, error = conditionMessage(e))
})

model_specification_12mo <- bind_rows(
  effectiveness_sensitivity %>%
    filter(variant == "full", time == 12, model_family == "gee", adjustment %in% c("unadjusted", "adjusted")) %>%
    mutate(specification = ifelse(adjustment == "unadjusted", "Unadjusted GEE", "Adjusted GEE (main covariates)")) %>%
    select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations),
  glm_unadjusted_result$timepoint_effects %>%
    mutate(specification = "Unadjusted GLM (12-month only)") %>%
    select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations),
  expanded_gee_result$gee_timepoint_effects %>%
    filter(time == 12) %>%
    mutate(specification = "Adjusted GEE (expanded baseline covariates)") %>%
    select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations),
  glm_adjusted_result$timepoint_effects %>%
    mutate(specification = "Adjusted GLM (12-month only)") %>%
    select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations),
  effectiveness_sensitivity %>%
    filter(variant == "full", time == 12, model_family == "mixed_effects", adjustment == "adjusted") %>%
    mutate(specification = "Adjusted GLMM (patient random effect)") %>%
    select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations),
  if (is.list(pharmacy_cluster_result) && isTRUE(pharmacy_cluster_result$success)) {
    pharmacy_cluster_result$result$timepoint_effects %>%
      filter(time == 12, adjustment == "adjusted_pharmacy_re") %>%
      mutate(specification = "Adjusted GLMM (patient + pharmacy random effects)") %>%
      select(specification, model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, p_value, n_imputations)
  } else {
    data.frame()
  }
)


# Pharmacy-clustered GEE is deliberately not part of the clean sensitivity bundle.
# `geepack::geeglm()` supports one clustering id. Setting id = pharmacy would
# replace, rather than add to, the patient-level repeated-measures clustering.
# The nested patient/pharmacy sensitivity is therefore handled by the GLMM above.
gee_pharmacy_result <- list(
  success = FALSE,
  skipped = TRUE,
  reason = paste(
    "Skipped by design: geeglm supports one clustering id, so id = pharmacy",
    "would replace the patient repeated-measures cluster rather than add a",
    "pharmacy-level cluster. Use the pharmacy random-intercept GLMM sensitivity",
    "for nested patient/pharmacy clustering."
  )
)


economic_data <- cleaning_artifact$economic_data
complete_cases <- cleaning_artifact$complete_cases
cea_cost_family <- method_config("economics", "main_cost_family")
cea_bootstrap_iterations <- getOption("bofe.sensitivity_bootstrap_iterations", NULL)
if (is.null(cea_bootstrap_iterations)) {
  env_cea_bootstrap_iterations <- Sys.getenv(method_config("environment_overrides", "sensitivity_bootstrap_iterations"), "")
  cea_bootstrap_iterations <- if (nzchar(env_cea_bootstrap_iterations)) {
    suppressWarnings(as.integer(env_cea_bootstrap_iterations))
  } else {
    method_config("economics", "sensitivity_bootstrap_iterations")
  }
}
if (!is.finite(cea_bootstrap_iterations) || cea_bootstrap_iterations < 1L) {
  cea_bootstrap_iterations <- method_config("economics", "sensitivity_bootstrap_iterations")
}

# Reuse the same helper logic for each CEA sensitivity branch.
pipeline_phase_info("08_sensitivity_analyses", "running CEA sensitivity variants")

cea_complete_case <- run_wide_cea_branch(
  wide_df = complete_cases,
  branch_label = "complete_case",
  model_family = "complete_case",
  economic_data = economic_data,
  cost_family = cea_cost_family,
  num_iter = as.integer(cea_bootstrap_iterations)
)

cea_simple <- run_wide_cea_branch(
  wide_df = simple_imputed,
  branch_label = "simple_within_arm",
  model_family = "simple_within_arm",
  economic_data = economic_data,
  cost_family = cea_cost_family,
  num_iter = as.integer(cea_bootstrap_iterations)
)

cea_full <- read_canonical_artifact("cea")
expected_cost_model <- cost_model_spec(cea_cost_family)$model
if (!any(cea_full$cea_model_comparison$model == expected_cost_model, na.rm = TRUE)) {
  stop("Current main CEA artifact is not using the configured cost model (", expected_cost_model, "). Re-run R/05_cost_effectiveness.R first.")
}
cea_sensitivity_summary <- bind_rows(
  cea_complete_case$summary %>% mutate(scenario = "complete_case"),
  cea_simple$summary %>% mutate(scenario = "simple_within_arm"),
  cea_full$summary %>% mutate(scenario = "full_mice")
) %>%
  select(
    scenario,
    model_family,
    metric,
    estimate,
    pooled_ci_lower,
    pooled_ci_upper,
    bootstrap_ci_lower,
    bootstrap_ci_upper,
    within_variance,
    between_variance,
    pooled_variance,
    pooled_std_error,
    n_boot,
    uncertainty_method
  )

# Sweep the intervention cost so the economic conclusion is easy to stress test.
cost_values <- method_config("economics", "intervention_cost_sweep")
cea_cost_sensitivity <- bind_rows(lapply(cost_values, function(cost_per_consultation) {
  cea_cost <- adjust_nested_cea_for_intervention_cost(
    cea_results = cea_full,
    intervention_cost_per_consultation = cost_per_consultation,
    branch_label = paste0("full_mice_cost_", cost_per_consultation)
  )
  cea_cost$summary %>%
    mutate(
      scenario = "intervention_cost_sweep",
      intervention_cost_per_consultation = cost_per_consultation
    )
}))

# Recompute QALYs under the configured alternate EQ-5D tariff on the same MI cohort.
tariff_sensitivity <- method_config("economics", "tariff_sensitivity")
tariff_sensitivity_label <- paste0(tariff_sensitivity, "_eq5d_tariff")
uk_tariff_summary <- tryCatch({
  uk_cea <- run_tariff_sensitivity_with_fixed_main_cost(
    reference_cea_results = cea_full,
    mids_obj = cea_full_mids,
    branch_label = tariff_sensitivity_label,
    economic_data = economic_data,
    tariff = tariff_sensitivity,
    intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    bootstrap_iterations = as.integer(cea_bootstrap_iterations)
  )
  uk_cea$summary %>%
    mutate(
      scenario = tariff_sensitivity_label,
      note = uk_cea$note
    )
}, error = function(e) {
  data.frame(
    scenario = tariff_sensitivity_label,
    model_family = tariff_sensitivity_label,
    metric = c("incremental_cost", "incremental_qaly", "ICER", paste0("probability_acceptable_at_", WTP_THRESHOLD_EUR_PER_QALY)),
    estimate = NA_real_,
    pooled_ci_lower = NA_real_,
    pooled_ci_upper = NA_real_,
    bootstrap_ci_lower = NA_real_,
    bootstrap_ci_upper = NA_real_,
    within_variance = NA_real_,
    between_variance = NA_real_,
    pooled_variance = NA_real_,
    pooled_std_error = NA_real_,
    n_boot = NA_integer_,
    uncertainty_method = "uk_eq5d_tariff",
    note = paste0("UK tariff sensitivity failed: ", conditionMessage(e)),
    stringsAsFactors = FALSE
  )
})

sensitivity_artifact <- list(
  stage = "08_sensitivity_analyses",
  imputations = list(
    simple_wide = simple_imputed,
    variant_summary = imputation_variant_summary
  ),
  effectiveness_sensitivity = effectiveness_sensitivity,
  missing_data_gee_12mo = missing_data_gee_12mo,
  model_specification_12mo = model_specification_12mo,
  expanded_gee = list(
    pooled_summary = expanded_gee_result$gee_pooled_summary,
    timepoint_effects = expanded_gee_result$gee_timepoint_effects
  ),
  glm_12mo = list(
    unadjusted = glm_unadjusted_result,
    adjusted = glm_adjusted_result
  ),
  cea_sensitivity_summary = cea_sensitivity_summary,
  cea_cost_sensitivity = cea_cost_sensitivity,
  uk_tariff_summary = uk_tariff_summary
)
if (is.list(pharmacy_cluster_result) && isTRUE(pharmacy_cluster_result$success)) {
  sensitivity_artifact$pharmacy_clustering <- list(
    timepoint_effects = pharmacy_cluster_result$result$timepoint_effects,
    pooled_summary = pharmacy_cluster_result$result$pooled_summary,
    manuscript_style_12mo = subset(pharmacy_cluster_result$result$timepoint_effects, time == 12 & adjustment == "adjusted_pharmacy_re")
  )
} else {
  sensitivity_artifact$pharmacy_clustering <- list(error = pharmacy_cluster_result)
}
if (is.list(gee_pharmacy_result) && isTRUE(gee_pharmacy_result$success)) {
  sensitivity_artifact$gee_pharmacy_clustering <- list(
    timepoint_effects = gee_pharmacy_result$result$gee_timepoint_effects,
    pooled_summary = gee_pharmacy_result$result$gee_pooled_summary,
    manuscript_style_12mo = subset(gee_pharmacy_result$result$gee_timepoint_effects, time == 12 & adjustment == "adjusted")
  )
} else {
  sensitivity_artifact$gee_pharmacy_clustering <- gee_pharmacy_result
}
write_canonical_artifact("sensitivity", sensitivity_artifact)
write_result_csv(effectiveness_sensitivity, "effectiveness_sensitivity_summary.csv")
write_result_csv(missing_data_gee_12mo, "missing_data_handling_gee_12mo.csv")
write_result_csv(model_specification_12mo, "model_specification_12mo_comparison.csv")
write_result_csv(cea_sensitivity_summary, "cea_sensitivity_summary.csv")
write_result_csv(cea_cost_sensitivity, "cea_cost_sensitivity_summary.csv")
write_result_csv(uk_tariff_summary, "cea_tariff_sensitivity_summary.csv")

pipeline_phase_info("08_sensitivity_analyses", "sensitivity analyses complete")
pipeline_phase_end(
  "08_sensitivity_analyses",
  pipeline_started,
  "saved canonical sensitivity artifact and sensitivity CSV exports"
)
