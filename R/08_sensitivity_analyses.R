# R/08_sensitivity_analyses.R
# Purpose: Run all secondary analyses outside the main manuscript pipeline.
#
# Sensitivity families:
#   - complete-case, naive, basic MICE, and full MICE effectiveness variants
#   - GLM 12-month outcome comparison
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

basic_source_cols <- if (all(c("effectiveness_df_impute", "cea_df_impute") %in% names(main_imputation_artifact))) {
  unique(c(names(main_imputation_artifact$effectiveness_df_impute), names(main_imputation_artifact$cea_df_impute)))
} else {
  names(df_impute)
}
df_impute_basic <- df_impute[, intersect(basic_source_cols, names(df_impute)), drop = FALSE]
basic_mids <- run_basic_mice_imputation(df_impute_basic)
simple_branch <- run_simple_within_arm_imputation(df_impute_basic)
simple_imputed <- simple_branch$data
imputation_variant_summary <- write_imputation_variant_summary(df_impute_basic, simple_imputed)

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
  basic = basic_mids$mids,
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

economic_data <- cleaning_artifact$economic_data
complete_cases <- cleaning_artifact$complete_cases
cea_cost_family <- method_config("economics", "main_cost_family")
cea_bootstrap_iterations <- getOption("bofe.sensitivity_bootstrap_iterations", NULL)
if (is.null(cea_bootstrap_iterations)) {
  env_cea_bootstrap_iterations <- Sys.getenv(method_config("environment_overrides", "sensitivity_bootstrap_iterations"), "")
  cea_bootstrap_iterations <- if (nzchar(env_cea_bootstrap_iterations)) {
    suppressWarnings(as.integer(env_cea_bootstrap_iterations))
  } else {
    BOOTSTRAP_ITERATIONS
  }
}
if (!is.finite(cea_bootstrap_iterations) || cea_bootstrap_iterations < 1L) {
  cea_bootstrap_iterations <- BOOTSTRAP_ITERATIONS
}

# Reuse the same helper logic for each CEA sensitivity branch.
pipeline_phase_info("08_sensitivity_analyses", "running CEA sensitivity variants")

cea_complete_case <- run_wide_cea_branch(
  wide_df = complete_cases,
  branch_label = "complete_case",
  model_family = "complete_case",
  economic_data = economic_data,
  cost_family = cea_cost_family
)

cea_simple <- run_wide_cea_branch(
  wide_df = simple_imputed,
  branch_label = "simple_within_arm",
  model_family = "simple_within_arm",
  economic_data = economic_data,
  cost_family = cea_cost_family
)

cea_full <- read_canonical_artifact("cea")
expected_cost_model <- cost_model_spec(cea_cost_family)$model
if (!any(cea_full$cea_model_comparison$model == expected_cost_model, na.rm = TRUE)) {
  stop("Current main CEA artifact is not using the configured cost model (", expected_cost_model, "). Re-run R/05_cost_effectiveness.R first.")
}
cea_basic <- run_nested_mi_cea_branch(
  mids_obj = basic_mids$mids,
  branch_label = "basic_mice",
  economic_data = economic_data,
  bootstrap_iterations = as.integer(cea_bootstrap_iterations),
  cost_family = cea_cost_family
)

cea_sensitivity_summary <- bind_rows(
  cea_complete_case$summary %>% mutate(scenario = "complete_case"),
  cea_simple$summary %>% mutate(scenario = "simple_within_arm"),
  cea_full$summary %>% mutate(scenario = "full_mice"),
  cea_basic$summary %>% mutate(scenario = "basic_mice")
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
  uk_cea <- run_nested_mi_cea_branch(
    mids_obj = cea_full_mids,
    branch_label = tariff_sensitivity_label,
    tariff = tariff_sensitivity,
    economic_data = economic_data,
    bootstrap_iterations = as.integer(cea_bootstrap_iterations),
    cost_family = cea_cost_family
  )
  uk_cea$summary %>%
    mutate(
      scenario = tariff_sensitivity_label,
      note = paste0(tariff_sensitivity, " tariff coefficients applied successfully.")
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
    basic_mids = basic_mids$mids,
    basic_predictor_matrix = basic_mids$predictor_matrix,
    basic_methods = basic_mids$methods,
    basic_predictor_audit = basic_mids$predictor_audit,
    basic_diagnostics = basic_mids$diagnostics,
    simple_wide = simple_imputed,
    variant_summary = imputation_variant_summary
  ),
  effectiveness_sensitivity = effectiveness_sensitivity,
  cea_sensitivity_summary = cea_sensitivity_summary,
  cea_cost_sensitivity = cea_cost_sensitivity,
  uk_tariff_summary = uk_tariff_summary
)
write_canonical_artifact("sensitivity", sensitivity_artifact)
write_result_csv(effectiveness_sensitivity, "effectiveness_sensitivity_summary.csv")
write_result_csv(cea_sensitivity_summary, "cea_sensitivity_summary.csv")
write_result_csv(cea_cost_sensitivity, "cea_cost_sensitivity_summary.csv")
write_result_csv(uk_tariff_summary, "cea_tariff_sensitivity_summary.csv")

pipeline_phase_info("08_sensitivity_analyses", "sensitivity analyses complete")
pipeline_phase_end(
  "08_sensitivity_analyses",
  pipeline_started,
  "saved canonical sensitivity artifact and sensitivity CSV exports"
)
