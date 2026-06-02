# R/08_sensitivity_analyses.R
# Purpose: Run all secondary analyses outside the main manuscript pipeline.
#
# Sensitivity families:
#   - complete-case, naive, basic MICE, and full MICE effectiveness variants
#   - GLM 12-month outcome comparison
#   - GLMM over all timepoints
#   - CEA intervention-cost sweep
#   - CEA UK EQ-5D tariff sensitivity

source("R/02_imputation_helpers.R")
source("R/04_effectiveness_helpers.R")
source("R/05_cost_effectiveness_helpers.R")

library(dplyr)

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "08_sensitivity_analyses",
  "running secondary effectiveness and CEA analyses"
)

# Refresh the sensitivity imputation artifacts from the current clean wide frame.
trial_df <- readRDS("data_processed/all_cases.rds")
df_impute <- build_imputation_wide_frame(trial_df)
basic_mids <- run_basic_mice_imputation(df_impute)
simple_imputed <- run_simple_within_arm_imputation(df_impute)
write_imputation_variant_summary(df_impute, simple_imputed)

# Keep the main effectiveness artefacts for the full variant, and compute the
# alternative variants directly in-process so the runner stays filesystem-light.
read_variant_effectiveness <- function(variant) {
  pipeline_phase_info("08_sensitivity_analyses", sprintf("running effectiveness variant '%s'", variant))

  mixed_df <- run_mixed_effectiveness_analysis(imputation_variant = variant, write_outputs = FALSE)$timepoint_effects
  gee_df <- run_gee_effectiveness_analysis(imputation_variant = variant, write_outputs = FALSE)$gee_timepoint_effects

  bind_rows(mixed_df, gee_df) %>%
    filter(time == 12) %>%
    mutate(
      variant = variant,
      analysis = "effectiveness_sensitivity"
    )
}

effectiveness_variants <- c(
  full = "full",
  basic = "basic",
  simple = "simple",
  complete_cases = "complete_cases"
)

pipeline_phase_info("08_sensitivity_analyses", "running effectiveness sensitivity variants")
effectiveness_sensitivity <- bind_rows(lapply(effectiveness_variants, read_variant_effectiveness))
write.csv(effectiveness_sensitivity, result_path("effectiveness_sensitivity_summary.csv"), row.names = FALSE)

# The main result tables are canonical; the sensitivity runner should not leave
# alternate model CSVs lying around if prior runs created them.
if (file.exists(result_path("model_summaries.csv"))) file.remove(result_path("model_summaries.csv"))
if (file.exists(result_path("model_timepoint_effects.csv"))) file.remove(result_path("model_timepoint_effects.csv"))

economic_data_path <- "data_processed/economic_data.rds"
if (!file.exists(economic_data_path)) {
  stop("Missing ", economic_data_path, ". Run R/01_cleaning.R first.")
}
economic_data <- readRDS(economic_data_path)
complete_cases <- readRDS("data_processed/complete_cases.rds")
cea_cost_family <- "gaussian_identity"
cea_bootstrap_iterations <- getOption("bofe.sensitivity_bootstrap_iterations", NULL)
if (is.null(cea_bootstrap_iterations)) {
  env_cea_bootstrap_iterations <- Sys.getenv("BOFE_SENSITIVITY_BOOTSTRAP_ITERATIONS", "")
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

cea_full <- readRDS(model_path("cea_models.rds"))
if (!any(cea_full$cea_model_comparison$model == "GLM_Gaussian_identity_cost", na.rm = TRUE)) {
  stop("Current main CEA artifact is not using the Gaussian-identity cost model. Re-run R/05_cost_effectiveness.R first.")
}
cea_basic <- run_nested_mi_cea_branch(
  mids_path = "data_processed/mids_imputation_basic.rds",
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

write.csv(cea_sensitivity_summary, result_path("cea_sensitivity_summary.csv"), row.names = FALSE)

# Sweep the intervention cost so the economic conclusion is easy to stress test.
cost_values <- seq(40, 200, by = 20)
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

write.csv(cea_cost_sensitivity, result_path("cea_cost_sensitivity_summary.csv"), row.names = FALSE)

# Recompute QALYs under the UK EQ-5D tariff on the same MI cohort.
uk_tariff_summary <- tryCatch({
  uk_cea <- run_nested_mi_cea_branch(
    mids_path = "data_processed/mids_imputation.rds",
    branch_label = "uk_eq5d_tariff",
    tariff = "uk",
    economic_data = economic_data,
    bootstrap_iterations = as.integer(cea_bootstrap_iterations),
    cost_family = cea_cost_family
  )
  uk_cea$summary %>%
    mutate(
      scenario = "uk_eq5d_tariff",
      note = "UK tariff coefficients applied successfully."
    )
}, error = function(e) {
  data.frame(
    scenario = "uk_eq5d_tariff",
    model_family = "uk_eq5d_tariff",
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

write.csv(uk_tariff_summary, result_path("cea_tariff_sensitivity_summary.csv"), row.names = FALSE)

# Remove stale raw effectiveness CSVs that are no longer part of the canonical
# output set. The combined sensitivity summaries above are the files we keep.
stale_effectiveness_files <- unique(c(
  list.files(RESULTS_DIR, pattern = "^model_summaries(_.*)?\\.csv$", full.names = TRUE),
  list.files(RESULTS_DIR, pattern = "^model_timepoint_effects(_.*)?\\.csv$", full.names = TRUE),
  list.files(RESULTS_DIR, pattern = "^model_gee_summaries_(basic|simple|complete_cases)\\.csv$", full.names = TRUE),
  list.files(RESULTS_DIR, pattern = "^model_gee_timepoint_effects_(basic|simple|complete_cases)\\.csv$", full.names = TRUE)
))
if (length(stale_effectiveness_files) > 0) {
  file.remove(stale_effectiveness_files)
}

pipeline_phase_info("08_sensitivity_analyses", "sensitivity analyses complete")
pipeline_phase_end(
  "08_sensitivity_analyses",
  pipeline_started,
  "saved secondary analysis tables"
)
