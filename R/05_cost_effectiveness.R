###########################################################################
# R/05_cost_effectiveness.R
# Purpose: Main cost-effectiveness analysis using the complex MICE dataset.
#
# Method choices are declared in R/00_methods_config.R.
###########################################################################

source("R/05_cost_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "05_cost_effectiveness",
  "running the main wide-imputed CEA branch"
)

imputation_artifact <- read_canonical_artifact("imputation")

bootstrap_iterations <- getOption("bofe.bootstrap_iterations", NULL)
if (is.null(bootstrap_iterations)) {
  env_bootstrap_iterations <- Sys.getenv(method_config("environment_overrides", "bootstrap_iterations"), "")
  bootstrap_iterations <- if (nzchar(env_bootstrap_iterations)) {
    suppressWarnings(as.integer(env_bootstrap_iterations))
  } else {
    BOOTSTRAP_ITERATIONS
  }
}
if (!is.finite(bootstrap_iterations) || bootstrap_iterations < 1L) {
  bootstrap_iterations <- BOOTSTRAP_ITERATIONS
}

# Fit the main MI CEA branch with a bootstrap that is nested inside the imputations.
main_results <- run_nested_mi_cea_branch(
  mids_obj = imputation_artifact$full_mids,
  branch_label = "mi_main",
  intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
  tariff = method_config("economics", "main_eq5d_tariff"),
  bootstrap_iterations = as.integer(bootstrap_iterations),
  cost_family = method_config("economics", "main_cost_family")
)

if (is.null(main_results) || is.null(main_results$summary) || nrow(main_results$summary) == 0) {
  stop("05_cost_effectiveness: main CEA branch produced no usable results.")
}

pipeline_phase_info(
  "05_cost_effectiveness",
  sprintf(
    "main MI CEA cohort built: %d patients, %d bootstrap estimates",
    nrow(main_results$patient_level),
    nrow(main_results$bootstrap_results)
  )
)

write_canonical_artifact("cea", main_results)
write_result_csv(main_results$bootstrap_results, "cea_bootstrap_results.csv")
write_result_csv(main_results$acceptability_curve, "cea_acceptability_curve.csv")
write_result_csv(main_results$summary, "cea_summary.csv")
write_result_csv(main_results$model_summaries, "cea_model_summaries.csv")
write_result_csv(main_results$cea_model_comparison, "cea_model_comparison.csv")

cat("05_cost_effectiveness: saved canonical CEA artifact and CEA CSV exports.\n")
pipeline_phase_end(
  "05_cost_effectiveness",
  pipeline_started,
  "saved canonical CEA artifact and CEA CSV exports"
)
