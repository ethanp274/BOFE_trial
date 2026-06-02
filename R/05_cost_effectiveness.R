###########################################################################
# R/05_cost_effectiveness.R
# Purpose: Main cost-effectiveness analysis using the complex MICE dataset.
#
# Main analysis:
#   - wide imputed patient-level cohort from R/02_imputation.R
#   - pooled MI CEA results using Rubin-style uncertainty combination
#
# Sensitivity analyses are now kept in separate scripts so the main
# manuscript pipeline stays readable and focused.
###########################################################################

source("R/05_cost_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "05_cost_effectiveness",
  "running the main wide-imputed CEA branch"
)

mids_path <- "data_processed/mids_imputation.rds"
if (!file.exists(mids_path)) {
  stop("Missing ", mids_path, ". Run R/02_imputation.R first.")
}

bootstrap_iterations <- getOption("bofe.bootstrap_iterations", NULL)
if (is.null(bootstrap_iterations)) {
  env_bootstrap_iterations <- Sys.getenv("BOFE_BOOTSTRAP_ITERATIONS", "")
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
  mids_path = mids_path,
  branch_label = "mi_main",
  intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
  bootstrap_iterations = as.integer(bootstrap_iterations),
  cost_family = "gaussian_identity"
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

# Save only the manuscript-facing summaries and bootstrap outputs.
write.csv(main_results$model_summaries, result_path("cea_model_summaries.csv"), row.names = FALSE)
write.csv(main_results$cea_model_comparison, result_path("cea_model_comparison.csv"), row.names = FALSE)
write.csv(main_results$bootstrap_results, result_path("cea_bootstrap_results.csv"), row.names = FALSE)
write.csv(main_results$acceptability_curve, result_path("cea_acceptability_curve.csv"), row.names = FALSE)

saveRDS(
  main_results,
  file = model_path("cea_models.rds")
)

cat("05_cost_effectiveness: saved main wide-imputed CEA outputs.\n")
pipeline_phase_end(
  "05_cost_effectiveness",
  pipeline_started,
  "saved main wide-imputed CEA outputs"
)
