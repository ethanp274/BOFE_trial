# R/04_models.R
# Purpose: Fit the mixed-effects sensitivity model on imputed data.
# The model uses only the variables needed for the legacy primary analysis:
# patient ID, treatment group, age, sex, baseline control, and time-varying
# control status.
#
# Inputs:
#   - data_processed/mids_imputation*.rds
# Outputs:
#   - results/model_summaries*.csv
#   - results/model_timepoint_effects*.csv
#   - models/models_mixed_imputed*.rds

source("R/04_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "04_models",
  "fitting the pooled mixed-effects sensitivity model"
)

imputation_variant <- tolower(
  getOption("bofe.imputation_variant", Sys.getenv("BOFE_IMPUTATION_VARIANT", "full"))
)

mixed_results <- run_mixed_effectiveness_analysis(
  imputation_variant = imputation_variant,
  write_outputs = TRUE
)

cat("04_models: pooled mixed-effects models fit on imputed data and saved summaries.\n")
pipeline_phase_end(
  "04_models",
  pipeline_started,
  "saved mixed-effects model summaries"
)
