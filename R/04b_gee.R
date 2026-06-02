# R/04b_gee.R
# Purpose: Fit the protocol-style GEE effectiveness model on imputed data.
# This script is intentionally separate from R/04_models.R so the GEE branch
# can be tuned and benchmarked independently from the mixed-effects sensitivity
# analysis.
#
# Inputs:
#   - data_processed/mids_imputation*.rds
# Outputs:
#   - results/model_gee_summaries*.csv
#   - results/model_gee_timepoint_effects*.csv
#   - models/models_gee_imputed*.rds

source("R/04_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "04b_gee",
  "fitting the protocol-style GEE effectiveness model"
)

imputation_variant <- tolower(
  getOption("bofe.imputation_variant", Sys.getenv("BOFE_IMPUTATION_VARIANT", "full"))
)

gee_results <- run_gee_effectiveness_analysis(
  imputation_variant = imputation_variant,
  write_outputs = TRUE
)

cat("04b_gee: GEE models fit on imputed data and saved summaries.\n")
pipeline_phase_end(
  "04b_gee",
  pipeline_started,
  "saved GEE model summaries"
)
