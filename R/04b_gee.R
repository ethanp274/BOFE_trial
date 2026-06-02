# R/04b_gee.R
# Purpose: Fit the configured primary GEE effectiveness model on imputed data.
#
# Inputs:
#   - canonical imputation, cleaning, or sensitivity artifacts
# Output:
#   - models/effectiveness_gee_artifact.rds

source("R/04_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "04b_gee",
  "fitting the configured GEE effectiveness model"
)

imputation_variant <- tolower(
  getOption(
    "bofe.imputation_variant",
    Sys.getenv(
      method_config("environment_overrides", "imputation_variant"),
      method_config("effectiveness", "default_imputation_variant")
    )
  )
)

gee_results <- run_gee_effectiveness_analysis(
  imputation_variant = imputation_variant,
  write_outputs = TRUE
)

cat("04b_gee: GEE models fit on imputed data and saved canonical artifact.\n")
pipeline_phase_end(
  "04b_gee",
  pipeline_started,
  "saved canonical GEE effectiveness artifact"
)
