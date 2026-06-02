# R/04_models.R
# Purpose: Fit the configured mixed-effects sensitivity model on imputed data.
#
# Inputs:
#   - canonical imputation, cleaning, or sensitivity artifacts
# Output:
#   - models/effectiveness_mixed_artifact.rds

source("R/04_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "04_models",
  "fitting the pooled mixed-effects sensitivity model"
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

mixed_results <- run_mixed_effectiveness_analysis(
  imputation_variant = imputation_variant,
  write_outputs = TRUE
)

cat("04_models: pooled mixed-effects models fit on imputed data and saved canonical artifact.\n")
pipeline_phase_end(
  "04_models",
  pipeline_started,
  "saved canonical mixed-effects artifact"
)
