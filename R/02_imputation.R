# R/02_imputation.R
# Multiple imputation pipeline.
#
# Main path:
#   - builds the wide ITT frame from the cleaned trial data
#   - runs the pipeline-standard full MICE branch only
#   - writes the time-aware predictor audit for the main branch
#
# Sensitivity imputation variants are generated separately in
# R/08_sensitivity_analyses.R so the primary stage stays focused.

source("R/02_imputation_helpers.R")

library(dplyr)
library(tidyr)
library(mice)
library(labelled)
library(haven)

pipeline_started <- pipeline_phase_start(
  "02_imputation",
  "building the main wide imputation frame"
)

out_dir <- DATA_PROCESSED_DIR
ensure_artifact_dirs()

cleaning_artifact <- read_canonical_artifact("cleaning")
trial_df <- cleaning_artifact$all_cases
df_impute <- build_imputation_wide_frame(trial_df)

pipeline_phase_info("02_imputation", "building the wide frame used by the main MICE branch")
full_mids <- run_full_mice_imputation(df_impute, out_dir = out_dir)

miss_report <- df_impute %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

imputation_artifact <- list(
  stage = "02_imputation",
  df_impute = df_impute,
  full_mids = full_mids$mids,
  full_predictor_matrix = full_mids$predictor_matrix,
  full_methods = full_mids$methods,
  full_predictor_audit = full_mids$predictor_audit,
  full_diagnostics = full_mids$diagnostics,
  first_completion = full_mids$first_completion,
  missingness_report = miss_report,
  contracts = "imputation_wide"
)
write_canonical_artifact("imputation", imputation_artifact)

message("02_imputation: saved canonical imputation artifact.")
pipeline_phase_end(
  "02_imputation",
  pipeline_started,
  "saved canonical imputation artifact"
)
