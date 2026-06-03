# R/02_imputation.R
# Multiple imputation pipeline.
#
# Main path:
#   - builds the wide ITT frame from the cleaned trial data
#   - derives explicit effectiveness and CEA MICE frames from that source
#   - runs analysis-specific MICE branches
#   - writes predictor-selection, analytic-matrix, and quickpred comparison audits
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
selection_profile <- build_imputation_selection_profile(df_impute)

pipeline_phase_info("02_imputation", "building the effectiveness-specific MICE branch")
effectiveness_mids <- run_effectiveness_mice_imputation(df_impute, out_dir = out_dir)

pipeline_phase_info("02_imputation", "building the CEA-specific MICE branch")
cea_mids <- run_cea_mice_imputation(df_impute, out_dir = out_dir)

miss_report <- df_impute %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

quickpred_comparison <- bind_rows(
  effectiveness_mids$quickpred_comparison,
  cea_mids$quickpred_comparison
)
quickpred_summary <- bind_rows(
  effectiveness_mids$quickpred_summary,
  cea_mids$quickpred_summary
)

write.csv(selection_profile, audit_path("imputation_variable_selection.csv"), row.names = FALSE)
write.csv(effectiveness_mids$predictor_audit, audit_path("imputation_predictor_audit_effectiveness.csv"), row.names = FALSE)
write.csv(cea_mids$predictor_audit, audit_path("imputation_predictor_audit_cea.csv"), row.names = FALSE)
write.csv(quickpred_summary, audit_path("imputation_quickpred_comparison_summary.csv"), row.names = FALSE)
write.csv(quickpred_comparison, audit_path("imputation_quickpred_pair_comparison.csv"), row.names = FALSE)

imputation_artifact <- list(
  stage = "02_imputation",
  df_impute = df_impute,
  selection_profile = selection_profile,
  effectiveness_df_impute = effectiveness_mids$df,
  effectiveness_mids = effectiveness_mids$mids,
  effectiveness_predictor_matrix = effectiveness_mids$predictor_matrix,
  effectiveness_quickpred_matrix = effectiveness_mids$quickpred_matrix,
  effectiveness_methods = effectiveness_mids$methods,
  effectiveness_predictor_audit = effectiveness_mids$predictor_audit,
  effectiveness_diagnostics = effectiveness_mids$diagnostics,
  effectiveness_first_completion = effectiveness_mids$first_completion,
  cea_df_impute = cea_mids$df,
  cea_mids = cea_mids$mids,
  cea_predictor_matrix = cea_mids$predictor_matrix,
  cea_quickpred_matrix = cea_mids$quickpred_matrix,
  cea_methods = cea_mids$methods,
  cea_predictor_audit = cea_mids$predictor_audit,
  cea_diagnostics = cea_mids$diagnostics,
  cea_first_completion = cea_mids$first_completion,
  quickpred_summary = quickpred_summary,
  quickpred_comparison = quickpred_comparison,
  full_mids = effectiveness_mids$mids,
  full_predictor_matrix = effectiveness_mids$predictor_matrix,
  full_methods = effectiveness_mids$methods,
  full_predictor_audit = effectiveness_mids$predictor_audit,
  full_diagnostics = effectiveness_mids$diagnostics,
  first_completion = effectiveness_mids$first_completion,
  missingness_report = miss_report,
  contracts = c("imputation_wide", "effectiveness_imputation_wide", "cea_imputation_wide")
)
write_canonical_artifact("imputation", imputation_artifact)

message("02_imputation: saved canonical imputation artifact.")
pipeline_phase_end(
  "02_imputation",
  pipeline_started,
  "saved canonical imputation artifact"
)
