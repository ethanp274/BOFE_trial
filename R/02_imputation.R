# R/02_imputation.R
# Multiple imputation pipeline.
#
# Data source handling:
#   - If BOFE_DATA_SOURCE="raw" (default): reads canonical cleaning_artifact.rds from raw SPSS processing
#   - If BOFE_DATA_SOURCE="public": loads pre-cleaned public CSV directly for reproducibility
#
# Main workflow:
#   - builds the wide ITT frame from the cleaned trial data
#   - derives explicit primary effectiveness and CEA MICE frames from that source
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

# Determine data source and load accordingly
# If public data is selected, load it directly (avoids re-processing already-clean data)
# If raw data is selected, read the cleaning artifact produced by R/01_cleaning.R
active_data_source <- get_active_data_source()
pipeline_phase_info("02_imputation", sprintf("data source: %s", active_data_source))

if (active_data_source == "public") {
  pipeline_phase_info("02_imputation", "loading pre-cleaned public anonymized dataset")
  
  if (!public_dataset_exists()) {
    stop("BOFE_DATA_SOURCE='public' requested but public dataset not found at: ",
         method_config("data_source", "public_dataset_path"), "\n",
         "Run R/01b_publication_long_dataset.R first, or set BOFE_DATA_SOURCE='raw'.")
  }
  
  # Load and reshape the public CSV to analysis format
  public_result <- load_public_dataset_wide()
  trial_df <- public_result$all_cases
  
  # Remap factor levels to match internal analysis expectations
  trial_df$D1.4 <- factor(
    ifelse(trial_df$D1.4 == "intervention", 
           "ig (intervention group)", 
           "cg (control group)"),
    levels = c("cg (control group)", "ig (intervention group)")
  )
  
  pipeline_phase_info("02_imputation", 
    sprintf("loaded public data: N=%d patients", nrow(trial_df)))
  
} else {
  # Raw data path: read the cleaning artifact
  cleaning_artifact <- read_canonical_artifact("cleaning")
  trial_df <- cleaning_artifact$all_cases
  pipeline_phase_info("02_imputation", 
    sprintf("loaded raw-data cleaning artifact: N=%d patients", nrow(trial_df)))
}
df_impute <- build_imputation_wide_frame(trial_df)
selection_profile <- build_imputation_selection_profile(df_impute)
secondary_effectiveness_enabled <- isTRUE(method_config("effectiveness", "secondary_outcomes", "enabled"))

pipeline_phase_info("02_imputation", "building the effectiveness-specific MICE branch")
effectiveness_mids <- run_effectiveness_mice_imputation(df_impute, out_dir = out_dir)

secondary_effectiveness_mids <- NULL
if (isTRUE(secondary_effectiveness_enabled)) {
  pipeline_phase_info("02_imputation", "building the secondary-effectiveness MICE branch")
  secondary_effectiveness_mids <- run_secondary_effectiveness_mice_imputation(df_impute, out_dir = out_dir)
} else {
  pipeline_phase_info("02_imputation", "secondary adherence MICE branch disabled by methods config")
}

pipeline_phase_info("02_imputation", "building the CEA-specific MICE branch")
cea_mids <- run_cea_mice_imputation(df_impute, out_dir = out_dir)

miss_report <- df_impute %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

quickpred_comparison <- bind_rows(Filter(Negate(is.null), list(
  effectiveness_mids$quickpred_comparison,
  if (!is.null(secondary_effectiveness_mids)) secondary_effectiveness_mids$quickpred_comparison else NULL,
  cea_mids$quickpred_comparison
)))
quickpred_summary <- bind_rows(Filter(Negate(is.null), list(
  effectiveness_mids$quickpred_summary,
  if (!is.null(secondary_effectiveness_mids)) secondary_effectiveness_mids$quickpred_summary else NULL,
  cea_mids$quickpred_summary
)))

write.csv(selection_profile, audit_path("imputation_variable_selection.csv"), row.names = FALSE)
write.csv(effectiveness_mids$predictor_audit, audit_path("imputation_predictor_audit_effectiveness.csv"), row.names = FALSE)
if (!is.null(secondary_effectiveness_mids)) {
  write.csv(secondary_effectiveness_mids$predictor_audit, audit_path("imputation_predictor_audit_secondary_effectiveness.csv"), row.names = FALSE)
}
write.csv(cea_mids$predictor_audit, audit_path("imputation_predictor_audit_cea.csv"), row.names = FALSE)
write.csv(quickpred_summary, audit_path("imputation_quickpred_comparison_summary.csv"), row.names = FALSE)
write.csv(quickpred_comparison, audit_path("imputation_quickpred_pair_comparison.csv"), row.names = FALSE)

imputation_artifact <- list(
  stage = "02_imputation",
  secondary_effectiveness_enabled = secondary_effectiveness_enabled,
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

if (!is.null(secondary_effectiveness_mids)) {
  imputation_artifact <- c(
    imputation_artifact,
    list(
      secondary_effectiveness_df_impute = secondary_effectiveness_mids$df,
      secondary_effectiveness_mids = secondary_effectiveness_mids$mids,
      secondary_effectiveness_predictor_matrix = secondary_effectiveness_mids$predictor_matrix,
      secondary_effectiveness_quickpred_matrix = secondary_effectiveness_mids$quickpred_matrix,
      secondary_effectiveness_methods = secondary_effectiveness_mids$methods,
      secondary_effectiveness_predictor_audit = secondary_effectiveness_mids$predictor_audit,
      secondary_effectiveness_diagnostics = secondary_effectiveness_mids$diagnostics,
      secondary_effectiveness_first_completion = secondary_effectiveness_mids$first_completion
    )
  )
  imputation_artifact$contracts <- c(imputation_artifact$contracts, "secondary_effectiveness_imputation_wide")
}
write_canonical_artifact("imputation", imputation_artifact)

message("02_imputation: saved canonical imputation artifact.")
pipeline_phase_end(
  "02_imputation",
  pipeline_started,
  "saved canonical imputation artifact"
)
