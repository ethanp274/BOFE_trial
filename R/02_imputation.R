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

proc_path <- "data_processed/all_cases.rds"
out_dir <- "data_processed"
clean_out_dir <- "clean_data"
results_dir <- "results"
dir.create(out_dir, showWarnings = FALSE)
dir.create(clean_out_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

if (!file.exists(proc_path)) stop(proc_path, " not found. Run R/01_cleaning.R first.")

trial_df <- readRDS(proc_path)
df_impute <- build_imputation_wide_frame(trial_df)

pipeline_phase_info("02_imputation", "building the wide frame used by the main MICE branch")
full_mids <- run_full_mice_imputation(df_impute, out_dir = out_dir)

if (file.exists(file.path(out_dir, "mids_imputation_full.rds"))) {
  file.copy(
    file.path(out_dir, "mids_imputation_full.rds"),
    file.path(out_dir, "mids_imputation.rds"),
    overwrite = TRUE
  )
}

miss_report <- df_impute %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")
write.csv(miss_report, file.path(out_dir, "imputation_missingness_summary.csv"), row.names = FALSE)

message("02_imputation: created the full MICE branch and diagnostics.")
pipeline_phase_end(
  "02_imputation",
  pipeline_started,
  "saved full imputation branch and diagnostics"
)
