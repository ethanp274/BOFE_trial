# R/01_cleaning.R
# Consolidated cleaning and ETL for BOFE project.
# Purpose: read raw SPSS questionnaire and cost files, perform canonical cleaning
# steps, derive analysis variables, attach cost summaries, and produce the
# analysis-ready wide dataset as data_processed/cleaning_artifact.rds.
#
# NOTE: This script handles raw data (SPSS files) only.
# For reproducibility using the public anonymized dataset, see R/02_imputation.R
# which supports loading the pre-cleaned public CSV directly.

# Source shared helpers
source("R/utils.R")

library(tidyverse)
library(haven)
library(labelled)

pipeline_started <- pipeline_phase_start(
  "01_cleaning",
  "reading raw SPSS questionnaire and cost files"
)

raw_dir <- file.path("raw_data")
pipeline_phase_info("01_cleaning", "loading baseline and follow-up questionnaires")

# Read each timepoint separately so the visit-wise merge logic stays explicit.
T0 <- read_sav(file.path(raw_dir, 'T0.sav'))
T3 <- read_sav(file.path(raw_dir, 'T3.sav'))
T6 <- read_sav(file.path(raw_dir, 'T6.sav'))
T9 <- read_sav(file.path(raw_dir, 'T9.sav'))
T12 <- read_sav(file.path(raw_dir, 'T12.sav'))

# Basic type fixes
T0$D1.4 <- as_factor(T0$D1.4)
T3$D1.4 <- as_factor(T3$D1.4)
T6$D1.4 <- as_factor(T6$D1.4)
T9$D1.4 <- as_factor(T9$D1.4)
T12$D1.4 <- as_factor(T12$D1.4)

# Apply centrally defined structural-zero rules.
T0 <- apply_structural_zero_rules(T0, "T0")
T3 <- apply_structural_zero_rules(T3, "T3")
T6 <- apply_structural_zero_rules(T6, "T6")
T9 <- apply_structural_zero_rules(T9, "T9")
T12 <- apply_structural_zero_rules(T12, "T12")

# Rename variables to include time suffix and merge.
# Only pharmacy/patient IDs remain unsuffixed; disease and group remain
# time-specific after merging.
rename_vars_for_timepoint_merge <- function(data, suffix) {
  id_vars <- c("D1.1", "D1.2")
  vars_to_rename <- setdiff(names(data), id_vars)
  names(data)[names(data) %in% vars_to_rename] <- paste0(vars_to_rename, "_", suffix)
  data
}

T0 <- rename_vars_for_timepoint_merge(T0, 0)
T3 <- rename_vars_for_timepoint_merge(T3, 3)
T6 <- rename_vars_for_timepoint_merge(T6, 6)
T9 <- rename_vars_for_timepoint_merge(T9, 9)
T12 <- rename_vars_for_timepoint_merge(T12, 12)

# Merge the five visits into one long-wide analysis frame.
survey_data <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2'), all = TRUE), list(T0, T3, T6, T9, T12))

survey_data_complete <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2')), list(T0, T3, T6, T9, T12))

# Apply configured manual data corrections.
cat("Before exclusions: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")
manual_exclusions <- method_config("cleaning", "manual_exclusions")
survey_data <- survey_data[!survey_data$D1.2 %in% manual_exclusions, ]
survey_data_complete <- survey_data_complete[!survey_data_complete$D1.2 %in% manual_exclusions, ]

manual_disease_corrections <- method_config("cleaning", "manual_disease_corrections")
for (patient_id in names(manual_disease_corrections)) {
  corrected_condition <- manual_disease_corrections[[patient_id]]
  survey_data$D1.3_6[survey_data$D1.2 == patient_id] <- corrected_condition
  survey_data_complete$D1.3_6[survey_data_complete$D1.2 == patient_id] <- corrected_condition
}

cat("After corrections: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")

# Keep baseline aliases alongside time-suffixed source columns.
survey_data$D1.3 <- survey_data$D1.3_0
survey_data$D1.4 <- survey_data$D1.4_0
survey_data_complete$D1.3 <- survey_data_complete$D1.3_0
survey_data_complete$D1.4 <- survey_data_complete$D1.4_0

# Derive the analysis variables used downstream by models and tables.
complete_cases <- add_analysis_derivations(survey_data_complete)
all_cases <- add_analysis_derivations(survey_data)

# Attach observed cost summaries so the economic helpers can work from one file.
economic_data <- build_economic_data(raw_dir)
complete_cases <- attach_cost_summaries(complete_cases, economic_data)
all_cases <- attach_cost_summaries(all_cases, economic_data)

cost_completeness_summary <- data.frame(
  cohort = c("all_cases", "complete_cases", "economic_data"),
  n_rows = c(nrow(all_cases), nrow(complete_cases), nrow(economic_data)),
  cost_complete = c(sum(all_cases$cost_complete), sum(complete_cases$cost_complete), sum(economic_data$cost_complete)),
  no_raw_cost_source = c(sum(!all_cases$cost_complete), sum(!complete_cases$cost_complete), sum(!economic_data$cost_complete)),
  medication_file_present = c(
    sum(all_cases$cost_medication_file_present),
    sum(complete_cases$cost_medication_file_present),
    sum(economic_data$cost_medication_file_present)
  ),
  source_present_missing_cost_summary_cells = c(
    sum(is.na(all_cases[all_cases$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(complete_cases[complete_cases$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(economic_data[economic_data$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE]))
  ),
  no_source_missing_cost_summary_cells = c(
    sum(is.na(all_cases[!all_cases$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(complete_cases[!complete_cases$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(economic_data[!economic_data$cost_complete, COST_SUMMARY_COLUMNS, drop = FALSE]))
  ),
  missing_cost_summary_cells = c(
    sum(is.na(all_cases[, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(complete_cases[, COST_SUMMARY_COLUMNS, drop = FALSE])),
    sum(is.na(economic_data[, COST_SUMMARY_COLUMNS, drop = FALSE]))
  ),
  stringsAsFactors = FALSE
)

complete_cases <- standardize_core_identifiers(complete_cases)
all_cases <- standardize_core_identifiers(all_cases)
complete_cases <- add_completeness_flags(complete_cases)
all_cases <- add_completeness_flags(all_cases)
assert_data_contract(complete_cases, "analysis_wide")
assert_data_contract(all_cases, "analysis_wide")

cleaning_artifact <- list(
  stage = "01_cleaning",
  all_cases = all_cases,
  complete_cases = complete_cases,
  economic_data = economic_data,
  cost_completeness_summary = cost_completeness_summary,
  data_source = "raw",
  contracts = c("analysis_wide", "economic_cost_summary")
)
write_canonical_artifact("cleaning", cleaning_artifact)

message("01_cleaning: saved canonical cleaning artifact.")
message(sprintf("01_cleaning: all_cases N=%d, complete_cases N=%d",
                nrow(all_cases), nrow(complete_cases)))
pipeline_phase_end(
  "01_cleaning",
  pipeline_started,
  "saved canonical cleaning artifact"
)


