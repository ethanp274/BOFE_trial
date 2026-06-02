# R/01_cleaning.R
# Consolidated cleaning and ETL for BOFE project.
# Purpose: read raw SPSS/CSV files, perform canonical cleaning steps, derive
# analysis variables, attach cost summaries, and produce analysis-ready datasets.

# Source shared helpers
source("R/utils.R")

library(tidyverse)
library(haven)
library(labelled)

pipeline_started <- pipeline_phase_start(
  "01_cleaning",
  "reading raw questionnaire and cost files"
)

raw_dir <- file.path("raw_data")
pipeline_phase_info("01_cleaning", "loading baseline and follow-up questionnaires")

# Read each timepoint separately so the legacy merge logic stays explicit.
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
# Legacy scripts only kept pharmacy/patient IDs unsuffixed; disease and group
# remained time-specific after merging.
rename_vars_legacy_merge <- function(data, suffix) {
  id_vars <- c("D1.1", "D1.2")
  vars_to_rename <- setdiff(names(data), id_vars)
  names(data)[names(data) %in% vars_to_rename] <- paste0(vars_to_rename, "_", suffix)
  data
}

T0 <- rename_vars_legacy_merge(T0, 0)
T3 <- rename_vars_legacy_merge(T3, 3)
T6 <- rename_vars_legacy_merge(T6, 6)
T9 <- rename_vars_legacy_merge(T9, 9)
T12 <- rename_vars_legacy_merge(T12, 12)

# Merge the five visits into one long-wide analysis frame.
survey_data <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2'), all = TRUE), list(T0, T3, T6, T9, T12))

survey_data_complete <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2')), list(T0, T3, T6, T9, T12))

# Apply the two manual corrections carried forward from the legacy scripts.
# 1. Remove PR2B: patient only in T12, not in baseline (T0) - not ITT eligible
cat("Before exclusions: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")
survey_data <- survey_data[survey_data$D1.2 != "PR2B", ]
survey_data_complete <- survey_data_complete[survey_data_complete$D1.2 != "PR2B", ]

# 2. Fix OH5A disease coding inconsistency: this patient is COPD.
survey_data$D1.3_6[survey_data$D1.2 == "OH5A"] <- 2
survey_data_complete$D1.3_6[survey_data_complete$D1.2 == "OH5A"] <- 2

cat("After corrections: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")

# Keep baseline aliases for the refactor, but preserve legacy columns too.
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

complete_cases <- add_completeness_flags(complete_cases)
all_cases <- add_completeness_flags(all_cases)

# Save the cleaned analysis sets for later stages.
dir.create('data_processed', showWarnings = FALSE)
saveRDS(complete_cases, file = 'data_processed/complete_cases.rds')
saveRDS(all_cases, file = 'data_processed/all_cases.rds')
saveRDS(economic_data, file = 'data_processed/economic_data.rds')

message('01_cleaning: created complete_cases.rds, all_cases.rds, and economic_data.rds')
pipeline_phase_end(
  "01_cleaning",
  pipeline_started,
  "saved analysis-ready datasets"
)
