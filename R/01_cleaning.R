# R/01_cleaning.R
# Consolidated cleaning and ETL for BOFE project
# Purpose: read raw SPSS/CSV files, perform canonical cleaning steps, and produce analysis-ready datasets.
# NOTE: preserve original behavior. Detailed structural zero->NA mappings are in BOFE script_copy.R and will be migrated.

# Source shared helpers
source("R/utils.R")

library(tidyverse)
library(haven)
library(labelled)

raw_dir <- file.path("raw_data")

# Read questionnaires
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

# Apply explicit na_if rules from legacy script (new_data_cleaning_pipe.R)
# This preserves the original structural-zero mappings used by the legacy pipeline.
legacy_path <- 'new_data_cleaning_pipe.R'
if(file.exists(legacy_path)){
  legacy_lines <- readLines(legacy_path)
  apply_na_if_from_legacy <- function(df, tag){
    # select lines for this timepoint that contain na_if
    lines <- legacy_lines[grepl(paste0('^\\s*', tag, '\\$'), legacy_lines)]
    lines <- lines[grepl('<- na_if\\(', lines)]
    if(length(lines)==0) return(df)
    # extract variable names like T3$D2.1 <- na_if(...)
    vars <- sub('^\\s*[^\\$]+\\$([A-Za-z0-9_.]+).*', '\\1', lines)
    vars <- unique(vars)
    for(v in vars){
      if(v %in% names(df)){
        df[[v]] <- na_if(df[[v]], 0)
      }
    }
    return(df)
  }
  T0 <- apply_na_if_from_legacy(T0, 'T0')
  T3 <- apply_na_if_from_legacy(T3, 'T3')
  T6 <- apply_na_if_from_legacy(T6, 'T6')
  T9 <- apply_na_if_from_legacy(T9, 'T9')
  T12 <- apply_na_if_from_legacy(T12, 'T12')
} else {
  # Fallback to generic pattern-based replacement
  zero_patterns <- c('^D3\\.10', '^D3\\.11', '^D5\\.', '^D2\\.')
  T0 <- replace_zeros_with_na_patterns(T0, zero_patterns)
  T3 <- replace_zeros_with_na_patterns(T3, zero_patterns)
  T6 <- replace_zeros_with_na_patterns(T6, zero_patterns)
  T9 <- replace_zeros_with_na_patterns(T9, zero_patterns)
  T12 <- replace_zeros_with_na_patterns(T12, zero_patterns)
}

# Rename variables to include time suffix and merge
T0 <- rename_vars(T0, 0)
T3 <- rename_vars(T3, 3)
T6 <- rename_vars(T6, 6)
T9 <- rename_vars(T9, 9)
T12 <- rename_vars(T12, 12)

# Merge datasets (preserve all rows for survey_data; complete cases separately)
survey_data <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2','D1.3','D1.4'), all = TRUE), list(T0, T3, T6, T9, T12))

survey_data_complete <- Reduce(function(x,y) merge(x, y, by = c('D1.1','D1.2','D1.3','D1.4')), list(T0, T3, T6, T9, T12))

# Data integrity corrections (from BOFE script_copy.R)
# 1. Remove PR2B: patient only in T12, not in baseline (T0) - not ITT eligible
cat("Before exclusions: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")
survey_data <- survey_data[survey_data$D1.2 != "PR2B", ]
survey_data_complete <- survey_data_complete[survey_data_complete$D1.2 != "PR2B", ]

# 2. Fix OH5A disease coding inconsistency: D1.3_6 should be 2 (COPD), not 1 (Asthma)
survey_data$D1.3_6[survey_data$D1.2 == "OH5A"] <- 2
survey_data_complete$D1.3_6[survey_data_complete$D1.2 == "OH5A"] <- 2

# 3. Handle OH5A duplicates - keep only first row per patient
survey_data <- survey_data[!duplicated(survey_data$D1.2), ]
survey_data_complete <- survey_data_complete[!duplicated(survey_data_complete$D1.2), ]

cat("After corrections: survey_data N =", nrow(survey_data), ", survey_data_complete N =", nrow(survey_data_complete), "\n")

complete_cases <- survey_data_complete
all_cases <- survey_data

# Strip labels for downstream processing
complete_cases_no_lab <- remove_labels(complete_cases)

# Save outputs for downstream modules
dir.create('data_processed', showWarnings = FALSE)
saveRDS(complete_cases, file = 'data_processed/complete_cases.rds')
saveRDS(all_cases, file = 'data_processed/all_cases.rds')

message('01_cleaning: created data_processed/complete_cases.rds and all_cases.rds')
