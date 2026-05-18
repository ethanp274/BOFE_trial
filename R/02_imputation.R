# R/02_imputation.R
# Create imputation dataset and run MICE (10 imputations, predictive mean matching)

# Load helpers
source("R/utils.R")

library(tidyverse)
library(mice)
library(naniar)

# Paths
proc_path <- "data_processed/all_cases.rds"
out_dir <- "data_processed"
clean_out_dir <- "clean_data"
dir.create(out_dir, showWarnings = FALSE)
dir.create(clean_out_dir, showWarnings = FALSE)

# Read processed data
if(!file.exists(proc_path)) stop(proc_path, " not found. Run R/01_cleaning.R first.")
df <- readRDS(proc_path)

# Identify ID and candidate imputation columns
id_vars <- c('D1.1','D1.2','D1.3','D1.4')
# time-suffixed columns or key scores
time_pattern <- "(_0$|_3$|_6$|_9$|_12$)"
score_pattern <- "EQindex|ACT.SCORE|CCQ|controlled|FVC|FEV1|gp_|nurse_|therapist_|outpatient_|inpatient_"
cand_cols <- names(df)[grepl(time_pattern, names(df)) | grepl(score_pattern, names(df), ignore.case = TRUE)]
# Fallback: numeric columns
if(length(cand_cols) == 0) cand_cols <- names(df)[sapply(df, is.numeric)]

imp_vars <- unique(c(id_vars, cand_cols))
imp_vars <- imp_vars[imp_vars %in% names(df)]
imp_data <- df[, imp_vars]

# Save pre-imputation dataset for inspection
preimp_path <- file.path(clean_out_dir, "imputation_data_from_pipeline.csv")
# Remove labelled classes or convert to safe types for CSV export
impute <- imp_data
impute[] <- lapply(impute, function(x){
  # Convert haven_labelled or labelled to character via factor labels
  if(inherits(x, 'haven_labelled') || inherits(x, 'labelled')){
    return(as.character(haven::as_factor(x)))
  }
  # If list-columns exist, coerce to character safely
  if(is.list(x)){
    return(vapply(x, function(y) if(is.null(y)) NA_character_ else as.character(y), FUN.VALUE = character(1)))
  }
  return(x)
})
write.csv(impute, preimp_path, row.names = FALSE)

# Prepare data for mice: remove identifier columns from variables to be imputed
imp_for_mice <- imp_data %>% select(-one_of(id_vars))

# Convert haven_labelled/labelled to base types suitable for mice
imp_for_mice[] <- lapply(imp_for_mice, function(x){
  if(inherits(x, 'haven_labelled') || inherits(x, 'labelled')){
    # Try numeric conversion; if fails, convert to labelled factor strings
    num_try <- suppressWarnings(as.numeric(x))
    if(sum(!is.na(num_try)) > 0 && !all(is.na(num_try) & !is.na(x))){
      return(num_try)
    } else {
      return(as.character(haven::as_factor(x)))
    }
  }
  # Ensure list-columns are flattened
  if(is.list(x)){
    return(vapply(x, function(y) if(is.null(y)) NA_character_ else as.character(y), FUN.VALUE = character(1)))
  }
  return(x)
})

# Set methods: pmm for numeric, '' for non-imputed character/factor ids (none present now)
methods <- make.method(imp_for_mice)
# Use predictive mean matching for numeric and default for others
for(nm in names(methods)){
  if(is.numeric(imp_for_mice[[nm]])) methods[nm] <- "pmm" else methods[nm] <- ""
}

# Predictor matrix: use default but exclude complete ID variables if present
pred <- make.predictorMatrix(imp_for_mice)
# Do not use row identifiers as predictors (none here), safe-guard included

# Run mice
set.seed(2026)
maxit <- 10
m <- 10
cat("Running mice with m=", m, ", maxit=", maxit, "\n")
imputation <- mice(imp_for_mice, m = m, method = methods, predictorMatrix = pred, maxit = maxit, printFlag = TRUE)

# Save mids object and one completed dataset
saveRDS(imputation, file = file.path(out_dir, "mids_imputation.rds"))
completed1 <- complete(imputation, 1)
write.csv(completed1, file.path(out_dir, "imputed_dataset_1.csv"), row.names = FALSE)

# Save basic diagnostics
diag_path <- file.path(out_dir, "imputation_diagnostics.txt")
cat(capture.output(summary(imputation), digits = 4), file = diag_path, sep = "\n")

# Missingness report
miss_report <- imp_data %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% pivot_longer(everything(), names_to = "variable", values_to = "n_missing")
write.csv(miss_report, file.path(out_dir, "imputation_missingness_summary.csv"), row.names = FALSE)

message('02_imputation: created mids_imputation.rds, imputed_dataset_1.csv, and diagnostics')
