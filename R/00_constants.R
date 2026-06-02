# R/00_constants.R
# Compatibility constants derived from R/00_methods_config.R.

if (!exists("BOFE_METHODS_CONFIG")) {
  source("R/00_methods_config.R")
}

TIMEPOINTS <- method_config("study", "timepoints")
FOLLOWUP_TIMEPOINTS <- method_config("study", "followup_timepoints")
GROUP_LEVELS <- method_config("study", "group_levels")
IMPUTATION_REPLICATES <- method_config("imputation", "replicates")
INTERVENTION_COST_PER_CONSULTATION <- method_config("economics", "intervention_cost_per_consultation")
WTP_THRESHOLD_EUR_PER_QALY <- method_config("economics", "wtp_threshold_eur_per_qaly")
BOOTSTRAP_ITERATIONS <- method_config("economics", "bootstrap_iterations")
DATA_PROCESSED_DIR <- "data_processed"
CLEAN_DATA_DIR <- "clean_data"
MODELS_DIR <- "models"
RESULTS_DIR <- "results"
AUDIT_DIR <- "audit"
LOGS_DIR <- "logs"
PIPELINE_PROGRESS_LOG <- file.path(LOGS_DIR, "pipeline_progress.log")

COST_MONTHS_FIRST_HALF <- method_config("economics", "cost_months_first_half")
COST_MONTHS_SECOND_HALF <- method_config("economics", "cost_months_second_half")
COST_SUMMARY_COLUMNS <- method_config("economics", "cost_summary_columns")

STRUCTURAL_ZERO_RULES <- method_config("cleaning", "structural_zero_rules")
