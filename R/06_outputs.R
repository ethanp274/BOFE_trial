###########################################################################
# R/06_outputs.R
# Purpose: Export modular pipeline data to legacy-style CSVs and create
# lightweight validation reports.
# Inputs:
#   - data_processed/all_cases.rds
#   - data_processed/complete_cases.rds
# Outputs:
#   - clean_data/all_cases_from_pipeline.csv
#   - clean_data/complete_cases_from_pipeline.csv
#   - clean_data/complete_cases_long_from_pipeline.csv
#   - results/manuscript_results_summary.csv
###########################################################################

source("R/utils.R")

library(dplyr)

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "06_outputs",
  "writing legacy-style CSV exports and manuscript summaries"
)

# Load the cleaned wide analysis sets used for validation exports.
all_cases <- readRDS("data_processed/all_cases.rds")
complete_cases <- readRDS("data_processed/complete_cases.rds")
economic_data_path <- "data_processed/economic_data.rds"

all_cases <- add_analysis_derivations(all_cases)
complete_cases <- add_analysis_derivations(complete_cases)

if (!file.exists(economic_data_path)) {
  stop("Missing ", economic_data_path, ". Run R/01_cleaning.R first.")
}

economic_data <- readRDS(economic_data_path)

if (!all(COST_SUMMARY_COLUMNS %in% names(all_cases))) {
  all_cases <- attach_cost_summaries(all_cases, economic_data)
  complete_cases <- attach_cost_summaries(complete_cases, economic_data)
}

all_cases <- add_completeness_flags(all_cases)
complete_cases <- add_completeness_flags(complete_cases)

# Rebuild the long panel only for the effectiveness export and validation checks.
complete_long <- wide_to_analysis_long(complete_cases, analysis = "effectiveness")
gee_results_path <- model_path("models_gee_imputed.rds")
cea_results_path <- model_path("cea_models.rds")

if (!file.exists(gee_results_path)) stop("Missing ", gee_results_path, ". Run R/04b_gee.R first.")
if (!file.exists(cea_results_path)) stop("Missing ", cea_results_path, ". Run R/05_cost_effectiveness.R first.")

gee_results <- readRDS(gee_results_path)
cea_results <- readRDS(cea_results_path)
pipeline_phase_info("06_outputs", "loaded effectiveness and CEA model artefacts")

# Keep the main summary focused on the primary GEE effectiveness result.
effectiveness_results <- gee_results$gee_timepoint_effects %>%
  filter(time == 12) %>%
  select(model_family, adjustment, model, time, odds_ratio, ci_low, ci_high, any_of("p_value"), n_imputations)

# Standardise the CEA summary column names before export.
cea_summary <- cea_results$summary
if (!"model_family" %in% names(cea_summary)) {
  cea_summary$model_family <- NA_character_
}
if (!"pooled_ci_lower" %in% names(cea_summary) && "lower_95" %in% names(cea_summary)) {
  cea_summary$pooled_ci_lower <- cea_summary$lower_95
}
if (!"pooled_ci_upper" %in% names(cea_summary) && "upper_95" %in% names(cea_summary)) {
  cea_summary$pooled_ci_upper <- cea_summary$upper_95
}
if (!"bootstrap_ci_lower" %in% names(cea_summary) && "bootstrap_lower_95" %in% names(cea_summary)) {
  cea_summary$bootstrap_ci_lower <- cea_summary$bootstrap_lower_95
}
if (!"bootstrap_ci_upper" %in% names(cea_summary) && "bootstrap_upper_95" %in% names(cea_summary)) {
  cea_summary$bootstrap_ci_upper <- cea_summary$bootstrap_upper_95
}
cea_summary <- cea_summary %>%
  mutate(section = "cost_effectiveness") %>%
  filter(metric %in% c("incremental_cost", "incremental_qaly", "ICER", "probability_acceptable_at_29000")) %>%
  select(
    section,
    model_family,
    metric,
    estimate,
    pooled_ci_lower,
    pooled_ci_upper,
    bootstrap_ci_lower,
    bootstrap_ci_upper,
    any_of(c(
      "within_variance",
      "between_variance",
      "pooled_variance",
      "pooled_std_error",
      "n_boot",
      "uncertainty_method"
    ))
  )

# Combine the primary effectiveness and CEA rows into one manuscript table.
manuscript_results_summary <- bind_rows(
  effectiveness_results %>%
    transmute(
      section = "effectiveness",
      model_family = model_family,
      adjustment = adjustment,
      model = model,
      time = time,
      metric = paste0(model, "_12mo_or"),
      estimate = odds_ratio,
      pooled_ci_lower = ci_low,
      pooled_ci_upper = ci_high,
      bootstrap_ci_lower = NA_real_,
      bootstrap_ci_upper = NA_real_,
      p_value = p_value,
      n_imputations = n_imputations,
      uncertainty_method = NA_character_
    ),
  cea_summary
)

pipeline_phase_info("06_outputs", "assembling manuscript-ready comparison tables")

write.csv(all_cases, "clean_data/all_cases_from_pipeline.csv", row.names = FALSE)
write.csv(complete_cases, "clean_data/complete_cases_from_pipeline.csv", row.names = FALSE)
write.csv(complete_long, "clean_data/complete_cases_long_from_pipeline.csv", row.names = FALSE)
write.csv(manuscript_results_summary, result_path("manuscript_results_summary.csv"), row.names = FALSE)

cat("06_outputs: saved manuscript outputs.\n")
pipeline_phase_end(
  "06_outputs",
  pipeline_started,
  "saved manuscript outputs"
)
