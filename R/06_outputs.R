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
#   - clean_data/complete_cases_CEA_from_pipeline.csv
#   - outputs/manuscript_results_summary.csv
#   - outputs/manuscript_results_effectiveness.csv
#   - outputs/manuscript_results_cea.csv
#   - outputs/manuscript_results_cea_summary.csv
#   - outputs/pipeline_validation_summary.csv
###########################################################################

source("R/utils.R")

library(dplyr)

if (!dir.exists("clean_data")) dir.create("clean_data", showWarnings = FALSE)
if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

all_cases <- readRDS("data_processed/all_cases.rds")
complete_cases <- readRDS("data_processed/complete_cases.rds")

all_cases <- add_analysis_derivations(all_cases)
complete_cases <- add_analysis_derivations(complete_cases)

if (!all(COST_SUMMARY_COLUMNS %in% names(all_cases)) && file.exists("data_processed/economic_data.rds")) {
  economic_data <- readRDS("data_processed/economic_data.rds")
  all_cases <- attach_cost_summaries(all_cases, economic_data)
  complete_cases <- attach_cost_summaries(complete_cases, economic_data)
}

all_cases <- add_completeness_flags(all_cases)
complete_cases <- add_completeness_flags(complete_cases)

complete_long <- make_longitudinal_analysis_data(complete_cases)
complete_cea <- prepare_cea_patient_level(complete_cases)

mixed_results_path <- "outputs/models_mixed_imputed.rds"
gee_results_path <- "outputs/models_gee_imputed.rds"
cea_results_path <- "outputs/cea_models.rds"

if (!file.exists(mixed_results_path)) stop("Missing ", mixed_results_path, ". Run R/04_models.R first.")
if (!file.exists(gee_results_path)) stop("Missing ", gee_results_path, ". Run R/04b_gee.R first.")
if (!file.exists(cea_results_path)) stop("Missing ", cea_results_path, ". Run R/05_cost_effectiveness.R first.")

mixed_results <- readRDS(mixed_results_path)
gee_results <- readRDS(gee_results_path)
cea_results <- readRDS(cea_results_path)

effectiveness_results <- bind_rows(
  mixed_results$timepoint_effects %>% mutate(model = "mixed_effects"),
  gee_results$gee_timepoint_effects %>% mutate(model = "gee")
) %>%
  select(model, time, odds_ratio, ci_low, ci_high, n_imputations)

effectiveness_12mo <- effectiveness_results %>%
  filter(time == 12)

cea_summary <- cea_results$summary
if (!"model_family" %in% names(cea_summary)) {
  cea_summary$model_family <- NA_character_
}
cea_summary <- cea_summary %>%
  mutate(section = "cost_effectiveness") %>%
  select(section, model_family, metric, estimate, lower_95, upper_95)

cea_model_comparison <- if (!is.null(cea_results$cea_model_comparison)) {
  cea_results$cea_model_comparison
} else {
  data.frame()
}

manuscript_results_summary <- bind_rows(
  effectiveness_12mo %>%
    transmute(
      section = "effectiveness",
      model_family = model,
      metric = paste0(model, "_12mo_or"),
      estimate = odds_ratio,
      lower_95 = ci_low,
      upper_95 = ci_high
  ),
  cea_summary
)

write.csv(all_cases, "clean_data/all_cases_from_pipeline.csv", row.names = FALSE)
write.csv(complete_cases, "clean_data/complete_cases_from_pipeline.csv", row.names = FALSE)
write.csv(complete_long, "clean_data/complete_cases_long_from_pipeline.csv", row.names = FALSE)
write.csv(complete_cea, "clean_data/complete_cases_CEA_from_pipeline.csv", row.names = FALSE)
write.csv(effectiveness_results, "outputs/manuscript_results_effectiveness.csv", row.names = FALSE)
write.csv(cea_model_comparison, "outputs/manuscript_results_cea.csv", row.names = FALSE)
write.csv(cea_summary, "outputs/manuscript_results_cea_summary.csv", row.names = FALSE)
write.csv(manuscript_results_summary, "outputs/manuscript_results_summary.csv", row.names = FALSE)

expected_counts <- data.frame(
  object = c("all_cases", "complete_cases", "complete_cases_long", "complete_cases_CEA"),
  expected_n = c(835, 756, 756 * length(TIMEPOINTS), NA_integer_)
)

observed_counts <- data.frame(
  object = c("all_cases", "complete_cases", "complete_cases_long", "complete_cases_CEA"),
  observed_n = c(nrow(all_cases), nrow(complete_cases), nrow(complete_long), nrow(complete_cea))
)

validation_summary <- merge(expected_counts, observed_counts, by = "object", all = TRUE)
validation_summary$matches_expected <- ifelse(
  is.na(validation_summary$expected_n),
  NA,
  validation_summary$expected_n == validation_summary$observed_n
)

validation_summary <- bind_rows(
  validation_summary,
  data.frame(
    object = c(
      "all_cases_unique_patients",
      "complete_cases_unique_patients",
      "missing_controlled_12_all_cases",
      "missing_EQindex_12_all_cases"
    ),
    expected_n = c(835, 756, NA_integer_, NA_integer_),
    observed_n = c(
      n_distinct(all_cases$D1.2),
      n_distinct(complete_cases$D1.2),
      sum(is.na(all_cases$controlled_12)),
      sum(is.na(all_cases$EQindex_12))
    ),
    matches_expected = c(
      n_distinct(all_cases$D1.2) == 835,
      n_distinct(complete_cases$D1.2) == 756,
      NA,
      NA
    )
  )
)

write.csv(validation_summary, "outputs/pipeline_validation_summary.csv", row.names = FALSE)

cat("06_outputs: saved legacy-style CSV exports and validation summary.\n")
