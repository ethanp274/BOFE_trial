###########################################################################
# R/06_outputs.R
# Purpose: Assemble manuscript-facing outputs from canonical stage artifacts.
# Inputs:
#   - data_processed/cleaning_artifact.rds
#   - models/effectiveness_gee_artifact.rds
#   - models/cea_artifact.rds
# Outputs:
#   - results/manuscript_outputs_artifact.rds
###########################################################################

source("R/utils.R")

library(dplyr)

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "06_outputs",
  "assembling manuscript output artifact"
)

cleaning_artifact <- read_canonical_artifact("cleaning")
all_cases <- cleaning_artifact$all_cases
complete_cases <- cleaning_artifact$complete_cases
economic_data <- cleaning_artifact$economic_data

all_cases <- add_analysis_derivations(all_cases)
complete_cases <- add_analysis_derivations(complete_cases)

if (!all(COST_SUMMARY_COLUMNS %in% names(all_cases))) {
  all_cases <- attach_cost_summaries(all_cases, economic_data)
  complete_cases <- attach_cost_summaries(complete_cases, economic_data)
}

all_cases <- add_completeness_flags(all_cases)
complete_cases <- add_completeness_flags(complete_cases)

# Rebuild the long panel only for the effectiveness export and validation checks.
complete_long <- wide_to_analysis_long(complete_cases, analysis = "effectiveness")
gee_results <- read_canonical_artifact("effectiveness_gee")
cea_results <- read_canonical_artifact("cea")
pipeline_phase_info("06_outputs", "loaded effectiveness and CEA model artefacts")

# Keep the main summary focused on the configured primary effectiveness result.
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
  filter(metric %in% c(
    "incremental_cost",
    "incremental_qaly",
    "ICER",
    paste0("probability_acceptable_at_", WTP_THRESHOLD_EUR_PER_QALY)
  )) %>%
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

manuscript_outputs_artifact <- list(
  stage = "06_outputs",
  manuscript_results_summary = manuscript_results_summary,
  complete_cases_long = complete_long
)
write_canonical_artifact("manuscript_outputs", manuscript_outputs_artifact)

cat("06_outputs: saved canonical manuscript outputs artifact.\n")
pipeline_phase_end(
  "06_outputs",
  pipeline_started,
  "saved canonical manuscript outputs artifact"
)
