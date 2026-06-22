# R/public_data_helpers.R
# Helper functions to load and reshape the public anonymized dataset
# for replicable analysis without access to raw data files.
#
# The public dataset is in long format (one row per patient per timepoint).
# This module reshapes it to the wide format expected by the main analysis pipeline.

#' Load public anonymized dataset and reshape to wide analysis format
#'
#' @param public_csv_path Path to the public CSV file (default from methods config)
#' @return List with elements:
#'   - all_cases: wide-format data frame with all ITT patients
#'   - economic_data: wide-format economic summary data frame
#' @export
load_public_dataset_wide <- function(public_csv_path = NULL) {
  if (is.null(public_csv_path)) {
    public_csv_path <- method_config("data_source", "public_dataset_path")
  }
  
  if (!file.exists(public_csv_path)) {
    stop(sprintf("Public dataset not found at: %s\nExpected: %s", 
                 public_csv_path, normalizePath(public_csv_path)))
  }
  
  message("Loading public anonymized dataset from: ", public_csv_path)
  
  # Read the public CSV
  df_long <- read.csv(public_csv_path, stringsAsFactors = FALSE, na.strings = "NA")
  
  # Convert character columns to appropriate types
  df_long$anon_patient_id <- as.character(df_long$anon_patient_id)
  df_long$timepoint_month <- as.integer(df_long$timepoint_month)
  df_long$trial_arm <- factor(df_long$trial_arm, 
                              levels = c("control", "intervention"))
  df_long$condition <- factor(df_long$condition, 
                              levels = c("asthma", "COPD"))
  
  # Map public columns to internal analysis format
  # The public dataset uses readable names; map them to analysis variables
  df_long <- df_long %>%
    rename(
      D1.2 = anon_patient_id,
      timepoint = timepoint_month,
      D1.4 = trial_arm,
      D1.3 = condition,
      disease_controlled = disease_controlled,
      EQindex = eq5d_index
    )
  
  # Reshape from long to wide format (one row per patient)
  # Outcome variables get timepoint suffix
  df_wide <- df_long %>%
    pivot_wider(
      id_cols = c("D1.2", "D1.4", "D1.3"),
      names_from = "timepoint",
      values_from = c(
        "disease_controlled",
        "EQindex",
        "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activities", 
        "eq5d_pain_discomfort", "eq5d_anxiety_depression",
        "outpatient_cost", "laboratory_cost", "medication_cost", 
        "delivery_cost", "inpatient_cost",
        "cost_source_present", "medication_cost_file_present",
        "residence_location_category", "age_category", "gender_category",
        "bmi_category", "smoking_category", "ischaemic_heart_disease",
        "cost_period_months"
      ),
      names_sort = TRUE,
      values_fill = NA
    )
  
  # Standardize suffixes: controlled_0, controlled_3, etc.
  # (pivot_wider will have added _0, _3, etc., so this should already match)
  
  # Create baseline aliases (unsuffixed versions from T0 values)
  df_wide$D1.4_0 <- df_wide$D1.4
  df_wide$D1.3_0 <- df_wide$D1.3
  df_wide$D1.4 <- df_wide$D1.4
  df_wide$D1.3 <- df_wide$D1.3
  
  # Stub in D1.1 (pharmacy ID) as NA since public data doesn't include it
  df_wide$D1.1 <- NA_character_
  
  # All patients in the public dataset are complete cases by design (published after cleaning)
  # So all_cases and complete_cases are the same
  all_cases <- df_wide
  
  # Build economic data frame for cost completeness accounting
  economic_data <- df_wide %>%
    select(
      D1.2,
      starts_with("cost_"),
      starts_with("medication_cost_file")
    ) %>%
    rename_with(~ gsub("_0$", "", .), starts_with("cost_")) %>%
    rename_with(~ gsub("_0$", "", .), starts_with("medication_cost_file"))
  
  # Add pharmacy ID and baseline identifiers
  economic_data$D1.1 <- NA_character_
  
  list(
    all_cases = all_cases,
    economic_data = economic_data,
    data_source = "public",
    load_timestamp = Sys.time()
  )
}

#' Get the active data source (raw or public) from config and environment
#'
#' @return Character: "raw" or "public"
#' @export
get_active_data_source <- function() {
  # Check environment override first
  env_override <- Sys.getenv("BOFE_DATA_SOURCE", unset = NA_character_)
  if (!is.na(env_override) && env_override %in% c("raw", "public")) {
    return(env_override)
  }
  
  # Fall back to config
  method_config("data_source", "source")
}

#' Check if public dataset exists at configured path
#'
#' @param public_csv_path Path to check (default from methods config)
#' @return Logical: TRUE if file exists
#' @export
public_dataset_exists <- function(public_csv_path = NULL) {
  if (is.null(public_csv_path)) {
    public_csv_path <- method_config("data_source", "public_dataset_path")
  }
  file.exists(public_csv_path)
}
