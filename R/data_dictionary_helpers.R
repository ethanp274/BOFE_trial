# R/data_dictionary_helpers.R
# Variable alias and column-discovery helpers.

build_wide_analysis_alias_map <- function() {
  c(
    patient = "D1.2",
    pharmacy = "D1.1",
    condition = "D1.3_0",
    group = "D1.4_0",
    location = "D2.1_0",
    gender = "D2.2_0",
    age = "D2.3_0",
    ethnicity = "D2.4_0",
    education = "D2.5_0",
    selection = "D2.6_0",
    live_alone = "D2.7_0",
    FVC_0 = "D2.8_0",
    FEV1_0 = "D2.9_0",
    height = "D3.1_0",
    weight = "D3.2_0",
    BMI = "D3.3_0",
    BMI_range = "D3.4_0",
    smoking = "D3.6_0",
    diabetes = "D3.7_1_0",
    ihd = "D3.7_2_0",
    employed = "D3.12_0",
    num_meds = "D5.2_0",
    gp_0 = "D3.10_1_0",
    nurse_0 = "D3.10_2_0",
    therapist_0 = "D3.10_3_0",
    ae_0 = "D3.10_4_0",
    outpatient_0 = "D3.10_5_0",
    inpatient_0 = "D3.10_6_0",
    inpatient_days_0 = "D3.10_7_0",
    sw_0 = "D3.11_1_0",
    daycare_0 = "D3.11_2_0",
    gp_3 = "D3.10_1_3",
    nurse_3 = "D3.10_2_3",
    therapist_3 = "D3.10_3_3",
    ae_3 = "D3.10_4_3",
    outpatient_3 = "D3.10_5_3",
    inpatient_3 = "D3.10_6_3",
    inpatient_days_3 = "D3.10_7_3",
    sw_3 = "D3.11_1_3",
    daycare_3 = "D3.11_2_3",
    gp_6 = "D3.10_1_6",
    nurse_6 = "D3.10_2_6",
    therapist_6 = "D3.10_3_6",
    ae_6 = "D3.10_4_6",
    outpatient_6 = "D3.10_5_6",
    inpatient_6 = "D3.10_6_6",
    inpatient_days_6 = "D3.10_7_6",
    sw_6 = "D3.11_1_6",
    daycare_6 = "D3.11_2_6",
    gp_9 = "D3.10_1_9",
    nurse_9 = "D3.10_2_9",
    therapist_9 = "D3.10_3_9",
    ae_9 = "D3.10_4_9",
    outpatient_9 = "D3.10_5_9",
    inpatient_9 = "D3.10_6_9",
    inpatient_days_9 = "D3.10_7_9",
    sw_9 = "D3.11_1_9",
    daycare_9 = "D3.11_2_9",
    gp_12 = "D3.10_1_12",
    nurse_12 = "D3.10_2_12",
    therapist_12 = "D3.10_3_12",
    ae_12 = "D3.10_4_12",
    outpatient_12 = "D3.10_5_12",
    inpatient_12 = "D3.10_6_12",
    inpatient_days_12 = "D3.10_7_12",
    sw_12 = "D3.11_1_12",
    daycare_12 = "D3.11_2_12",
    med_adherence_0 = "D5.9_0",
    last_missed_dose_0 = "D5.10_0",
    med_adherence_3 = "D5.9_3",
    last_missed_dose_3 = "D5.10_3",
    med_adherence_6 = "D5.9_6",
    last_missed_dose_6 = "D5.10_6",
    med_adherence_9 = "D5.9_9",
    last_missed_dose_9 = "D5.10_9",
    med_adherence_12 = "D5.9_12",
    last_missed_dose_12 = "D5.10_12"
  )
}

rename_by_aliases <- function(df, alias_map) {
  if (is.null(alias_map) || length(alias_map) == 0) {
    return(df)
  }

  for (alias in names(alias_map)) {
    source_name <- unname(alias_map[[alias]])
    if (source_name %in% names(df)) {
      names(df)[names(df) == source_name] <- alias
    }
  }
  df
}

present_cols <- function(df, cols) {
  cols[cols %in% names(df)]
}

infer_timepoint_from_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    return(NA_integer_)
  }
  match_obj <- regexec("([0-9]+)$", name)
  reg <- regmatches(name, match_obj)[[1]]
  if (length(reg) == 0) return(NA_integer_)
  as.integer(reg[2])
}

primary_patient_col <- function(df) {
  if ("patient" %in% names(df)) "patient" else "D1.2"
}

primary_group_col <- function(df) {
  if ("group" %in% names(df)) "group" else "D1.4"
}

primary_condition_col <- function(df) {
  if ("condition" %in% names(df)) "condition" else "D1.3_0"
}

