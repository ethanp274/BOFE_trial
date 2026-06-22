# R/01b_publication_long_dataset.R
# Build an anonymized, publication-ready long-form dataset after cleaning and
# before imputation. This script does not alter any canonical analysis artifact.

source("R/utils.R")

pipeline_started <- pipeline_phase_start(
  "01b_publication_long_dataset",
  "creating anonymized long-form publication dataset"
)

cleaning_path <- file.path(DATA_PROCESSED_DIR, "cleaning_artifact.rds")
if (!file.exists(cleaning_path)) {
  stop("Missing ", cleaning_path, ". Run R/01_cleaning.R before this export.")
}

artifact <- readRDS(cleaning_path)
if (is.null(artifact$all_cases)) {
  stop("cleaning_artifact.rds does not contain all_cases.")
}

all_cases <- standardize_core_identifiers(as.data.frame(artifact$all_cases))

required_wide_columns <- c(
  "patient", "group", "condition", "gender", "age", "D2.1_0",
  "D3.4_0", "D3.6_0", "D3.7_2_0", "cost_complete", "cost_medication_file_present",
  paste0("controlled_", TIMEPOINTS),
  paste0("EQindex_", TIMEPOINTS),
  unlist(lapply(TIMEPOINTS, function(tp) paste0("EQ5D5L.", 1:5, "_", tp)), use.names = FALSE),
  COST_SUMMARY_COLUMNS
)

missing_required <- setdiff(required_wide_columns, names(all_cases))
if (length(missing_required) > 0) {
  stop(
    "Publication export is missing required cleaned columns: ",
    paste(missing_required, collapse = ", ")
  )
}

clean_binary <- function(x) {
  out <- as_numeric_safe(x)
  ifelse(is.na(out), NA_integer_, as.integer(out))
}

map_public_category <- function(x, value_map, variable_name) {
  key <- as.character(as_numeric_safe(x))
  out <- unname(value_map[key])
  unmapped <- unique(key[!is.na(key) & is.na(out)])
  if (length(unmapped) > 0) {
    stop(
      "Publication export has unmapped values for ",
      variable_name,
      ": ",
      paste(unmapped, collapse = ", ")
    )
  }
  out
}

age_category_map <- c(
  "1" = "18_to_30",
  "2" = "31_to_40",
  "3" = "41_to_50",
  "4" = "51_to_60",
  "5" = "61_to_70",
  "6" = "71_to_80",
  "7" = "81_plus"
)

gender_category_map <- c(
  "1" = "female",
  "2" = "male",
  "3" = "prefer_not_to_say",
  "4" = "transgender"
)

bmi_category_map <- c(
  "1" = "severely_underweight",
  "2" = "underweight",
  "3" = "healthy_weight",
  "4" = "overweight",
  "5" = "obese_class_1",
  "6" = "obese_class_2",
  "7" = "obese_class_3"
)

smoking_category_map <- c(
  "1" = "current_smoker",
  "2" = "occasional_smoker",
  "3" = "former_smoker",
  "4" = "never_smoked"
)

publication_seed <- 20260622L
set.seed(publication_seed)

source_patients <- sort(unique(as.character(all_cases$patient)))
anon_patient_ids <- sample(sprintf("P%04d", seq_along(source_patients)))
patient_key <- setNames(anon_patient_ids, source_patients)

source_locations <- sort(unique(as.character(all_cases$D2.1_0[!is.na(all_cases$D2.1_0)])))
if (length(source_locations) > length(LETTERS)) {
  stop("More anonymized residence categories are present than supported by LETTERS.")
}
location_letters <- sample(LETTERS[seq_along(source_locations)])
location_key <- setNames(location_letters, source_locations)

core_patient_frame <- data.frame(
  patient = as.character(all_cases$patient),
  anon_patient_id = unname(patient_key[as.character(all_cases$patient)]),
  trial_arm = ifelse(
    as.character(all_cases$group) == "ig (intervention group)",
    "intervention",
    ifelse(as.character(all_cases$group) == "cg (control group)", "control", NA_character_)
  ),
  condition = ifelse(
    as_numeric_safe(all_cases$condition) == 1,
    "asthma",
    ifelse(as_numeric_safe(all_cases$condition) == 2, "copd", NA_character_)
  ),
  residence_location_category = unname(location_key[as.character(all_cases$D2.1_0)]),
  age_category = map_public_category(all_cases$age, age_category_map, "age_category"),
  gender_category = map_public_category(all_cases$gender, gender_category_map, "gender_category"),
  bmi_category = map_public_category(all_cases$D3.4_0, bmi_category_map, "bmi_category"),
  smoking_category = map_public_category(all_cases$D3.6_0, smoking_category_map, "smoking_category"),
  ischaemic_heart_disease = clean_binary(all_cases$D3.7_2_0),
  cost_source_present = clean_binary(all_cases$cost_complete),
  medication_cost_file_present = clean_binary(all_cases$cost_medication_file_present),
  stringsAsFactors = FALSE
)

eq5d_item_map <- c(
  mobility = "EQ5D5L.1",
  self_care = "EQ5D5L.2",
  usual_activities = "EQ5D5L.3",
  pain_discomfort = "EQ5D5L.4",
  anxiety_depression = "EQ5D5L.5"
)

cost_time_map <- list(
  outpatient_cost = c("6" = "cost_M6", "12" = "cost_M12"),
  laboratory_cost = c("6" = "cost_C6", "12" = "cost_C12"),
  medication_cost = c("6" = "cost_F6", "12" = "cost_F12"),
  delivery_cost = c("6" = "cost_H6", "12" = "cost_H12"),
  inpatient_cost = c("6" = "cost_O6", "12" = "cost_O12")
)

long_rows <- lapply(TIMEPOINTS, function(tp) {
  row <- core_patient_frame
  row$timepoint_month <- tp
  row$disease_controlled <- clean_binary(all_cases[[paste0("controlled_", tp)]])
  row$eq5d_index <- as_numeric_safe(all_cases[[paste0("EQindex_", tp)]])

  for (item_name in names(eq5d_item_map)) {
    row[[paste0("eq5d_", item_name)]] <- as_numeric_safe(
      all_cases[[paste0(eq5d_item_map[[item_name]], "_", tp)]]
    )
  }

  row$cost_period_months <- ifelse(
    tp == 6,
    "0_to_6",
    ifelse(tp == 12, "6_to_12", NA_character_)
  )
  for (cost_name in names(cost_time_map)) {
    row[[cost_name]] <- NA_real_
    time_key <- as.character(tp)
    if (time_key %in% names(cost_time_map[[cost_name]])) {
      row[[cost_name]] <- as_numeric_safe(all_cases[[cost_time_map[[cost_name]][[time_key]]]])
    }
  }

  row
})

publication_long <- do.call(rbind, long_rows)
publication_long <- publication_long[order(
  publication_long$anon_patient_id,
  publication_long$timepoint_month
), , drop = FALSE]
row.names(publication_long) <- NULL

export_columns <- c(
  "anon_patient_id", "timepoint_month", "trial_arm", "condition",
  "residence_location_category", "age_category", "gender_category",
  "bmi_category", "smoking_category", "ischaemic_heart_disease",
  "disease_controlled", "eq5d_index",
  paste0("eq5d_", names(eq5d_item_map)),
  "cost_period_months", names(cost_time_map),
  "cost_source_present", "medication_cost_file_present"
)
publication_long <- publication_long[, export_columns, drop = FALSE]

forbidden_exact_names <- c("patient", "pharmacy", "D1.1", "D1.2", "D2.1_0")
if (any(names(publication_long) %in% forbidden_exact_names)) {
  stop(
    "Publication export contains forbidden source identifier columns: ",
    paste(intersect(names(publication_long), forbidden_exact_names), collapse = ", ")
  )
}
if (any(grepl("^D1\\.", names(publication_long)))) {
  stop("Publication export contains raw D1.* identifier/design columns.")
}

if (any(is.na(publication_long$anon_patient_id)) || anyDuplicated(publication_long[, c("anon_patient_id", "timepoint_month")])) {
  stop("Anonymous patient IDs are missing or duplicated within timepoint.")
}

if (nrow(publication_long) != length(source_patients) * length(TIMEPOINTS)) {
  stop("Unexpected publication export row count.")
}

output_csv <- file.path(DATA_PROCESSED_DIR, "bofe_publication_anonymized_long.csv")
write.csv(publication_long, output_csv, row.names = FALSE, na = "NA")

codebook_path <- file.path(DATA_PROCESSED_DIR, "bofe_publication_anonymized_long_codebook.md")
codebook_lines <- c(
  "# BOFE Publication Anonymized Long Dataset Codebook",
  "",
  paste0("Generated by `R/01b_publication_long_dataset.R` on ", Sys.Date(), "."),
  "",
  "This CSV is derived from `data_processed/cleaning_artifact.rds` after cleaning and before imputation. It is intended for publication alongside the manuscript and does not alter the main analysis artifacts.",
  "",
  "## Anonymization",
  "",
  "- `anon_patient_id` is a reproducible random short ID generated with a fixed seed; the source-to-anonymous mapping is not exported.",
  "- Original patient IDs, pharmacy IDs, and raw residence codes are excluded.",
  "- `residence_location_category` is an anonymized letter category; no mapping back to the original residence categories is exported.",
  "- Age, sex/gender, BMI, and smoking are exported only as interpretable categorical analysis labels.",
  "- Missing values are written as literal `NA` in the CSV.",
  "",
  "## Variables",
  "",
  "- `anon_patient_id`: anonymized patient ID.",
  "- `timepoint_month`: study month, one of 0, 3, 6, 9, 12.",
  "- `trial_arm`: randomized arm, control or intervention.",
  "- `condition`: asthma or COPD.",
  "- `residence_location_category`: anonymized baseline residence category letter.",
  "- `age_category`: baseline age category; values are `18_to_30`, `31_to_40`, `41_to_50`, `51_to_60`, `61_to_70`, `71_to_80`, and `81_plus`.",
  "- `gender_category`: baseline gender category; possible source labels are `female`, `male`, `prefer_not_to_say`, and `transgender`.",
  "- `bmi_category`: baseline BMI category; values are `severely_underweight`, `underweight`, `healthy_weight`, `overweight`, `obese_class_1`, `obese_class_2`, and `obese_class_3`.",
  "- `smoking_category`: baseline smoking category; values are `current_smoker`, `occasional_smoker`, `former_smoker`, and `never_smoked`.",
  "- `ischaemic_heart_disease`: baseline binary comorbidity indicator.",
  "- `disease_controlled`: harmonized disease-control outcome; asthma uses ACT >= 20 and COPD uses CCQ < 2.",
  "- `eq5d_index`: cleaned EQ-5D-5L utility index for the configured main tariff.",
  "- `eq5d_mobility`, `eq5d_self_care`, `eq5d_usual_activities`, `eq5d_pain_discomfort`, `eq5d_anxiety_depression`: cleaned EQ-5D-5L item levels, valid values 1-5.",
  "- `cost_period_months`: `0_to_6` on the 6-month row, `6_to_12` on the 12-month row, and `NA` on rows without a half-year cost summary.",
  "- `outpatient_cost`, `laboratory_cost`, `medication_cost`, `delivery_cost`, `inpatient_cost`: canonical half-year cost summaries aligned to the 6- or 12-month row.",
  "- `cost_source_present`: 1 if the patient appears in any raw economic cost source file under the project cost-completeness rule; 0 otherwise.",
  "- `medication_cost_file_present`: 1 if the patient appears in the raw medication cost source file; 0 otherwise.",
  "",
  "## Notes",
  "",
  "The file is intentionally pre-imputation. `NA` values indicate fields that are missing or structurally unavailable in the cleaned source data and should be handled by the analysis imputation model rather than manually filled in the publication file."
)
writeLines(codebook_lines, codebook_path, useBytes = TRUE)

pipeline_phase_info(
  "01b_publication_long_dataset",
  sprintf(
    "Wrote %s with %d rows, %d patients, and %d columns.",
    output_csv,
    nrow(publication_long),
    length(source_patients),
    ncol(publication_long)
  )
)
pipeline_phase_end(
  "01b_publication_long_dataset",
  pipeline_started,
  "publication anonymized long dataset export complete"
)
