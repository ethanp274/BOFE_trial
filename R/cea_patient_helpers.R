# R/cea_patient_helpers.R
# Patient-level CEA construction helpers.

# Collapse the wide analysis frame to one row per patient for CEA.
prepare_cea_patient_level <- function(
    df,
    require_cost_data = TRUE,
    patient_ids = NULL,
    economic_data = NULL,
    intervention_cost_per_consultation = INTERVENTION_COST_PER_CONSULTATION,
    tariff = method_config("economics", "main_eq5d_tariff"),
    allow_duplicate_patients = FALSE
) {
  tariff <- match.arg(tariff, configured_eq5d_tariffs())
  df <- standardize_core_identifiers(df)

  if (!all(COST_SUMMARY_COLUMNS %in% names(df))) {
    if (is.null(economic_data)) {
      stop("prepare_cea_patient_level: cost summaries are missing and no economic_data was supplied.")
    }
    df <- attach_cost_summaries(df, economic_data)
  }

  if (!is.null(patient_ids)) {
    patient_ids <- unique(as.character(patient_ids))
    df <- df[as.character(df[[primary_patient_col(df)]]) %in% patient_ids, , drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("prepare_cea_patient_level: no patients were available after filtering.")
  }

  eq5d_raw_cols <- unlist(
    lapply(TIMEPOINTS, function(tp) {
      paste0(c("EQ5D5L.1", "EQ5D5L.2", "EQ5D5L.3", "EQ5D5L.4", "EQ5D5L.5"), "_", tp)
    }),
    use.names = FALSE
  )
  has_raw_eq5d <- all(eq5d_raw_cols %in% names(df))
  if (tariff == "uk" && !has_raw_eq5d) {
    stop(
      "prepare_cea_patient_level: UK tariff requested but raw EQ-5D item columns are missing. ",
      "Ensure the wide analysis frame retains the EQ5D5L.* columns before imputation."
    )
  }
  if (has_raw_eq5d) {
    df <- derive_eqindex(df, tariff = tariff)
  }

  required_cols <- c(
    "EQindex_0", "EQindex_3", "EQindex_6", "EQindex_9", "EQindex_12",
    "cost_C6", "cost_C12", "cost_M6", "cost_M12", "cost_F6", "cost_F12",
    "cost_H6", "cost_H12", "cost_O6", "cost_O12",
    "controlled_0"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "prepare_cea_patient_level: missing required wide columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  patient_level <- data.frame(
    patient = as.character(df[[primary_patient_col(df)]]),
    condition = as_numeric_safe(df[[primary_condition_col(df)]]),
    group = as.character(df[[primary_group_col(df)]]),
    gender = as_numeric_safe(df[["gender"]]),
    age = as_numeric_safe(df[["age"]]),
    controlled_0 = as_numeric_safe(df[["controlled_0"]]),
    QALY = 0.25 * (as_numeric_safe(df[["EQindex_0"]]) + as_numeric_safe(df[["EQindex_3"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_3"]]) + as_numeric_safe(df[["EQindex_6"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_6"]]) + as_numeric_safe(df[["EQindex_9"]])) / 2 +
      0.25 * (as_numeric_safe(df[["EQindex_9"]]) + as_numeric_safe(df[["EQindex_12"]])) / 2,
    interv_cost = ifelse(
      as.character(df[[primary_group_col(df)]]) == "ig (intervention group)",
      intervention_cost_per_consultation * 2,
      0
    ),
    outpatient_cost = as_numeric_safe(df[["cost_M6"]]) + as_numeric_safe(df[["cost_M12"]]),
    lab_cost = as_numeric_safe(df[["cost_C6"]]) + as_numeric_safe(df[["cost_C12"]]),
    med_cost = as_numeric_safe(df[["cost_F6"]]) + as_numeric_safe(df[["cost_F12"]]),
    delivery_cost = as_numeric_safe(df[["cost_H6"]]) + as_numeric_safe(df[["cost_H12"]]),
    inpatient_cost = as_numeric_safe(df[["cost_O6"]]) + as_numeric_safe(df[["cost_O12"]]),
    stringsAsFactors = FALSE
  )

  cost_component_cols <- c("outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost")
  patient_level$has_cost_data <- complete.cases(patient_level[, cost_component_cols, drop = FALSE])
  patient_level$total_cost <- rowSums(
    patient_level[, c("interv_cost", cost_component_cols), drop = FALSE],
    na.rm = FALSE
  )
  if (isTRUE(require_cost_data)) {
    patient_level <- patient_level[patient_level$has_cost_data, , drop = FALSE]
  }
  patient_level$total_cost_gamma <- patient_level$total_cost + 0.001
  patient_level$QALY_model <- patient_level$QALY
  patient_level$group <- factor(patient_level$group, levels = GROUP_LEVELS)
  patient_level$gender <- factor(patient_level$gender)
  patient_level$age <- factor(patient_level$age)
  patient_level$controlled_0 <- factor(patient_level$controlled_0, levels = c(0, 1))

  if (!isTRUE(allow_duplicate_patients) && anyDuplicated(patient_level$patient)) {
    stop("prepare_cea_patient_level: duplicated patient IDs were created.")
  }
  assert_data_contract(
    patient_level,
    "cea_patient_level",
    require_unique_key = !isTRUE(allow_duplicate_patients)
  )

  patient_level
}
