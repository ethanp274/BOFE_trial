# R/00_contracts.R
# Explicit data-shape contracts for the BOFE pipeline.

contract_eqindex_columns <- paste0("EQindex_", TIMEPOINTS)
contract_controlled_columns <- paste0("controlled_", TIMEPOINTS)
contract_eq5d_item_columns <- unlist(
  lapply(TIMEPOINTS, function(tp) {
    paste0(c("EQ5D5L.1", "EQ5D5L.2", "EQ5D5L.3", "EQ5D5L.4", "EQ5D5L.5"), "_", tp)
  }),
  use.names = FALSE
)

BOFE_DATA_CONTRACTS <- list(
  analysis_wide = list(
    description = "Canonical cleaned wide analysis frame after R/01_cleaning.R.",
    row_grain = "One row per ITT patient.",
    unique_key = "patient",
    required_columns = c(
      "patient", "pharmacy", "condition", "group", "gender", "age",
      contract_controlled_columns,
      contract_eqindex_columns,
      COST_SUMMARY_COLUMNS
    ),
    notes = c(
      "Cost columns are half-year summary totals, not monthly panel columns.",
      "Disease-specific outcome columns may be structurally missing; endpoint definitions are declared in R/00_methods_config.R."
    )
  ),
  economic_cost_summary = list(
    description = "Canonical cost-summary frame created from raw monthly cost CSVs.",
    row_grain = "One row per patient with half-year cost summaries.",
    unique_key = "patient",
    required_columns = c("patient", COST_SUMMARY_COLUMNS),
    notes = c(
      "Only half-year cost summaries should leave build_economic_data().",
      "Raw monthly cost panels should not be carried downstream."
    )
  ),
  imputation_wide = list(
    description = "Wide MICE input/completed frame used for main imputation analyses.",
    row_grain = "One row per ITT patient.",
    unique_key = "patient",
    required_columns = c(
      "patient", "condition", "group", "gender", "age",
      contract_controlled_columns,
      contract_eqindex_columns,
      contract_eq5d_item_columns,
      COST_SUMMARY_COLUMNS
    ),
    notes = c(
      "Imputation and tariff-sensitivity method choices are declared in R/00_methods_config.R."
    )
  ),
  effectiveness_long = list(
    description = "Long repeated-measures frame used by GEE and mixed-effects effectiveness models.",
    row_grain = "One row per patient-time observation.",
    unique_key = NULL,
    required_columns = c(
      "patient", "group", "time", "age", "gender",
      "controlled_0", "controlled_t"
    ),
    notes = c(
      "Effectiveness model choices are declared in R/00_methods_config.R.",
      "Rows should be sorted by patient and time before clustered model fitting."
    )
  ),
  cea_patient_level = list(
    description = "Patient-level frame used by CEA GLM and nested MI bootstrap models.",
    row_grain = "One row per patient, except within bootstrap samples where duplicates are expected.",
    unique_key = "patient",
    required_columns = c(
      "patient", "condition", "group", "gender", "age", "controlled_0",
      "QALY", "QALY_model", "interv_cost",
      "outpatient_cost", "lab_cost", "med_cost", "delivery_cost", "inpatient_cost",
      "has_cost_data", "total_cost", "total_cost_gamma"
    ),
    notes = c(
      "CEA model choices are declared in R/00_methods_config.R.",
      "total_cost is the sum of intervention plus five cost components."
    )
  )
)

list_data_contracts <- function() {
  names(BOFE_DATA_CONTRACTS)
}

get_data_contract <- function(name) {
  if (!name %in% names(BOFE_DATA_CONTRACTS)) {
    stop(
      "get_data_contract: unknown contract '", name, "'. Available contracts: ",
      paste(names(BOFE_DATA_CONTRACTS), collapse = ", ")
    )
  }
  BOFE_DATA_CONTRACTS[[name]]
}

data_contract_report <- function(df, name) {
  contract <- get_data_contract(name)
  required <- contract$required_columns
  present <- required %in% names(df)
  data.frame(
    contract = name,
    column = required,
    present = present,
    stringsAsFactors = FALSE
  )
}

assert_required_columns <- function(df, required_columns, context = "data frame") {
  missing_cols <- setdiff(required_columns, names(df))
  if (length(missing_cols) > 0) {
    stop(
      context, " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  invisible(TRUE)
}

assert_unique_key <- function(df, key, context = "data frame") {
  if (is.null(key) || length(key) == 0 || is.na(key)) {
    return(invisible(TRUE))
  }
  assert_required_columns(df, key, context = context)
  duplicated_key <- duplicated(df[, key, drop = FALSE])
  if (any(duplicated_key)) {
    stop(
      context, " has duplicated key rows for ",
      paste(key, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

assert_data_contract <- function(
    df,
    name,
    require_unique_key = TRUE,
    allow_empty = FALSE
) {
  contract <- get_data_contract(name)
  context <- paste0("contract '", name, "'")
  if (!allow_empty && nrow(df) == 0) {
    stop(context, " received an empty data frame.")
  }
  assert_required_columns(df, contract$required_columns, context = context)
  if (isTRUE(require_unique_key)) {
    assert_unique_key(df, contract$unique_key, context = context)
  }
  invisible(TRUE)
}
