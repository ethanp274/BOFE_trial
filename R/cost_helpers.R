# R/cost_helpers.R
# Raw economic-data compilation and cost-summary helpers.

read_cost_file <- function(path, suffix) {
  df <- read.csv(path, header = TRUE, check.names = FALSE)
  cost_cols <- setdiff(names(df), "PAZIENTE")
  names(df)[names(df) %in% cost_cols] <- paste0(cost_cols, "_", suffix)
  for (col in setdiff(names(df), "PAZIENTE")) {
    df[[col]] <- as_numeric_safe(df[[col]])
    df[[col]][df[[col]] < 0 | df[[col]] == 9999] <- NA
  }
  df
}

sum_cost_period <- function(df, months, suffix) {
  cols <- paste0(months, "_", suffix)
  row_sums_strict(df, cols)
}

build_economic_data <- function(raw_dir = "raw_data") {
  cost_M <- read_cost_file(file.path(raw_dir, "bofe_cost_outpatient_clean.csv"), "M")
  cost_C <- read_cost_file(file.path(raw_dir, "bofe_cost_labs_clean.csv"), "C")
  cost_F <- read_cost_file(file.path(raw_dir, "bofe_cost_medication_clean.csv"), "F")
  cost_H <- read_cost_file(file.path(raw_dir, "bofe_cost_delivery_clean.csv"), "H")
  cost_O <- read_cost_file(file.path(raw_dir, "bofe_cost_inpatient_clean.csv"), "O")

  economic_data <- Reduce(
    function(x, y) merge(x, y, by = "PAZIENTE", all = TRUE),
    list(cost_M, cost_C, cost_F, cost_H, cost_O)
  )
  names(economic_data)[names(economic_data) == "PAZIENTE"] <- "D1.2"
  economic_data[["patient"]] <- economic_data[["D1.2"]]

  economic_data[["cost_C6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "C")
  economic_data[["cost_C12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "C")
  economic_data[["cost_M6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "M")
  economic_data[["cost_M12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "M")
  economic_data[["cost_F6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "F")
  economic_data[["cost_F12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "F")
  economic_data[["cost_H6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "H")
  economic_data[["cost_H12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "H")
  economic_data[["cost_O6"]] <- sum_cost_period(economic_data, COST_MONTHS_FIRST_HALF, "O")
  economic_data[["cost_O12"]] <- sum_cost_period(economic_data, COST_MONTHS_SECOND_HALF, "O")

  # Keep the canonical economic frame lean: downstream code only uses the
  # patient identifier plus the half-year summary columns.
  economic_data <- economic_data[, c("D1.2", "patient", COST_SUMMARY_COLUMNS), drop = FALSE]
  assert_data_contract(economic_data, "economic_cost_summary")
  economic_data
}

attach_cost_summaries <- function(df, economic_data) {
  keep_cols <- c(primary_patient_col(economic_data), COST_SUMMARY_COLUMNS)
  keep_cols <- present_cols(economic_data, keep_cols)
  merge(df, economic_data[, keep_cols, drop = FALSE], by.x = primary_patient_col(df), by.y = primary_patient_col(economic_data), all.x = TRUE, sort = FALSE)
}

add_completeness_flags <- function(df) {
  disease <- if ("D1.3_0" %in% names(df)) as_numeric_safe(df[["D1.3_0"]]) else as_numeric_safe(df[["D1.3"]])
  act_cols <- paste0("ACT.SCORE_", TIMEPOINTS)
  ccq_cols <- paste0("CCQ.SCORE_", TIMEPOINTS)
  eq_cols <- paste0("EQindex_", TIMEPOINTS)

  has_act <- all(act_cols %in% names(df))
  has_ccq <- all(ccq_cols %in% names(df))
  has_eq <- all(eq_cols %in% names(df))

  df[["complete_q"]] <- 0L
  if (has_act && has_eq) {
    df[["complete_q"]][disease == 1 & complete.cases(df[, c(act_cols, eq_cols), drop = FALSE])] <- 1L
  }
  if (has_ccq && has_eq) {
    df[["complete_q"]][disease == 2 & complete.cases(df[, c(ccq_cols, eq_cols), drop = FALSE])] <- 1L
  }
  patient_id_col <- primary_patient_col(df)
  df[["complete_q"]][df[[patient_id_col]] %in% method_config("cleaning", "complete_questionnaire_exclusions")] <- 0L

  df[["complete_c"]] <- 0L
  cost_cols <- present_cols(df, COST_SUMMARY_COLUMNS)
  if (length(cost_cols) > 0) {
    has_complete_cost <- complete.cases(df[, cost_cols, drop = FALSE])
    df[["complete_c"]][has_complete_cost] <- 1L
  }

  df[["complete"]] <- ifelse(df[["complete_q"]] == 1L & df[["complete_c"]] == 1L, 1L, 0L)
  df
}
