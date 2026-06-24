###########################################################################
# R/03_descriptives.R
# Purpose: Generate descriptive statistics and baseline characteristics tables.
# Input:   canonical cleaning artifact.
# Output:  canonical descriptives artifact.
###########################################################################

library(dplyr)
library(tidyr)
library(haven)
source("R/utils.R")

pipeline_started <- pipeline_phase_start(
  "03_descriptives",
  "generating baseline tables and missingness summaries"
)

cleaning_artifact <- read_canonical_artifact("cleaning")
all_cases_raw <- cleaning_artifact$all_cases
complete_cases_raw <- cleaning_artifact$complete_cases

# Remove labels to avoid vctrs comparison issues.
all_cases <- remove_labels(all_cases_raw)
complete_cases <- remove_labels(complete_cases_raw)

cat("Loaded cleaning artifact: ", nrow(all_cases), " ITT rows and ", nrow(complete_cases), " analyzed rows\n\n", sep = "")

ensure_artifact_dirs()

###########################################################################
# SECTION 1: Variable definitions
###########################################################################

# Continuous variables are reported as mean [95% CI] in the final tables.
continuous_vars <- c(
  "D3.1_0", "D3.2_0", "D3.3_0",
  "D3.10_1_0", "D3.10_2_0", "D3.10_3_0", "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", "D3.10_7_0",
  "D3.11_1_0", "D3.11_2_0",
  "ACT.SCORE_0", "CCQ.SCORE_0", "EQindex_0", "D5.2_0"
)

continuous_var_names <- c(
  "D3.1_0" = "Height (cm)",
  "D3.2_0" = "Weight (kg)",
  "D3.3_0" = "BMI",
  "D3.10_1_0" = "GP Visits",
  "D3.10_2_0" = "Nurse Visits",
  "D3.10_3_0" = "Therapist Visits",
  "D3.10_4_0" = "Emergency Visits",
  "D3.10_5_0" = "Outpatient Visits",
  "D3.10_6_0" = "Inpatient Visits",
  "D3.10_7_0" = "Inpatient Days",
  "D3.11_1_0" = "Social Worker Visits",
  "D3.11_2_0" = "Day Centre (per week)",
  "ACT.SCORE_0" = "Asthma Control Test (ACT) Score",
  "CCQ.SCORE_0" = "Clinical COPD Questionnaire (CCQ) Score",
  "EQindex_0" = "EQ-5D-5L Index",
  "D5.2_0" = "Number of Medications"
)

# Binary/categorical variables are reported as n/N (%).
proportion_vars <- c(
  "D2.2_0", "D2.7_0",
  "D3.5_1_0", "D3.5_2_0", "D3.5_3_0",
  "D3.7_1_0", "D3.7_2_0", "D3.7_3_0",
  "D3.8_0", "D3.9_0", "D3.12_0"
)

proportion_var_names <- c(
  "D2.2_0" = "Female",
  "D2.7_0" = "Live Alone",
  "D3.5_1_0" = "Flu Vaccination (past 12mo)",
  "D3.5_2_0" = "Pneumonia Vaccination (past 12mo)",
  "D3.5_3_0" = "COVID Vaccination (past 12mo)",
  "D3.7_1_0" = "Diabetes",
  "D3.7_2_0" = "Heart Disease",
  "D3.7_3_0" = "Other Chronic Disease",
  "D3.8_0" = "COVID Test Done",
  "D3.9_0" = "COVID Positive",
  "D3.12_0" = "Employed"
)

# Multi-level categorical variables are shown as one header row plus one row
# per category, including an explicit Missing row.
categorical_vars <- c(
  "D1.3_0", "D2.3_0", "D2.4_0", "D2.5_0", "D2.6_0", "D3.4_0", "D3.6_0"
)

categorical_var_names <- c(
  "D1.3_0" = "Condition",
  "D2.3_0" = "Age category",
  "D2.4_0" = "Ethnicity",
  "D2.5_0" = "Education",
  "D2.6_0" = "Selection source",
  "D3.4_0" = "BMI category",
  "D3.6_0" = "Smoking status"
)

categorical_value_maps <- list(
  "D1.3_0" = c(
    "1" = "Asthma",
    "2" = "COPD"
  ),
  "D2.3_0" = c(
    "1" = "18-30",
    "2" = "31-40",
    "3" = "41-50",
    "4" = "51-60",
    "5" = "61-70",
    "6" = "71-80",
    "7" = "81+"
  ),
  "D3.4_0" = c(
    "1" = "Severely underweight",
    "2" = "Underweight",
    "3" = "Healthy weight",
    "4" = "Overweight",
    "5" = "Obese class I",
    "6" = "Obese class II",
    "7" = "Obese class III"
  ),
  "D3.6_0" = c(
    "1" = "Current smoker",
    "2" = "Occasional smoker",
    "3" = "Former smoker",
    "4" = "Never smoked"
  )
)

# ACT is only applicable to asthma and CCQ only to COPD. Treat those rows as
# non-applicable rather than missing so the percentages stay interpretable.
get_applicable_subset <- function(data, var) {
  condition_col <- if ("condition" %in% names(data)) {
    "condition"
  } else if ("D1.3_0" %in% names(data)) {
    "D1.3_0"
  } else if ("D1.3" %in% names(data)) {
    "D1.3"
  } else {
    NA_character_
  }

  base_var <- sub("_[0-9]+$", "", var)
  if (!is.na(condition_col) && base_var == "ACT.SCORE") {
    return(data %>% filter(.data[[condition_col]] == 1))
  }
  if (!is.na(condition_col) && base_var == "CCQ.SCORE") {
    return(data %>% filter(.data[[condition_col]] == 2))
  }
  data
}

calc_missingness_stats <- function(data, var) {
  if (!(var %in% names(data))) {
    return(list(n_missing = NA_integer_, n_total = NA_integer_))
  }
  list(
    n_missing = sum(is.na(data[[var]])),
    n_total = nrow(data)
  )
}

format_missingness <- function(n_missing, n_total) {
  if (is.na(n_missing) || is.na(n_total) || n_total <= 0) return("NA")
  sprintf("%d/%d (%.1f%%)", n_missing, n_total, 100 * n_missing / n_total)
}

format_count_pct <- function(n_value, n_total) {
  if (is.na(n_value) || is.na(n_total) || n_total <= 0) return("NA")
  sprintf("%d/%d (%.1f%%)", n_value, n_total, 100 * n_value / n_total)
}

###########################################################################
# SECTION 2: Helper functions
###########################################################################

calc_continuous_stats <- function(data, var) {
  values <- data[[var]]
  values <- values[!is.na(values)]
  n <- length(values)
  mean_val <- if (n > 0) mean(values) else NA_real_
  sd_val <- if (n > 1) sd(values) else NA_real_
  se_val <- if (!is.na(sd_val) && n > 0) sd_val / sqrt(n) else NA_real_
  crit_val <- if (n > 1) qt(0.975, df = n - 1) else NA_real_
  ci_half_width <- if (!is.na(se_val) && !is.na(crit_val)) crit_val * se_val else NA_real_

  list(
    n = n,
    mean = mean_val,
    sd = sd_val,
    lower_ci = if (!is.na(mean_val) && !is.na(ci_half_width)) mean_val - ci_half_width else NA_real_,
    upper_ci = if (!is.na(mean_val) && !is.na(ci_half_width)) mean_val + ci_half_width else NA_real_
  )
}

calc_proportion_stats <- function(data, var) {
  n_total <- sum(!is.na(data[[var]]))
  n_yes <- sum(data[[var]] == 1, na.rm = TRUE)
  list(
    n_total = n_total,
    n_yes = n_yes,
    pct_yes = if (n_total > 0) 100 * n_yes / n_total else NA
  )
}

test_continuous <- function(data, var, group_var) {
  if (!(var %in% names(data)) || !(group_var %in% names(data))) return(NA_real_)
  subset <- data[, c(var, group_var), drop = FALSE]
  subset <- subset[complete.cases(subset), , drop = FALSE]
  if (nrow(subset) < 2 || length(unique(subset[[group_var]])) < 2) return(NA_real_)
  tryCatch(
    wilcox.test(subset[[var]] ~ subset[[group_var]], exact = FALSE)$p.value,
    error = function(e) NA
  )
}

test_proportion <- function(data, var, group_var) {
  if (!(var %in% names(data)) || !(group_var %in% names(data))) return(NA_real_)
  subset <- data[, c(var, group_var), drop = FALSE]
  subset <- subset[complete.cases(subset), , drop = FALSE]
  if (nrow(subset) < 2 || length(unique(subset[[group_var]])) < 2 || length(unique(subset[[var]])) < 2) {
    return(NA_real_)
  }
  cont_table <- table(subset[[var]], subset[[group_var]], useNA = "no")
  if (nrow(cont_table) < 2 || ncol(cont_table) < 2) return(NA_real_)
  tryCatch(
    chisq.test(cont_table)$p.value,
    error = function(e) NA
  )
}

format_mean_ci <- function(mean_val, lower_ci, upper_ci, digits = 2) {
  if (any(is.na(c(mean_val, lower_ci, upper_ci)))) return("NA")
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
    mean_val, lower_ci, upper_ci
  )
}

format_p_value <- function(p_val, digits = 3) {
  if (is.na(p_val)) return("NA")
  threshold <- 10^(-digits)
  if (p_val < threshold) {
    return(paste0("<", formatC(threshold, format = "f", digits = digits)))
  }
  formatC(p_val, format = "f", digits = digits)
}

format_std_diff <- function(x, digits = 3) {
  if (is.na(x) || !is.finite(x)) return("NA")
  formatC(x, format = "f", digits = digits)
}

calc_standardized_difference_continuous <- function(ig_data, cg_data, var) {
  ig_values <- ig_data[[var]]
  cg_values <- cg_data[[var]]
  ig_values <- ig_values[!is.na(ig_values)]
  cg_values <- cg_values[!is.na(cg_values)]

  if (length(ig_values) == 0 || length(cg_values) == 0) return(NA_real_)

  ig_mean <- mean(ig_values)
  cg_mean <- mean(cg_values)
  ig_var <- if (length(ig_values) > 1) stats::var(ig_values) else 0
  cg_var <- if (length(cg_values) > 1) stats::var(cg_values) else 0
  pooled_sd <- sqrt((ig_var + cg_var) / 2)

  if (!is.finite(pooled_sd) || pooled_sd <= 0) return(NA_real_)
  abs((ig_mean - cg_mean) / pooled_sd)
}

calc_standardized_difference_binary <- function(ig_data, cg_data, var) {
  ig_nonmissing <- !is.na(ig_data[[var]])
  cg_nonmissing <- !is.na(cg_data[[var]])
  ig_n <- sum(ig_nonmissing)
  cg_n <- sum(cg_nonmissing)

  if (ig_n == 0 || cg_n == 0) return(NA_real_)

  p_ig <- mean(ig_data[[var]][ig_nonmissing] == 1)
  p_cg <- mean(cg_data[[var]][cg_nonmissing] == 1)
  pooled_sd <- sqrt((p_ig * (1 - p_ig) + p_cg * (1 - p_cg)) / 2)

  if (!is.finite(pooled_sd) || pooled_sd <= 0) return(NA_real_)
  abs((p_ig - p_cg) / pooled_sd)
}

normalize_category_code <- function(x) {
  out <- as.character(x)
  suppressWarnings({
    numeric_x <- as.numeric(out)
  })
  is_integerish <- !is.na(numeric_x) & abs(numeric_x - round(numeric_x)) < 1e-8
  out[is_integerish] <- as.character(as.integer(round(numeric_x[is_integerish])))
  out
}

category_value_map <- function(var) {
  categorical_value_maps[[var]]
}

category_levels_for_var <- function(data, var) {
  value_map <- category_value_map(var)
  if (!is.null(value_map)) {
    return(names(value_map))
  }

  values <- normalize_category_code(data[[var]])
  sort(unique(values[!is.na(values)]))
}

category_label_for_value <- function(var, value) {
  value_map <- category_value_map(var)
  if (!is.null(value_map) && value %in% names(value_map)) {
    return(unname(value_map[[value]]))
  }
  paste("Category", value)
}

calc_standardized_difference_category <- function(ig_data, cg_data, var, value = NULL, missing = FALSE) {
  if (!(var %in% names(ig_data)) || !(var %in% names(cg_data))) {
    return(NA_real_)
  }

  ig_values <- normalize_category_code(ig_data[[var]])
  cg_values <- normalize_category_code(cg_data[[var]])
  ig_n <- length(ig_values)
  cg_n <- length(cg_values)

  if (ig_n == 0 || cg_n == 0) return(NA_real_)

  if (isTRUE(missing)) {
    p_ig <- mean(is.na(ig_data[[var]]))
    p_cg <- mean(is.na(cg_data[[var]]))
  } else {
    p_ig <- mean(!is.na(ig_values) & ig_values == value)
    p_cg <- mean(!is.na(cg_values) & cg_values == value)
  }

  pooled_sd <- sqrt((p_ig * (1 - p_ig) + p_cg * (1 - p_cg)) / 2)
  if (!is.finite(pooled_sd) || pooled_sd <= 0) return(NA_real_)
  abs((p_ig - p_cg) / pooled_sd)
}

test_categorical <- function(data, var, group_var) {
  if (!(var %in% names(data)) || !(group_var %in% names(data))) return(NA_real_)

  observed <- data[!is.na(data[[var]]) & !is.na(data[[group_var]]), , drop = FALSE]
  if (nrow(observed) == 0) return(NA_real_)

  values <- normalize_category_code(observed[[var]])
  if (length(unique(values)) < 2 || length(unique(observed[[group_var]])) != 2) {
    return(NA_real_)
  }

  tab <- table(values, observed[[group_var]])
  if (nrow(tab) < 2 || ncol(tab) != 2) return(NA_real_)

  sparse <- any(tab < 5)
  test <- suppressWarnings(chisq.test(tab, simulate.p.value = sparse, B = if (sparse) 10000 else 2000))
  unname(test$p.value)
}

# Summarise one continuous variable set by arm and add missingness.
generate_continuous_summary <- function(data, vars, var_names) {
  group_var <- if ("group" %in% names(data)) "group" else "D1.4"
  rows <- list()

  for (var in vars) {
    if (!(var %in% names(data))) next
    label <- if (var %in% names(var_names)) unname(var_names[[var]]) else var

    applicable_data <- get_applicable_subset(data, var)
    ig_data <- applicable_data %>% filter(.data[[group_var]] == "ig (intervention group)")
    cg_data <- applicable_data %>% filter(.data[[group_var]] == "cg (control group)")

    overall_stats <- calc_continuous_stats(applicable_data, var)
    ig_stats <- calc_continuous_stats(ig_data, var)
    cg_stats <- calc_continuous_stats(cg_data, var)
    p_val <- test_continuous(applicable_data, var, group_var)

    rows[[length(rows) + 1]] <- data.frame(
      Variable = label,
      Overall = format_mean_ci(overall_stats$mean, overall_stats$lower_ci, overall_stats$upper_ci),
      Intervention = format_mean_ci(ig_stats$mean, ig_stats$lower_ci, ig_stats$upper_ci),
      Control = format_mean_ci(cg_stats$mean, cg_stats$lower_ci, cg_stats$upper_ci),
      Missing_Overall = format_missingness(sum(is.na(applicable_data[[var]])), nrow(applicable_data)),
      Missing_Intervention = format_missingness(sum(is.na(ig_data[[var]])), nrow(ig_data)),
      Missing_Control = format_missingness(sum(is.na(cg_data[[var]])), nrow(cg_data)),
      P_Value = format_p_value(p_val),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

###########################################################################
# SECTION 4: Generate baseline characteristics tables
###########################################################################

generate_table1 <- function(data, missingness_data, dataset_name) {
  # Build one Table 1 row at a time so the output stays easy to audit.
  cat("\n=== GENERATING TABLE 1 FOR ", dataset_name, " ===\n", sep = "")
  group_var <- if ("group" %in% names(data)) "group" else "D1.4"
  missingness_group_var <- if ("group" %in% names(missingness_data)) "group" else "D1.4"

  table1_data <- list()

  # Add the continuous summary rows first.
  for (var in continuous_vars) {
    var_name <- continuous_var_names[var]
    if (is.na(var_name)) var_name <- var

    if (!(var %in% names(data))) next

    applicable_data <- get_applicable_subset(data, var)
    applicable_missing <- get_applicable_subset(missingness_data, var)
    ig_data <- applicable_data %>% filter(.data[[group_var]] == "ig (intervention group)")
    cg_data <- applicable_data %>% filter(.data[[group_var]] == "cg (control group)")
    ig_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "ig (intervention group)")
    cg_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "cg (control group)")
    missing_overall <- calc_missingness_stats(applicable_missing, var)
    missing_ig <- calc_missingness_stats(ig_missing, var)
    missing_cg <- calc_missingness_stats(cg_missing, var)

    stats_overall <- calc_continuous_stats(applicable_data, var)
    stats_ig <- calc_continuous_stats(ig_data, var)
    stats_cg <- calc_continuous_stats(cg_data, var)

    p_val <- test_continuous(applicable_data, var, group_var)

    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = var_name,
      Overall = format_mean_ci(stats_overall$mean, stats_overall$lower_ci, stats_overall$upper_ci),
      Intervention = format_mean_ci(stats_ig$mean, stats_ig$lower_ci, stats_ig$upper_ci),
      Control = format_mean_ci(stats_cg$mean, stats_cg$lower_ci, stats_cg$upper_ci),
      Std_Diff = format_std_diff(calc_standardized_difference_continuous(ig_data, cg_data, var)),
      Missing_Overall = format_missingness(missing_overall$n_missing, missing_overall$n_total),
      Missing_Intervention = format_missingness(missing_ig$n_missing, missing_ig$n_total),
      Missing_Control = format_missingness(missing_cg$n_missing, missing_cg$n_total),
      P_Value = format_p_value(p_val),
      stringsAsFactors = FALSE
    )
  }

  # Then add the binary and categorical rows.
  for (var in proportion_vars) {
    var_name <- proportion_var_names[var]
    if (is.na(var_name)) var_name <- var

    if (!(var %in% names(data))) next

    applicable_data <- get_applicable_subset(data, var)
    applicable_missing <- get_applicable_subset(missingness_data, var)
    ig_data <- applicable_data %>% filter(.data[[group_var]] == "ig (intervention group)")
    cg_data <- applicable_data %>% filter(.data[[group_var]] == "cg (control group)")
    ig_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "ig (intervention group)")
    cg_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "cg (control group)")
    missing_overall <- calc_missingness_stats(applicable_missing, var)
    missing_ig <- calc_missingness_stats(ig_missing, var)
    missing_cg <- calc_missingness_stats(cg_missing, var)

    stats_overall <- calc_proportion_stats(applicable_data, var)
    stats_ig <- calc_proportion_stats(ig_data, var)
    stats_cg <- calc_proportion_stats(cg_data, var)

    p_val <- test_proportion(applicable_data, var, group_var)

    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = var_name,
      Overall = sprintf("%d/%d (%.1f%%)",
                        stats_overall$n_yes, stats_overall$n_total, stats_overall$pct_yes),
      Intervention = sprintf("%d/%d (%.1f%%)",
                             stats_ig$n_yes, stats_ig$n_total, stats_ig$pct_yes),
      Control = sprintf("%d/%d (%.1f%%)",
                        stats_cg$n_yes, stats_cg$n_total, stats_cg$pct_yes),
      Std_Diff = format_std_diff(calc_standardized_difference_binary(ig_data, cg_data, var)),
      Missing_Overall = format_missingness(missing_overall$n_missing, missing_overall$n_total),
      Missing_Intervention = format_missingness(missing_ig$n_missing, missing_ig$n_total),
      Missing_Control = format_missingness(missing_cg$n_missing, missing_cg$n_total),
      P_Value = format_p_value(p_val),
      stringsAsFactors = FALSE
    )
  }

  # Finally add the multi-level categorical blocks with explicit Missing rows.
  for (var in categorical_vars) {
    var_name <- categorical_var_names[var]
    if (is.na(var_name)) var_name <- var

    if (!(var %in% names(data))) next

    applicable_data <- get_applicable_subset(data, var)
    applicable_missing <- get_applicable_subset(missingness_data, var)
    ig_data <- applicable_data %>% filter(.data[[group_var]] == "ig (intervention group)")
    cg_data <- applicable_data %>% filter(.data[[group_var]] == "cg (control group)")
    ig_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "ig (intervention group)")
    cg_missing <- applicable_missing %>% filter(.data[[missingness_group_var]] == "cg (control group)")
    missing_overall <- calc_missingness_stats(applicable_missing, var)
    missing_ig <- calc_missingness_stats(ig_missing, var)
    missing_cg <- calc_missingness_stats(cg_missing, var)
    p_val <- test_categorical(applicable_data, var, group_var)

    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = var_name,
      Overall = "",
      Intervention = "",
      Control = "",
      Std_Diff = "",
      Missing_Overall = format_missingness(missing_overall$n_missing, missing_overall$n_total),
      Missing_Intervention = format_missingness(missing_ig$n_missing, missing_ig$n_total),
      Missing_Control = format_missingness(missing_cg$n_missing, missing_cg$n_total),
      P_Value = format_p_value(p_val),
      stringsAsFactors = FALSE
    )

    category_levels <- category_levels_for_var(applicable_missing, var)
    values_all <- normalize_category_code(applicable_data[[var]])
    values_ig <- normalize_category_code(ig_data[[var]])
    values_cg <- normalize_category_code(cg_data[[var]])

    for (value in category_levels) {
      n_all <- sum(!is.na(values_all) & values_all == value)
      n_ig <- sum(!is.na(values_ig) & values_ig == value)
      n_cg <- sum(!is.na(values_cg) & values_cg == value)

      table1_data[[length(table1_data) + 1]] <- data.frame(
        Variable = paste0("  ", category_label_for_value(var, value)),
        Overall = format_count_pct(n_all, nrow(applicable_data)),
        Intervention = format_count_pct(n_ig, nrow(ig_data)),
        Control = format_count_pct(n_cg, nrow(cg_data)),
        Std_Diff = format_std_diff(calc_standardized_difference_category(ig_data, cg_data, var, value = value)),
        Missing_Overall = "",
        Missing_Intervention = "",
        Missing_Control = "",
        P_Value = "",
        stringsAsFactors = FALSE
      )
    }

    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = "  Missing",
      Overall = format_count_pct(sum(is.na(applicable_data[[var]])), nrow(applicable_data)),
      Intervention = format_count_pct(sum(is.na(ig_data[[var]])), nrow(ig_data)),
      Control = format_count_pct(sum(is.na(cg_data[[var]])), nrow(cg_data)),
      Std_Diff = format_std_diff(calc_standardized_difference_category(ig_data, cg_data, var, missing = TRUE)),
      Missing_Overall = "",
      Missing_Intervention = "",
      Missing_Control = "",
      P_Value = "",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, table1_data)
}

# Build Table 1 from the full pre-imputation cohort now that missing values are
# represented directly within the categorical blocks.
pipeline_phase_info("03_descriptives", "building baseline characteristics tables")
table1_baseline <- generate_table1(all_cases, all_cases, "ALL_CASES (PRE-IMPUTATION, N=835)")

cat("Built baseline Table 1 from full pre-imputation cohort (N=", nrow(all_cases), ")\n\n", sep = "")

# Missingness is summarized separately so the Table 1 outputs stay focused.

cat("=== GENERATING MISSINGNESS SUMMARY ===\n")
pipeline_phase_info("03_descriptives", "summarising outcome missingness across timepoints")

outcome_vars <- c("ACT.SCORE", "CCQ.SCORE", "EQindex", "controlled")
timepoints <- TIMEPOINTS

missingness_summary <- data.frame(
  Timepoint = integer(0),
  Variable = character(0),
  Basis = character(0),
  Applicable_N_ITT = integer(0),
  N_Missing_ITT = integer(0),
  Pct_Missing_ITT = numeric(0),
  Applicable_N_Analyzed = integer(0),
  N_Missing_Analyzed = integer(0),
  Pct_Missing_Analyzed = numeric(0),
  stringsAsFactors = FALSE
)

for (tp in timepoints) {
  # Walk timepoint by timepoint to keep the output aligned with the trial visits.
  for (outcome in outcome_vars) {
    var_name <- paste0(outcome, "_", tp)
    if (var_name %in% names(all_cases)) {
      all_applicable <- get_applicable_subset(all_cases, var_name)
      analyzed_applicable <- get_applicable_subset(complete_cases, var_name)
      n_missing_itt <- sum(is.na(all_applicable[[var_name]]))
      n_missing_analyzed <- sum(is.na(analyzed_applicable[[var_name]]))
      basis_label <- if (grepl("^ACT\\.SCORE", outcome)) {
        "asthma only"
      } else if (grepl("^CCQ\\.SCORE", outcome)) {
        "COPD only"
      } else {
        "all patients"
      }

      missingness_summary <- missingness_summary %>% add_row(
        Timepoint = tp,
        Variable = outcome,
        Basis = basis_label,
        Applicable_N_ITT = nrow(all_applicable),
        N_Missing_ITT = n_missing_itt,
        Pct_Missing_ITT = if (nrow(all_applicable) > 0) 100 * n_missing_itt / nrow(all_applicable) else NA_real_,
        Applicable_N_Analyzed = nrow(analyzed_applicable),
        N_Missing_Analyzed = n_missing_analyzed,
        Pct_Missing_Analyzed = if (nrow(analyzed_applicable) > 0) 100 * n_missing_analyzed / nrow(analyzed_applicable) else NA_real_
      )
    }
  }
}

cat("Built missingness summary\n")

# Add the two extra descriptive tables requested for the complete-case cohort.
cost_summary_vars <- c(
  "interv_cost", "outpatient_cost", "lab_cost", "med_cost",
  "delivery_cost", "inpatient_cost", "total_cost"
)
cost_summary_names <- c(
  "interv_cost" = "Intervention Cost",
  "outpatient_cost" = "Outpatient Cost",
  "lab_cost" = "Laboratory Cost",
  "med_cost" = "Medication Cost",
  "delivery_cost" = "Delivery Cost",
  "inpatient_cost" = "Inpatient Cost",
  "total_cost" = "Total Cost"
)
complete_cases_cea <- prepare_cea_patient_level(complete_cases, require_cost_data = FALSE)
cost_summary <- generate_continuous_summary(complete_cases_cea, cost_summary_vars, cost_summary_names)

resource_use_vars <- c(
  "D3.10_1_0", "D3.10_2_0", "D3.10_3_0", "D3.10_4_0",
  "D3.10_5_0", "D3.10_6_0", "D3.10_7_0", "D3.11_1_0", "D3.11_2_0"
)
resource_use_names <- c(
  "D3.10_1_0" = "GP Visits",
  "D3.10_2_0" = "Nurse Visits",
  "D3.10_3_0" = "Therapist Visits",
  "D3.10_4_0" = "Emergency Visits",
  "D3.10_5_0" = "Outpatient Visits",
  "D3.10_6_0" = "Inpatient Visits",
  "D3.10_7_0" = "Inpatient Days",
  "D3.11_1_0" = "Social Worker Visits",
  "D3.11_2_0" = "Day Centre Attendance"
)
resource_use_summary <- generate_continuous_summary(complete_cases, resource_use_vars, resource_use_names)

descriptives_artifact <- list(
  stage = "03_descriptives",
  table1_baseline = table1_baseline,
  table1_analyzed = table1_baseline,
  missingness_summary = missingness_summary,
  cost_summary = cost_summary,
  resource_use_summary = resource_use_summary
)
write_canonical_artifact("descriptives", descriptives_artifact)
write_result_csv(table1_baseline, "table1_all_cases_characteristics.csv")
tryCatch(
  write_result_csv(table1_baseline, "table1_complete_cases_characteristics.csv"),
  error = function(e) {
    pipeline_phase_info(
      "03_descriptives",
      paste0(
        "could not refresh legacy table1_complete_cases_characteristics.csv alias: ",
        conditionMessage(e)
      )
    )
    invisible(NULL)
  }
)
write_result_csv(missingness_summary, "missingness_summary.csv")
write_result_csv(cost_summary, "cost_summary_complete_cases.csv")
write_result_csv(resource_use_summary, "resource_use_summary_complete_cases.csv")

cat("\n=== MISSINGNESS PREVIEW ===\n")
print(missingness_summary)

pipeline_phase_end(
  "03_descriptives",
  pipeline_started,
  "saved canonical descriptives artifact"
)

cat("\nR/03_descriptives.R completed successfully\n")
