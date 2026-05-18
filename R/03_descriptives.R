###########################################################################
# R/03_descriptives.R
# Purpose: Generate descriptive statistics and baseline characteristics tables
# Input:   data_processed/all_cases.rds and complete_cases.rds
# Output:  
#   - outputs/table1_all_cases_characteristics.csv (ITT, N=835)
#   - outputs/table1_complete_cases_characteristics.csv (analyzed, N=756)
#   - outputs/missingness_summary.csv
#   - outputs/summary_by_disease.csv
###########################################################################

library(dplyr)
library(tidyr)
library(haven)
source("R/utils.R")

# Load data
all_cases_raw <- readRDS('data_processed/all_cases.rds')
complete_cases_raw <- readRDS('data_processed/complete_cases.rds')

# Remove labels to avoid vctrs comparison issues.
all_cases <- remove_labels(all_cases_raw)
complete_cases <- remove_labels(complete_cases_raw)

cat("Loaded all_cases.rds: ", nrow(all_cases), " rows (ITT population)\n", sep = "")
cat("Loaded complete_cases.rds: ", nrow(complete_cases), " rows (analyzed population)\n\n", sep = "")

# Ensure output directory exists
if (!dir.exists('outputs')) dir.create('outputs', showWarnings = FALSE)

###########################################################################
# SECTION 1: Variable definitions
###########################################################################

# Continuous variables (mean ± SD)
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

# Proportion variables (n, %)
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

###########################################################################
# SECTION 2: Helper functions
###########################################################################

calc_continuous_stats <- function(data, var) {
  list(
    n = sum(!is.na(data[[var]])),
    mean = mean(data[[var]], na.rm = TRUE),
    sd = sd(data[[var]], na.rm = TRUE)
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
  tryCatch(
    wilcox.test(data[[var]] ~ data[[group_var]], exact = FALSE)$p.value,
    error = function(e) NA
  )
}

test_proportion <- function(data, var, group_var) {
  cont_table <- table(data[[var]], data[[group_var]], useNA = "no")
  tryCatch(
    chisq.test(cont_table)$p.value,
    error = function(e) NA
  )
}

###########################################################################
# SECTION 4: Generate baseline characteristics tables
###########################################################################

generate_table1 <- function(data, dataset_name) {
  cat("\n=== GENERATING TABLE 1 FOR", dataset_name, "===\n")
  
  table1_data <- list()
  
  # Continuous variables
  for (var in continuous_vars) {
    var_name <- continuous_var_names[var]
    if (is.na(var_name)) var_name <- var
    
    if (!(var %in% names(data))) next
    
    stats_overall <- calc_continuous_stats(data, var)
    stats_ig <- calc_continuous_stats(
      data %>% filter(D1.4 == "ig (intervention group)"),
      var
    )
    stats_cg <- calc_continuous_stats(
      data %>% filter(D1.4 == "cg (control group)"),
      var
    )
    
    p_val <- test_continuous(data, var, "D1.4")
    
    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = var_name,
      Overall = sprintf("%.2f ± %.2f", stats_overall$mean, stats_overall$sd),
      Intervention = sprintf("%.2f ± %.2f", stats_ig$mean, stats_ig$sd),
      Control = sprintf("%.2f ± %.2f", stats_cg$mean, stats_cg$sd),
      P_Value = p_val
    )
  }
  
  # Proportion variables
  for (var in proportion_vars) {
    var_name <- proportion_var_names[var]
    if (is.na(var_name)) var_name <- var
    
    if (!(var %in% names(data))) next
    
    stats_overall <- calc_proportion_stats(data, var)
    stats_ig <- calc_proportion_stats(
      data %>% filter(D1.4 == "ig (intervention group)"),
      var
    )
    stats_cg <- calc_proportion_stats(
      data %>% filter(D1.4 == "cg (control group)"),
      var
    )
    
    p_val <- test_proportion(data, var, "D1.4")
    
    table1_data[[length(table1_data) + 1]] <- data.frame(
      Variable = var_name,
      Overall = sprintf("%d/%d (%.1f%%)", 
                        stats_overall$n_yes, stats_overall$n_total, stats_overall$pct_yes),
      Intervention = sprintf("%d/%d (%.1f%%)",
                             stats_ig$n_yes, stats_ig$n_total, stats_ig$pct_yes),
      Control = sprintf("%d/%d (%.1f%%)",
                        stats_cg$n_yes, stats_cg$n_total, stats_cg$pct_yes),
      P_Value = p_val
    )
  }
  
  return(do.call(rbind, table1_data))
}

# Generate for both populations
table1_itt <- generate_table1(all_cases, "ALL_CASES (ITT, N=835)")
table1_analyzed <- generate_table1(complete_cases, "COMPLETE_CASES (ANALYZED, N=756)")

write.csv(table1_itt, "outputs/table1_all_cases_characteristics.csv", row.names = FALSE)
write.csv(table1_analyzed, "outputs/table1_complete_cases_characteristics.csv", row.names = FALSE)

cat("\nSaved:\n  outputs/table1_all_cases_characteristics.csv (N=", nrow(all_cases), ")\n", sep = "")
cat("  outputs/table1_complete_cases_characteristics.csv (N=", nrow(complete_cases), ")\n\n", sep = "")

###########################################################################
# SECTION 4: Missingness summary by timepoint
###########################################################################

cat("=== GENERATING MISSINGNESS SUMMARY ===\n")

outcome_vars <- c("ACT.SCORE", "CCQ.SCORE", "EQindex", "controlled")
timepoints <- c(0, 3, 6, 9, 12)

missingness_summary <- data.frame(
  Timepoint = integer(0),
  Variable = character(0),
  N_Missing_ITT = integer(0),
  Pct_Missing_ITT = numeric(0),
  N_Missing_Analyzed = integer(0),
  Pct_Missing_Analyzed = numeric(0),
  stringsAsFactors = FALSE
)

for (tp in timepoints) {
  for (outcome in outcome_vars) {
    var_name <- paste0(outcome, "_", tp)
    if (var_name %in% names(all_cases)) {
      n_missing_itt <- sum(is.na(all_cases[[var_name]]))
      n_missing_analyzed <- sum(is.na(complete_cases[[var_name]]))
      
      missingness_summary <- missingness_summary %>% add_row(
        Timepoint = tp,
        Variable = outcome,
        N_Missing_ITT = n_missing_itt,
        Pct_Missing_ITT = 100 * n_missing_itt / nrow(all_cases),
        N_Missing_Analyzed = n_missing_analyzed,
        Pct_Missing_Analyzed = 100 * n_missing_analyzed / nrow(complete_cases)
      )
    }
  }
}

write.csv(missingness_summary, "outputs/missingness_summary.csv", row.names = FALSE)
cat("Saved: outputs/missingness_summary.csv\n")

###########################################################################
# SECTION 5: Summary by disease type
###########################################################################

cat("\n=== GENERATING DISEASE-STRATIFIED SUMMARIES ===\n")

summary_rows <- list()

for (pop_name in c("ITT", "Analyzed")) {
  data_to_use <- if (pop_name == "ITT") all_cases else complete_cases
  
  for (disease_code in c(1, 2)) {
    disease_name <- if (disease_code == 1) "Asthma" else "COPD"
    disease_data <- data_to_use %>% filter(D1.3 == disease_code)
    
    n_ig <- sum(disease_data$D1.4 == "ig (intervention group)", na.rm = TRUE)
    n_cg <- sum(disease_data$D1.4 == "cg (control group)", na.rm = TRUE)
    
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      Population = pop_name,
      Disease = disease_name,
      N = nrow(disease_data),
      N_Intervention = n_ig,
      N_Control = n_cg,
      Mean_Age = mean(disease_data$D2.3_0, na.rm = TRUE),
      Pct_Female = 100 * mean(disease_data$D2.2_0, na.rm = TRUE),
      Mean_ACT = mean(disease_data$ACT.SCORE_0, na.rm = TRUE),
      Mean_CCQ = mean(disease_data$CCQ.SCORE_0, na.rm = TRUE),
      Mean_EQindex = mean(disease_data$EQindex_0, na.rm = TRUE)
    )
  }
}

summary_by_disease <- do.call(rbind, summary_rows)

write.csv(summary_by_disease, "outputs/summary_by_disease.csv", row.names = FALSE)
cat("Saved: outputs/summary_by_disease.csv\n")

###########################################################################
# SECTION 6: Print summaries to console
###########################################################################

cat("\n=== TABLE 1 PREVIEW (ITT, first 8 rows) ===\n")
print(head(table1_itt, 8))

cat("\n=== MISSINGNESS PREVIEW ===\n")
print(missingness_summary)

cat("\n=== SUMMARY BY DISEASE ===\n")
print(summary_by_disease)

cat("\n✓ R/03_descriptives.R completed successfully\n")
