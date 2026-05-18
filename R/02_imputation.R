# R/02_imputation.R
# Legacy-aligned multiple imputation pipeline.
#
# This follows the original multiple_imputation.R structure closely:
# - build a wide imputation frame with the same analysis variables
# - split by treatment arm and disease
# - impute each subset with mice::mice(method = "pmm")
# - combine the four mids objects with mice::rbind()

source("R/utils.R")

library(dplyr)
library(tidyr)
library(mice)
library(labelled)
library(haven)

proc_path <- "data_processed/all_cases.rds"
out_dir <- "data_processed"
clean_out_dir <- "clean_data"
dir.create(out_dir, showWarnings = FALSE)
dir.create(clean_out_dir, showWarnings = FALSE)

if (!file.exists(proc_path)) stop(proc_path, " not found. Run R/01_cleaning.R first.")

df <- readRDS(proc_path)

# Legacy script expected baseline disease/group columns to remain distinct
# after the wide merge.
df$D1.3_0 <- as_numeric_safe(df[["D1.3_0"]])
df$D1.4_0 <- as.character(df[["D1.4_0"]])

df_impute <- data.frame(df)
df_impute <- df_impute[, colSums(is.na(df_impute)) < nrow(df_impute)]

df_impute <- df_impute %>%
  rename(
    c(
      patient = D1.2,
      condition = D1.3_0,
      group = D1.4_0,

      location = D2.1_0,
      gender = D2.2_0,
      age = D2.3_0,
      ethnicity = D2.4_0,
      education = D2.5_0,
      selection = D2.6_0,
      live_alone = D2.7_0,
      FVC_0 = D2.8_0,
      FEV1_0 = D2.9_0,
      height = D3.1_0,
      weight = D3.2_0,
      BMI = D3.3_0,
      BMI_range = D3.4_0,
      smoking = D3.6_0,
      diabetes = D3.7_1_0,
      ihd = D3.7_2_0,
      employed = D3.12_0,
      num_meds = D5.2_0,

      gp_0 = D3.10_1_0, nurse_0 = D3.10_2_0,
      therapist_0 = D3.10_3_0, ae_0 = D3.10_4_0,
      outpatient_0 = D3.10_5_0, inpatient_0 = D3.10_6_0,
      inpatient_days_0 = D3.10_7_0, sw_0 = D3.11_1_0,
      daycare_0 = D3.11_2_0,

      gp_6 = D3.10_1_6, nurse_6 = D3.10_2_6,
      therapist_6 = D3.10_3_6, ae_6 = D3.10_4_6,
      outpatient_6 = D3.10_5_6, inpatient_6 = D3.10_6_6,
      inpatient_days_6 = D3.10_7_6, sw_6 = D3.11_1_6,
      daycare_6 = D3.11_2_6,

      gp_12 = D3.10_1_12, nurse_12 = D3.10_2_12,
      therapist_12 = D3.10_3_12, ae_12 = D3.10_4_12,
      outpatient_12 = D3.10_5_12, inpatient_12 = D3.10_6_12,
      inpatient_days_12 = D3.10_7_12, sw_12 = D3.11_1_12,
      daycare_12 = D3.11_2_12,

      med_adherence_0 = D5.9_0, last_missed_dose_0 = D5.10_0,
      med_adherence_3 = D5.9_3, last_missed_dose_3 = D5.10_3,
      med_adherence_6 = D5.9_6, last_missed_dose_6 = D5.10_6,
      med_adherence_9 = D5.9_9, last_missed_dose_9 = D5.10_9,
      med_adherence_12 = D5.9_12, last_missed_dose_12 = D5.10_12
    )
  ) %>%
  select(
    c(
      patient, condition, group, location, gender, ethnicity, age,
      education, selection, live_alone, FVC_0, FEV1_0, height,
      weight, BMI, BMI_range, smoking, diabetes, ihd, employed, num_meds,
      controlled_0, controlled_3, controlled_6, controlled_9, controlled_12,
      EQindex_0, EQindex_3, EQindex_6, EQindex_9, EQindex_12,
      gp_0, nurse_0, therapist_0, ae_0, outpatient_0, inpatient_0, inpatient_days_0, sw_0, daycare_0,
      gp_6, nurse_6, therapist_6, ae_6, outpatient_6, inpatient_6, inpatient_days_6, sw_6, daycare_6,
      gp_12, nurse_12, therapist_12, ae_12, outpatient_12, inpatient_12, inpatient_days_12, sw_12, daycare_12,
      med_adherence_0, last_missed_dose_0,
      med_adherence_3, last_missed_dose_3,
      med_adherence_6, last_missed_dose_6,
      med_adherence_9, last_missed_dose_9,
      med_adherence_12, last_missed_dose_12,
      cost_M6, cost_M12,
      cost_C6, cost_C12,
      cost_H6, cost_H12,
      cost_F6, cost_F12,
      cost_O6, cost_O12
    )
  )

df_impute$age <- as.factor(df_impute$age)
df_impute$education <- as.factor(df_impute$education)
df_impute$selection <- as.factor(df_impute$selection)
df_impute$live_alone <- as.factor(df_impute$live_alone)
df_impute$gender <- as.factor(df_impute$gender)
df_impute$ethnicity <- as.factor(df_impute$ethnicity)
df_impute$BMI_range <- as.factor(df_impute$BMI_range)
df_impute$smoking <- as.factor(df_impute$smoking)
df_impute$diabetes <- as.factor(df_impute$diabetes)
df_impute$ihd <- as.factor(df_impute$ihd)
df_impute$employed <- as.factor(df_impute$employed)
df_impute$controlled_0 <- as.factor(df_impute$controlled_0)
df_impute$controlled_3 <- as.factor(df_impute$controlled_3)
df_impute$controlled_6 <- as.factor(df_impute$controlled_6)
df_impute$controlled_9 <- as.factor(df_impute$controlled_9)
df_impute$controlled_12 <- as.factor(df_impute$controlled_12)

cost_cols <- c("cost_M6", "cost_M12", "cost_C6", "cost_C12", "cost_H6", "cost_H12", "cost_F6", "cost_F12", "cost_O6", "cost_O12")
df_impute[, cost_cols][is.na(df_impute[, cost_cols])] <- 0

pred_matrix <- matrix(0, nrow = ncol(df_impute), ncol = ncol(df_impute))
colnames(pred_matrix) <- names(df_impute)
rownames(pred_matrix) <- names(df_impute)

pred_matrix[, grep("^gender$", colnames(df_impute))] <- 1
pred_matrix[, grep("^age$", colnames(df_impute))] <- 1
pred_matrix[, grep("^controlled_0$", colnames(df_impute))] <- 1
pred_matrix[, grep("^EQindex_0$", colnames(df_impute))] <- 1
pred_matrix[grep("^controlled_6$", colnames(df_impute)), grep("^controlled_3$", colnames(df_impute))] <- 1
pred_matrix[grep("^controlled_9$", colnames(df_impute)), grep("^controlled_6$", colnames(df_impute))] <- 1
pred_matrix[grep("^controlled_12$", colnames(df_impute)), grep("^controlled_9$", colnames(df_impute))] <- 1
diag(pred_matrix) <- 0

df_impute_UC <- df_impute %>% filter(group == "cg (control group)")
df_impute_BOFE <- df_impute %>% filter(group == "ig (intervention group)")

df_impute_UC_asthma <- df_impute_UC %>% filter(condition == 1)
df_impute_UC_COPD <- df_impute_UC %>% filter(condition == 2)
df_impute_BOFE_asthma <- df_impute_BOFE %>% filter(condition == 1)
df_impute_BOFE_COPD <- df_impute_BOFE %>% filter(condition == 2)

df_impute_UC_asthma <- remove_val_labels(df_impute_UC_asthma)
df_impute_UC_COPD <- remove_val_labels(df_impute_UC_COPD)
df_impute_BOFE_asthma <- remove_val_labels(df_impute_BOFE_asthma)
df_impute_BOFE_COPD <- remove_val_labels(df_impute_BOFE_COPD)

set.seed(123)
df_impute_UC_asthma <- mice(df_impute_UC_asthma, m = IMPUTATION_REPLICATES, method = "pmm", predictorMatrix = pred_matrix, seed = 123, printFlag = FALSE)
df_impute_UC_COPD <- mice(df_impute_UC_COPD, m = IMPUTATION_REPLICATES, method = "pmm", predictorMatrix = pred_matrix, seed = 123, printFlag = FALSE)
df_impute_BOFE_asthma <- mice(df_impute_BOFE_asthma, m = IMPUTATION_REPLICATES, method = "pmm", predictorMatrix = pred_matrix, seed = 123, printFlag = FALSE)
df_impute_BOFE_COPD <- mice(df_impute_BOFE_COPD, m = IMPUTATION_REPLICATES, method = "pmm", predictorMatrix = pred_matrix, seed = 123, printFlag = FALSE)

mids_UC <- mice::rbind(df_impute_UC_asthma, df_impute_UC_COPD)
mids_BOFE <- mice::rbind(df_impute_BOFE_asthma, df_impute_BOFE_COPD)
mids_total <- mice::rbind(mids_UC, mids_BOFE)

saveRDS(mids_total, file = file.path(out_dir, "mids_imputation.rds"))

diag_path <- file.path(out_dir, "imputation_diagnostics.txt")
cat(capture.output(summary(mids_total), digits = 4), file = diag_path, sep = "\n")

if (file.exists(file.path(out_dir, "mids_imputation.rds"))) {
  completed1 <- mice::complete(mids_total, 1)
  write.csv(completed1, file.path(out_dir, "imputed_dataset_1.csv"), row.names = FALSE)
}

miss_report <- df_impute %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")
write.csv(miss_report, file.path(out_dir, "imputation_missingness_summary.csv"), row.names = FALSE)

message("02_imputation: created mids_imputation.rds and supporting diagnostics")
