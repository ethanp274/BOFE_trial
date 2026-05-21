###########################################################################
# R/04_models.R
# Purpose: Fit the manuscript-aligned mixed-effects model on imputed data.
# The model uses only the variables needed for the legacy primary analysis:
# patient ID, treatment group, age, sex, baseline control, and time-varying
# control status.
# Inputs:
#   - data_processed/mids_imputation.rds
# Outputs:
#   - outputs/model_summaries.csv
#   - outputs/model_timepoint_effects.csv
#   - outputs/models_mixed_imputed.rds
###########################################################################

source("R/utils.R")

library(dplyr)
library(mice)
library(lme4)
library(broom.mixed)

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

pipeline_started <- pipeline_phase_start(
  "04_models",
  "fitting the pooled mixed-effects effectiveness model"
)

imputation_path <- "data_processed/mids_imputation.rds"
if (!file.exists(imputation_path)) {
  stop("Missing ", imputation_path, ". Run R/02_imputation.R first.")
}

imputation <- readRDS(imputation_path)
pipeline_phase_info("04_models", sprintf("reconstructing %d imputed long datasets", imputation$m))
make_long_data <- function(frame) {
  frame <- as.data.frame(frame)

  pts <- unique(frame$patient)
  rep_pts <- rep(pts, each = 5)
  time <- rep(seq(0, 12, 3), length(pts))
  long_data <- data.frame(patient = rep_pts, time = time)

  long_data <- merge(long_data, frame, by.x = "patient", by.y = "patient", all.x = TRUE, no.dups = FALSE)

  long_data <- long_data %>%
    mutate(controlled_t = case_when(
      time == 0 ~ controlled_0,
      time == 3 ~ controlled_3,
      time == 6 ~ controlled_6,
      time == 9 ~ controlled_9,
      time == 12 ~ controlled_12
    )) %>%
    select(-c(controlled_3, controlled_6, controlled_9, controlled_12))

  long_data <- long_data %>%
    mutate(EQindex_t = case_when(
      time == 0 ~ EQindex_0,
      time == 3 ~ EQindex_3,
      time == 6 ~ EQindex_6,
      time == 9 ~ EQindex_9,
      time == 12 ~ EQindex_12
    )) %>%
    select(-c(EQindex_3, EQindex_6, EQindex_9, EQindex_12))

  long_data <- long_data %>%
    mutate(med_adherence = case_when(
      time == 0 ~ med_adherence_0,
      time == 3 ~ med_adherence_3,
      time == 6 ~ med_adherence_6,
      time == 9 ~ med_adherence_9,
      time == 12 ~ med_adherence_12
    )) %>%
    select(-c(med_adherence_0, med_adherence_3, med_adherence_6, med_adherence_9, med_adherence_12))

  long_data <- long_data %>%
    mutate(
      gp = case_when(time == 0 ~ gp_0, time == 3 ~ 0, time == 6 ~ gp_6, time == 9 ~ 0, time == 12 ~ gp_12),
      nurse = case_when(time == 0 ~ nurse_0, time == 3 ~ 0, time == 6 ~ nurse_6, time == 9 ~ 0, time == 12 ~ nurse_12),
      therapist = case_when(time == 0 ~ therapist_0, time == 3 ~ 0, time == 6 ~ therapist_6, time == 9 ~ 0, time == 12 ~ therapist_12),
      ae = case_when(time == 0 ~ ae_0, time == 3 ~ 0, time == 6 ~ ae_6, time == 9 ~ 0, time == 12 ~ ae_12),
      outpatient = case_when(time == 0 ~ outpatient_0, time == 3 ~ 0, time == 6 ~ outpatient_6, time == 9 ~ 0, time == 12 ~ outpatient_12),
      inpatient = case_when(time == 0 ~ inpatient_0, time == 3 ~ 0, time == 6 ~ inpatient_6, time == 9 ~ 0, time == 12 ~ inpatient_12),
      inpatient_days = case_when(time == 0 ~ inpatient_days_0, time == 3 ~ 0, time == 6 ~ inpatient_days_6, time == 9 ~ 0, time == 12 ~ inpatient_days_12),
      sw = case_when(time == 0 ~ sw_0, time == 3 ~ 0, time == 6 ~ sw_6, time == 9 ~ 0, time == 12 ~ sw_12),
      daycare = case_when(time == 0 ~ daycare_0, time == 3 ~ 0, time == 6 ~ daycare_6, time == 9 ~ 0, time == 12 ~ daycare_12)
    ) %>%
    select(-c(
      gp_0, gp_6, gp_12,
      nurse_0, nurse_6, nurse_12,
      therapist_0, therapist_6, therapist_12,
      ae_0, ae_6, ae_12,
      outpatient_0, outpatient_6, outpatient_12,
      inpatient_0, inpatient_6, inpatient_12,
      inpatient_days_0, inpatient_days_6, inpatient_days_12,
      sw_0, sw_6, sw_12,
      daycare_0, daycare_6, daycare_12
    ))

  long_data <- long_data %>%
    mutate(
      cost_M = case_when(time == 0 ~ 0, time == 3 ~ 0, time == 6 ~ cost_M6, time == 9 ~ 0, time == 12 ~ cost_M12),
      cost_C = case_when(time == 0 ~ 0, time == 3 ~ 0, time == 6 ~ cost_C6, time == 9 ~ 0, time == 12 ~ cost_C12),
      cost_O = case_when(time == 0 ~ 0, time == 3 ~ 0, time == 6 ~ cost_O6, time == 9 ~ 0, time == 12 ~ cost_O12),
      cost_H = case_when(time == 0 ~ 0, time == 3 ~ 0, time == 6 ~ cost_H6, time == 9 ~ 0, time == 12 ~ cost_H12),
      cost_F = case_when(time == 0 ~ 0, time == 3 ~ 0, time == 6 ~ cost_F6, time == 9 ~ 0, time == 12 ~ cost_F12)
    ) %>%
    select(-c(cost_C6, cost_C12, cost_M6, cost_M12, cost_O6, cost_O12, cost_H6, cost_H12, cost_F6, cost_F12))

  long_data[, c("cost_M", "cost_C", "cost_H", "cost_F", "cost_O")][is.na(long_data[, c("cost_M", "cost_C", "cost_H", "cost_F", "cost_O")])] <- 0
  long_data$time <- as.factor(long_data$time)
  long_data
}

imputed_long_source <- mice::complete(imputation, action = "long", include = TRUE)

  long_sets <- lapply(seq_len(imputation$m), function(i) {
  frame <- imputed_long_source %>%
    filter(.imp == i) %>%
    select(-c(.imp, .id))
  long_data <- make_long_data(frame)
  long_data$time <- as.factor(long_data$time)
  levels(long_data$time) <- c("0mo", "3mo", "6mo", "9mo", "12mo")
  long_data$time <- as.character(long_data$time)
  long_data$group <- factor(long_data$group, levels = GROUP_LEVELS)
  long_data$age <- as.factor(long_data$age)
  long_data[".imp"] <- i
  long_data
})

mids_data_og <- imputed_long_source %>%
  filter(.imp == 0) %>%
  select(-c(.imp, .id)) %>%
  make_long_data()
mids_data_og$time <- as.factor(mids_data_og$time)
levels(mids_data_og$time) <- c("0mo", "3mo", "6mo", "9mo", "12mo")
mids_data_og$time <- as.character(mids_data_og$time)
mids_data_og$group <- factor(mids_data_og$group, levels = GROUP_LEVELS)
mids_data_og$age <- as.factor(mids_data_og$age)
mids_data_og[".imp"] <- 0

mids_data_long <- Reduce(mice::rbind, c(list(mids_data_og), long_sets))
mids_data_long <- as.mids(mids_data_long)

pipeline_phase_info("04_models", "fitting and pooling mixed-effects models")

mira_glmm <- with(
  mids_data_long,
  glmer(
    formula = controlled_t ~ controlled_0 + age + gender + group * time + (1 | patient),
    family = "binomial",
    control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B"))
  )
)

mira_glmm <- as.mira(mira_glmm)
fit_list <- mira_glmm$analyses
pooled_fit <- pool(mira_glmm)
pooled_summary <- summary(pooled_fit)

pooled_summary$term <- pooled_summary$term
pooled_summary$odds_ratio <- exp(pooled_summary$estimate)
pooled_summary$ci_low <- exp(pooled_summary$estimate - qt(0.975, pooled_summary$df) * pooled_summary$std.error)
pooled_summary$ci_high <- exp(pooled_summary$estimate + qt(0.975, pooled_summary$df) * pooled_summary$std.error)
pooled_summary$model <- "Pooled_mixed_effects_imputed"

extract_timepoint_contrast <- function(fit, time_value) {
  coef_names <- names(fixef(fit))
  group_term <- grep("^group", coef_names, value = TRUE)[1]
  if (is.na(group_term) || !nzchar(group_term)) {
    stop("Could not identify the treatment-group coefficient.")
  }

  if (time_value == 0) {
    terms <- c(group_term)
  } else {
    candidate_terms <- c(
      paste0(group_term, ":time", time_value),
      paste0(group_term, ":time", time_value, "mo"),
      paste0(group_term, ":time", time_value, "mo)")
    )
    interaction_term <- candidate_terms[candidate_terms %in% coef_names][1]
    if (is.na(interaction_term) || !nzchar(interaction_term)) {
      stop("Missing interaction term for time ", time_value)
    }
    terms <- c(group_term, interaction_term)
  }

  beta <- sum(fixef(fit)[terms])
  variance <- sum(vcov(fit)[terms, terms, drop = FALSE])
  c(log_or = beta, var = variance)
}

contrast_table <- lapply(TIMEPOINTS, function(tp) {
  contrasts <- t(vapply(fit_list, extract_timepoint_contrast, numeric(2), time_value = tp))
  qbar <- mean(contrasts[, "log_or"])
  ubar <- mean(contrasts[, "var"])
  b <- if (nrow(contrasts) > 1) stats::var(contrasts[, "log_or"]) else 0
  total_var <- ubar + (1 + 1 / nrow(contrasts)) * b
  se <- sqrt(total_var)
  data.frame(
    time = tp,
    log_or = qbar,
    odds_ratio = exp(qbar),
    ci_low = exp(qbar - 1.96 * se),
    ci_high = exp(qbar + 1.96 * se),
    n_imputations = nrow(contrasts),
    stringsAsFactors = FALSE
  )
})
contrast_table <- bind_rows(contrast_table)

write.csv(pooled_summary, "outputs/model_summaries.csv", row.names = FALSE)
write.csv(contrast_table, "outputs/model_timepoint_effects.csv", row.names = FALSE)

saveRDS(
  list(
    imputation = imputation,
    long_reconstruction = mids_data_long,
    fits = fit_list,
    pooled_fit = pooled_fit,
    pooled_summary = pooled_summary,
    timepoint_effects = contrast_table,
    manuscript_style_12mo = subset(contrast_table, time == 12)
  ),
  file = "outputs/models_mixed_imputed.rds"
)

cat("04_models: pooled mixed-effects model fit on imputed data and saved summaries.\n")
pipeline_phase_end(
  "04_models",
  pipeline_started,
  "saved mixed-effects model summaries"
)
