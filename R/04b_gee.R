# R/04b_gee.R
# Purpose: Fit the protocol-style GEE effectiveness model on imputed data.
# This script is intentionally separate from R/04_models.R so the GEE branch
# can be tuned and benchmarked independently from the mixed-effects sensitivity
# analysis.
#
# Inputs:
#   - data_processed/mids_imputation.rds
# Outputs:
#   - outputs/model_gee_summaries.csv
#   - outputs/model_gee_timepoint_effects.csv
#   - outputs/models_gee_imputed.rds

source("R/utils.R")

library(dplyr)
library(mice)
library(geepack)

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

imputation_path <- "data_processed/mids_imputation.rds"
if (!file.exists(imputation_path)) {
  stop("Missing ", imputation_path, ". Run R/02_imputation.R first.")
}

imputation <- readRDS(imputation_path)
imputed_sets <- mice::complete(imputation, action = "all", include = FALSE)

make_gee_long_data <- function(frame) {
  frame <- as.data.frame(frame)

  pts <- unique(frame$patient)
  long_data <- data.frame(
    patient = rep(pts, each = length(TIMEPOINTS)),
    time = rep(TIMEPOINTS, length(pts))
  )

  long_data <- merge(long_data, frame, by = "patient", all.x = TRUE, no.dups = FALSE)

  long_data$controlled_t <- dplyr::case_when(
    long_data$time == 0 ~ as_numeric_safe(long_data$controlled_0),
    long_data$time == 3 ~ as_numeric_safe(long_data$controlled_3),
    long_data$time == 6 ~ as_numeric_safe(long_data$controlled_6),
    long_data$time == 9 ~ as_numeric_safe(long_data$controlled_9),
    long_data$time == 12 ~ as_numeric_safe(long_data$controlled_12)
  )

  long_data$group <- factor(long_data$group, levels = GROUP_LEVELS)
  long_data$time <- factor(long_data$time, levels = TIMEPOINTS)
  long_data$patient <- factor(long_data$patient)
  long_data$gender <- factor(long_data$gender)
  long_data$age <- factor(long_data$age)
  long_data$controlled_0 <- factor(long_data$controlled_0, levels = c(0, 1))

  long_data
}

fit_gee_model <- function(frame, imputation_index) {
  long_data <- make_gee_long_data(frame)

  message("04b_gee: fitting imputation ", imputation_index, "/", length(imputed_sets))

  geeglm(
    formula = controlled_t ~ controlled_0 + age + gender + group * time,
    family = binomial(link = "logit"),
    id = patient,
    data = long_data,
    corstr = "exchangeable",
    std.err = "san.se"
  )
}

fit_gee_list <- lapply(seq_along(imputed_sets), function(i) {
  tryCatch(
    fit_gee_model(imputed_sets[[i]], i),
    error = function(e) {
      stop("GEE model failed for imputation ", i, ": ", conditionMessage(e), call. = FALSE)
    }
  )
})

pool_gee_models <- function(gee_list) {
  coefs_list <- lapply(gee_list, coef)
  vars_list <- lapply(gee_list, vcov)
  qmat <- do.call(rbind, coefs_list)
  qbar <- colMeans(qmat)
  ubar <- Reduce("+", vars_list) / length(vars_list)
  bmat <- stats::cov(qmat)
  total_var <- ubar + (1 + 1 / nrow(qmat)) * bmat
  se <- sqrt(diag(total_var))
  z <- qbar / se
  p_values <- 2 * (1 - pnorm(abs(z)))

  data.frame(
    term = names(qbar),
    estimate = unname(qbar),
    std.error = unname(se),
    statistic = unname(z),
    p.value = unname(p_values),
    odds_ratio = exp(unname(qbar)),
    ci_low = exp(unname(qbar) - 1.96 * unname(se)),
    ci_high = exp(unname(qbar) + 1.96 * unname(se)),
    row.names = NULL
  )
}

extract_gee_timepoint_contrast <- function(fit, time_value) {
  coef_names <- names(coef(fit))
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
      return(c(log_or = NA_real_, var = NA_real_))
    }
    terms <- c(group_term, interaction_term)
  }

  beta <- sum(coef(fit)[terms], na.rm = TRUE)
  variance <- sum(vcov(fit)[terms, terms, drop = FALSE], na.rm = TRUE)
  c(log_or = beta, var = variance)
}

pool_gee_timepoints <- function(gee_list) {
  lapply(TIMEPOINTS, function(tp) {
    contrasts <- t(vapply(gee_list, extract_gee_timepoint_contrast, numeric(2), time_value = tp))
    contrasts <- contrasts[!is.na(contrasts[, "log_or"]), , drop = FALSE]

    if (nrow(contrasts) == 0) {
      return(NULL)
    }

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
  }) |>
    bind_rows()
}

gee_pooled_summary <- pool_gee_models(fit_gee_list)
gee_pooled_summary$model <- "GEE_exchangeable_imputed"

gee_contrast_table <- pool_gee_timepoints(fit_gee_list)

write.csv(gee_pooled_summary, "outputs/model_gee_summaries.csv", row.names = FALSE)
write.csv(gee_contrast_table, "outputs/model_gee_timepoint_effects.csv", row.names = FALSE)

saveRDS(
  list(
    imputation = imputation,
    gee_fits = fit_gee_list,
    gee_pooled_summary = gee_pooled_summary,
    gee_timepoint_effects = gee_contrast_table,
    gee_manuscript_style_12mo = subset(gee_contrast_table, time == 12)
  ),
  file = "outputs/models_gee_imputed.rds"
)

cat("04b_gee: GEE models fit on imputed data and saved summaries.\n")
