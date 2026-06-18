# R/10_imputation_matrix_sensitivity.R
# Run pre-specified primary-effectiveness MICE predictor-matrix sensitivity tests.
#
# This script does not alter the canonical primary imputation artifact. It reruns
# MICE/GEE for matrix variants proposed by R/09_imputation_predictor_matrix_audit.R
# and writes a compact 12-month GEE comparison table.

source("R/02_imputation_helpers.R")
source("R/04_effectiveness_helpers.R")

library(dplyr)
library(mice)

phase_started <- pipeline_phase_start(
  "10_imputation_matrix_sensitivity",
  "running primary-effectiveness predictor-matrix sensitivity tests"
)

ensure_artifact_dirs()

sanitize_quickpred_matrix <- function(df, quickpred_matrix, branch) {
  profile <- build_imputation_variable_profile(df)
  roles <- setNames(profile$role, profile$variable)
  times <- setNames(profile$timepoint, profile$variable)
  out <- quickpred_matrix
  out[,] <- as.integer(out != 0)

  for (target in rownames(out)) {
    for (predictor in colnames(out)) {
      if (target == predictor ||
          target %in% c("patient", "group") ||
          predictor %in% c("patient", "group") ||
          times[[predictor]] > times[[target]] ||
          !predictor_role_allowed_for_target(roles[[target]], roles[[predictor]], branch)) {
        out[target, predictor] <- 0
      }
    }
  }

  diag(out) <- 0
  out
}

build_effectiveness_variant_matrices <- function(df, analytic_matrix, quickpred_matrix) {
  profile <- build_imputation_variable_profile(df)
  roles <- setNames(profile$role, profile$variable)
  times <- setNames(profile$timepoint, profile$variable)

  no_utility_index <- analytic_matrix
  utility_predictors <- names(roles)[roles == "utility_index"]
  no_utility_index[, intersect(utility_predictors, colnames(no_utility_index))] <- 0

  no_same_visit_aux <- analytic_matrix
  for (target in rownames(no_same_visit_aux)) {
    for (predictor in colnames(no_same_visit_aux)) {
      same_visit_aux <- times[[predictor]] == times[[target]] &&
        roles[[predictor]] != "baseline_covariate"
      if (same_visit_aux) {
        no_same_visit_aux[target, predictor] <- 0
      }
    }
  }

  history_only <- analytic_matrix
  for (target in rownames(history_only)) {
    for (predictor in colnames(history_only)) {
      keep <- roles[[predictor]] == "baseline_covariate" ||
        (roles[[predictor]] == "effectiveness_outcome" && times[[predictor]] < times[[target]]) ||
        predictor %in% c("controlled_0", method_config("effectiveness", "adjusted_covariates"))
      if (!keep) {
        history_only[target, predictor] <- 0
      }
    }
  }

  list(
    current_analytic = analytic_matrix,
    no_utility_index_auxiliaries = no_utility_index,
    no_same_visit_auxiliaries = no_same_visit_aux,
    history_only_controlled = history_only,
    sanitized_quickpred = sanitize_quickpred_matrix(df, quickpred_matrix, "effectiveness")
  )
}

summarise_matrix_links <- function(matrix) {
  data.frame(
    n_links = sum(matrix != 0),
    min_predictors_per_target = min(rowSums(matrix != 0)[rowSums(matrix != 0) > 0]),
    max_predictors_per_target = max(rowSums(matrix != 0)),
    stringsAsFactors = FALSE
  )
}

extract_adjusted_12mo <- function(gee_result, variant, matrix) {
  row <- gee_result$gee_timepoint_effects %>%
    filter(adjustment == "adjusted", time == 12) %>%
    slice(1)
  link_summary <- summarise_matrix_links(matrix)
  bind_cols(
    data.frame(
      variant = variant,
      stringsAsFactors = FALSE
    ),
    link_summary,
    row %>%
      transmute(
        model_family,
        adjustment,
        time,
        odds_ratio,
        ci_low,
        ci_high,
        p_value,
        n_imputations
      )
  )
}

imputation_artifact <- read_canonical_artifact("imputation")
effectiveness_df <- imputation_artifact$effectiveness_df_impute
effectiveness_methods <- imputation_artifact$effectiveness_methods
effectiveness_matrix <- imputation_artifact$effectiveness_predictor_matrix
effectiveness_quickpred <- imputation_artifact$effectiveness_quickpred_matrix

variant_matrices <- build_effectiveness_variant_matrices(
  effectiveness_df,
  effectiveness_matrix,
  effectiveness_quickpred
)

results <- list()
comparison_rows <- list()

for (variant in names(variant_matrices)) {
  pipeline_phase_info(
    "10_imputation_matrix_sensitivity",
    sprintf("testing matrix variant '%s'", variant)
  )

  matrix <- variant_matrices[[variant]]

  if (variant == "current_analytic") {
    mids_obj <- imputation_artifact$effectiveness_mids
  } else {
    seed <- method_config("imputation", "full_seed") +
      match(variant, names(variant_matrices)) * 1000L
    mids_obj <- run_arm_split_mice(
      df_impute = effectiveness_df,
      predictor_matrix = matrix,
      methods = effectiveness_methods,
      seed = seed,
      output_prefix = paste0("mids_matrix_sensitivity_", variant),
      write_first_completion = FALSE
    )$mids
  }

  gee_result <- run_gee_effectiveness_analysis(
    imputation_variant = "full",
    write_outputs = FALSE,
    imputation_override = mids_obj
  )

  results[[variant]] <- list(
    predictor_matrix = matrix,
    mids = mids_obj,
    gee = gee_result
  )
  comparison_rows[[variant]] <- extract_adjusted_12mo(gee_result, variant, matrix)
}

comparison <- bind_rows(comparison_rows) %>%
  arrange(match(variant, names(variant_matrices)))

artifact <- list(
  stage = "10_imputation_matrix_sensitivity",
  comparison = comparison,
  results = results,
  matrix_variants = names(variant_matrices)
)

saveRDS(artifact, audit_path("imputation_matrix_sensitivity_artifact.rds"))
write.csv(comparison, audit_path("imputation_matrix_sensitivity_gee_12mo.csv"), row.names = FALSE)

print(comparison)

message("10_imputation_matrix_sensitivity: saved matrix-sensitivity GEE comparison.")
pipeline_phase_end(
  "10_imputation_matrix_sensitivity",
  phase_started,
  "saved matrix-sensitivity GEE comparison"
)
