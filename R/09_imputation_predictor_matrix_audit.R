# R/09_imputation_predictor_matrix_audit.R
# Diagnostic audit of MICE predictor-matrix choices.
#
# This script does not change the primary imputation model. It creates a
# reproducible audit trail for whether predictor links are clinically/rule
# justified, time-safe, empirically supported, and worth testing in sensitivity
# analyses.

source("R/02_imputation_helpers.R")

library(dplyr)
library(tidyr)

phase_started <- pipeline_phase_start(
  "09_imputation_predictor_matrix_audit",
  "auditing MICE predictor-matrix choices"
)

ensure_artifact_dirs()

numeric_for_audit <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(x))
  }
  if (is.character(x)) {
    return(as.numeric(factor(x)))
  }
  as_numeric_safe(x)
}

safe_abs_cor <- function(x, y) {
  x <- numeric_for_audit(x)
  y <- numeric_for_audit(y)
  keep <- stats::complete.cases(x, y)
  if (sum(keep) < 10L) {
    return(NA_real_)
  }
  x <- x[keep]
  y <- y[keep]
  if (length(unique(x)) < 2L || length(unique(y)) < 2L) {
    return(NA_real_)
  }
  abs(suppressWarnings(stats::cor(x, y)))
}

pair_usable_fraction <- function(x, y) {
  mean(!is.na(x) & !is.na(y))
}

build_pair_score_table <- function(df, analytic_matrix, quickpred_matrix, branch, methods = NULL) {
  profile <- build_imputation_variable_profile(df)
  role_lookup <- setNames(profile$role, profile$variable)
  time_lookup <- setNames(profile$timepoint, profile$variable)
  missing_lookup <- setNames(profile$missing_fraction, profile$variable)
  observed_lookup <- setNames(profile$n_observed, profile$variable)

  targets <- rownames(analytic_matrix)
  predictors <- colnames(analytic_matrix)
  target_predictor_grid <- expand.grid(
    target = targets,
    predictor = predictors,
    stringsAsFactors = FALSE
  ) %>%
    filter(
      target != predictor,
      !target %in% c("patient", "group"),
      !predictor %in% c("patient", "group")
    )

  if (!is.null(methods)) {
    imputed_targets <- names(methods)[methods != ""]
    target_predictor_grid <- target_predictor_grid %>%
      filter(target %in% imputed_targets)
  }

  target_predictor_grid %>%
    rowwise() %>%
    mutate(
      branch = branch,
      target_role = unname(role_lookup[target]),
      predictor_role = unname(role_lookup[predictor]),
      target_timepoint = unname(time_lookup[target]),
      predictor_timepoint = unname(time_lookup[predictor]),
      predictor_missing_fraction = unname(missing_lookup[predictor]),
      predictor_n_observed = unname(observed_lookup[predictor]),
      analytic_selected = analytic_matrix[target, predictor] != 0,
      quickpred_selected = quickpred_matrix[target, predictor] != 0,
      uses_future_timepoint = predictor_timepoint > target_timepoint,
      role_allowed = predictor_role_allowed_for_target(target_role, predictor_role, branch),
      usable_fraction = pair_usable_fraction(df[[target]], df[[predictor]]),
      abs_value_association = safe_abs_cor(df[[target]], df[[predictor]]),
      abs_missingness_association = safe_abs_cor(is.na(df[[target]]), df[[predictor]]),
      max_abs_association = max(
        c(abs_value_association, abs_missingness_association),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    mutate(
      max_abs_association = ifelse(is.infinite(max_abs_association), NA_real_, max_abs_association),
      protected_primary_anchor = target_role == "effectiveness_outcome" &
        predictor %in% c("controlled_0", method_config("effectiveness", "adjusted_covariates")),
      evidence_band = case_when(
        is.na(max_abs_association) ~ "not_estimable",
        max_abs_association >= 0.20 ~ "strong",
        max_abs_association >= 0.10 ~ "moderate",
        max_abs_association >= 0.05 ~ "weak",
        TRUE ~ "minimal"
      ),
      audit_recommendation = case_when(
        analytic_selected & uses_future_timepoint ~ "remove_future_leak",
        quickpred_selected & uses_future_timepoint ~ "reject_quickpred_future",
        analytic_selected & !role_allowed ~ "remove_role_disallowed",
        quickpred_selected & !role_allowed ~ "reject_role_disallowed",
        analytic_selected & protected_primary_anchor ~ "retain_primary_anchor",
        analytic_selected & evidence_band %in% c("minimal", "not_estimable") ~
          "candidate_removal_sensitivity_low_empirical_support",
        !analytic_selected & quickpred_selected & role_allowed & !uses_future_timepoint &
          usable_fraction >= method_config("imputation", "predictor_selection", "quickpred_min_usable_cases") ~
          "candidate_add_sensitivity_quickpred_supported",
        analytic_selected ~ "retain_current",
        TRUE ~ "not_selected"
      )
    ) %>%
    arrange(target_timepoint, target, desc(analytic_selected), desc(quickpred_selected), predictor)
}

summarise_matrix_variant <- function(df, matrix, branch, variant) {
  profile <- build_imputation_variable_profile(df)
  role_lookup <- setNames(profile$role, profile$variable)
  time_lookup <- setNames(profile$timepoint, profile$variable)
  pair_idx <- which(matrix != 0, arr.ind = TRUE)

  if (nrow(pair_idx) == 0L) {
    return(data.frame(
      branch = branch,
      variant = variant,
      n_links = 0L,
      n_future_links = 0L,
      mean_predictors_per_target = 0,
      min_predictors_per_target = 0L,
      max_predictors_per_target = 0L,
      role_pairs = "",
      stringsAsFactors = FALSE
    ))
  }

  targets <- rownames(matrix)[pair_idx[, "row"]]
  predictors <- colnames(matrix)[pair_idx[, "col"]]
  predictors_by_target <- rowSums(matrix != 0)
  role_pairs <- paste0(unname(role_lookup[predictors]), "->", unname(role_lookup[targets]))

  data.frame(
    branch = branch,
    variant = variant,
    n_links = nrow(pair_idx),
    n_future_links = sum(unname(time_lookup[predictors]) > unname(time_lookup[targets]), na.rm = TRUE),
    mean_predictors_per_target = mean(predictors_by_target[predictors_by_target > 0]),
    min_predictors_per_target = min(predictors_by_target[predictors_by_target > 0]),
    max_predictors_per_target = max(predictors_by_target),
    role_pairs = paste(names(sort(table(role_pairs), decreasing = TRUE)), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

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

write_audit_protocol <- function(path) {
  lines <- c(
    "# BOFE MICE predictor-matrix audit protocol",
    "",
    "Purpose: assess the primary effectiveness MICE predictor matrix without selecting variables because they strengthen or weaken the treatment effect.",
    "",
    "Decision rules:",
    "1. Hard exclusions: patient ID, randomised group inside arm-split MICE, future-time predictors, role-disallowed variables, and predictors with no observed data.",
    "2. Protected anchors: variables in the primary analysis model and prior disease-control history should not be removed solely because their empirical association is weak.",
    "3. Candidate additions: variables suggested by quickpred are sensitivity candidates only after removing future-time and role-disallowed links.",
    "4. Candidate removals: current non-anchor links with minimal or non-estimable value/missingness association are tested only as pre-specified sensitivity variants.",
    "5. Treatment-effect results from candidate matrices should be summarized as robustness checks, not used to choose the primary matrix.",
    "",
    "Recommended stress tests:",
    "- Static matrix audit: confirm zero future links and explain every current link by role, time, missingness, and empirical association.",
    "- Overimputation check: mask an observed subset of target outcomes within arm and timepoint, impute under each pre-specified matrix, and compare calibration/error without looking at treatment-effect direction.",
    "- Primary-result stability check: run the GEE under current_analytic, no_utility_index_auxiliaries, no_same_visit_auxiliaries, history_only_controlled, and sanitized_quickpred; report the 12-month OR range transparently.",
    "- Sensitivity conclusion rule: prefer the current rule-based matrix unless an alternative shows clear diagnostic failure of the current matrix and remains clinically/methodologically defensible.",
    "",
    "Generated files:",
    "- audit/imputation_baseline_predictor_presence_effectiveness.csv",
    "- audit/imputation_predictor_pair_scores_effectiveness.csv",
    "- audit/imputation_predictor_matrix_decisions_effectiveness.csv",
    "- audit/imputation_predictor_matrix_variant_summary.csv"
  )
  writeLines(lines, path, useBytes = TRUE)
}

imputation_artifact <- read_canonical_artifact("imputation")
effectiveness_df <- imputation_artifact$effectiveness_df_impute
effectiveness_matrix <- imputation_artifact$effectiveness_predictor_matrix
effectiveness_quickpred <- imputation_artifact$effectiveness_quickpred_matrix
effectiveness_methods <- imputation_artifact$effectiveness_methods

configured_baseline_predictors <- method_config(
  "imputation",
  "predictor_selection",
  "baseline_predictors"
)
baseline_predictor_presence <- data.frame(
  variable = configured_baseline_predictors,
  present_in_effectiveness_frame = configured_baseline_predictors %in% names(effectiveness_df),
  stringsAsFactors = FALSE
)
missing_configured_baseline <- baseline_predictor_presence$variable[
  !baseline_predictor_presence$present_in_effectiveness_frame
]
if (length(missing_configured_baseline) > 0L) {
  stop(
    "Configured baseline predictor(s) missing from effectiveness MICE frame: ",
    paste(missing_configured_baseline, collapse = ", "),
    call. = FALSE
  )
}

pipeline_phase_info("09_imputation_predictor_matrix_audit", "building pair-level score table")
pair_scores <- build_pair_score_table(
  df = effectiveness_df,
  analytic_matrix = effectiveness_matrix,
  quickpred_matrix = effectiveness_quickpred,
  branch = "effectiveness",
  methods = effectiveness_methods
)

decision_summary <- pair_scores %>%
  count(audit_recommendation, evidence_band, sort = TRUE)

variant_matrices <- build_effectiveness_variant_matrices(
  effectiveness_df,
  effectiveness_matrix,
  effectiveness_quickpred
)
variant_summary <- bind_rows(lapply(names(variant_matrices), function(variant) {
  summarise_matrix_variant(
    effectiveness_df,
    variant_matrices[[variant]],
    branch = "effectiveness",
    variant = variant
  )
}))

write.csv(pair_scores, audit_path("imputation_predictor_pair_scores_effectiveness.csv"), row.names = FALSE)
write.csv(decision_summary, audit_path("imputation_predictor_matrix_decisions_effectiveness.csv"), row.names = FALSE)
write.csv(variant_summary, audit_path("imputation_predictor_matrix_variant_summary.csv"), row.names = FALSE)
write.csv(baseline_predictor_presence, audit_path("imputation_baseline_predictor_presence_effectiveness.csv"), row.names = FALSE)
write_audit_protocol(audit_path("imputation_predictor_matrix_audit_protocol.md"))

audit_artifact <- list(
  stage = "09_imputation_predictor_matrix_audit",
  baseline_predictor_presence = baseline_predictor_presence,
  effectiveness_pair_scores = pair_scores,
  effectiveness_decision_summary = decision_summary,
  effectiveness_variant_summary = variant_summary,
  artifact_name = "imputation_predictor_matrix_audit",
  created_at = Sys.time()
)
saveRDS(audit_artifact, audit_path("imputation_predictor_matrix_audit_artifact.rds"))

message("09_imputation_predictor_matrix_audit: saved predictor-matrix audit outputs.")
pipeline_phase_end(
  "09_imputation_predictor_matrix_audit",
  phase_started,
  "saved predictor-matrix audit outputs"
)
