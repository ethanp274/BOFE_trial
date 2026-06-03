# R/02_imputation_helpers.R
# Shared helpers for the wide MICE imputation stage and its sensitivity variants.

source("R/utils.R")

library(dplyr)
library(tidyr)
library(mice)

build_imputation_wide_frame <- function(trial_df) {
  alias_map <- build_wide_analysis_alias_map()

  trial_df <- trial_df %>%
    select(-any_of(names(alias_map)))
  trial_df <- rename_by_aliases(trial_df, alias_map)

  df_impute <- trial_df %>%
    select(any_of(c(
      names(alias_map),
      paste0("controlled_", TIMEPOINTS),
      paste0("EQindex_", TIMEPOINTS),
      unlist(
        lapply(TIMEPOINTS, function(tp) {
          paste0(c("EQ5D5L.1", "EQ5D5L.2", "EQ5D5L.3", "EQ5D5L.4", "EQ5D5L.5"), "_", tp)
        }),
        use.names = FALSE
      ),
      COST_SUMMARY_COLUMNS
    )))

  factor_cols <- c(
    "group", "patient", "condition", "gender", "ethnicity", "education",
    "selection", "live_alone", "age", "BMI_range", "smoking", "diabetes",
    "ihd", "employed", "controlled_0", "controlled_3", "controlled_6",
    "controlled_9", "controlled_12"
  )

  for (nm in intersect(factor_cols, names(df_impute))) {
    df_impute[[nm]] <- as.factor(df_impute[[nm]])
  }

  assert_data_contract(df_impute, "imputation_wide")
  df_impute
}

build_mice_methods <- function(df) {
  methods <- mice::make.method(df)
  for (nm in names(df)) {
    if (nm == "patient") {
      methods[[nm]] <- ""
      next
    }

    x <- df[[nm]]
    if (all(is.na(x)) || all(!is.na(x))) {
      methods[[nm]] <- ""
    } else if (is.numeric(x)) {
      methods[[nm]] <- method_config("imputation", "numeric_method")
    } else if (is.factor(x)) {
      methods[[nm]] <- if (nlevels(x) <= 2) {
        method_config("imputation", "binary_factor_method")
      } else {
        method_config("imputation", "multicategory_factor_method")
      }
    } else {
      methods[[nm]] <- method_config("imputation", "numeric_method")
    }
  }
  methods
}

build_branch_mice_inputs <- function(df_impute, branch = c("effectiveness", "cea")) {
  branch <- match.arg(branch)
  branch_df <- build_imputation_branch_frame(df_impute, branch = branch)
  predictor_matrix <- build_analytic_mice_predictors(branch_df, branch = branch)
  quickpred_matrix <- build_quickpred_matrix(branch_df)
  methods <- build_mice_methods(branch_df)
  predictor_audit <- summarise_mice_predictors(branch_df, predictor_matrix, methods) %>%
    arrange(timepoint, variable)
  quickpred_comparison <- compare_imputation_predictor_matrices(
    analytic_matrix = predictor_matrix,
    quickpred_matrix = quickpred_matrix,
    df = branch_df,
    branch = branch
  )
  quickpred_summary <- summarise_predictor_matrix_comparison(
    quickpred_comparison,
    predictor_matrix,
    quickpred_matrix,
    branch = branch
  )

  list(
    branch = branch,
    df = branch_df,
    predictor_matrix = predictor_matrix,
    quickpred_matrix = quickpred_matrix,
    methods = methods,
    predictor_audit = predictor_audit,
    quickpred_comparison = quickpred_comparison,
    quickpred_summary = quickpred_summary
  )
}

run_arm_split_mice <- function(
    df_impute,
    predictor_matrix,
    methods,
    seed,
    output_prefix,
    out_dir = "data_processed",
    write_first_completion = FALSE) {
  df_control <- df_impute %>% filter(group == "cg (control group)")
  df_intervention <- df_impute %>% filter(group == "ig (intervention group)")

  methods <- methods
  methods["group"] <- ""

  predictor_audit <- summarise_mice_predictors(df_impute, predictor_matrix, methods) %>%
    arrange(timepoint, variable)

  mids_control <- mice(
    df_control,
    m = IMPUTATION_REPLICATES,
    method = methods,
    predictorMatrix = predictor_matrix,
    seed = seed,
    printFlag = FALSE
  )
  mids_intervention <- mice(
    df_intervention,
    m = IMPUTATION_REPLICATES,
    method = methods,
    predictorMatrix = predictor_matrix,
    seed = seed,
    printFlag = FALSE
  )
  mids_obj <- mice::rbind(mids_control, mids_intervention)

  diagnostics <- capture.output(summary(mids_obj), digits = 4)
  first_completion <- NULL
  if (isTRUE(write_first_completion)) {
    first_completion <- mice::complete(mids_obj, 1)
  }

  list(
    branch = output_prefix,
    mids = mids_obj,
    predictor_matrix = predictor_matrix,
    methods = methods,
    predictor_audit = predictor_audit,
    diagnostics = diagnostics,
    first_completion = first_completion
  )
}

run_analytic_mice_imputation <- function(
    df_impute,
    branch = c("effectiveness", "cea"),
    out_dir = "data_processed",
    write_first_completion = FALSE) {
  branch <- match.arg(branch)
  branch_inputs <- build_branch_mice_inputs(df_impute, branch = branch)

  pipeline_phase_info(
    "02_imputation",
    sprintf(
      "running %s MICE: %d variables, %d imputed targets, %d predictor links",
      branch,
      ncol(branch_inputs$df),
      sum(branch_inputs$methods != ""),
      sum(branch_inputs$predictor_matrix != 0)
    )
  )

  mids_branch <- run_arm_split_mice(
    df_impute = branch_inputs$df,
    predictor_matrix = branch_inputs$predictor_matrix,
    methods = branch_inputs$methods,
    seed = if (branch == "effectiveness") method_config("imputation", "full_seed") else method_config("imputation", "full_seed") + 1000L,
    output_prefix = paste0("mids_imputation_", branch),
    out_dir = out_dir,
    write_first_completion = write_first_completion
  )

  c(
    branch_inputs,
    list(
      mids = mids_branch$mids,
      diagnostics = mids_branch$diagnostics,
      first_completion = mids_branch$first_completion
    )
  )
}

run_effectiveness_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  run_analytic_mice_imputation(
    df_impute = df_impute,
    branch = "effectiveness",
    out_dir = out_dir,
    write_first_completion = TRUE
  )
}

run_cea_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  run_analytic_mice_imputation(
    df_impute = df_impute,
    branch = "cea",
    out_dir = out_dir,
    write_first_completion = TRUE
  )
}

run_full_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info(
    "02_imputation",
    "run_full_mice_imputation() is retained as a compatibility wrapper for the primary effectiveness branch"
  )
  run_effectiveness_mice_imputation(df_impute, out_dir = out_dir)
}

run_simple_within_arm_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the simple within-arm mean/mode sensitivity")
  simple_imputed <- groupwise_simple_imputation(df_impute)
  list(
    branch = "simple_within_arm",
    data = simple_imputed
  )
}

write_imputation_variant_summary <- function(
    df_impute,
    simple_imputed,
    out_dir = "data_processed") {
  cleaning_artifact <- read_canonical_artifact("cleaning")
  complete_cases_n <- nrow(cleaning_artifact$complete_cases)
  variant_summary <- data.frame(
    variant = c("full_mice", "simple_within_arm", "complete_cases"),
    canonical_artifact = c(
      canonical_artifact_path("imputation"),
      canonical_artifact_path("sensitivity"),
      canonical_artifact_path("cleaning")
    ),
    n_rows = c(
      nrow(df_impute),
      nrow(simple_imputed),
      complete_cases_n
    ),
    stringsAsFactors = FALSE
  )

  variant_summary
}
