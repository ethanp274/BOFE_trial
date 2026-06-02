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
    "selection", "live_alone", "BMI_range", "smoking", "diabetes",
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
    if (all(is.na(x))) {
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

run_full_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the full chained MICE with all available predictors")
  pred_matrix_full <- build_time_aware_mice_predictors(df_impute, id_col = "patient", group_col = "group")
  methods_full <- build_mice_methods(df_impute)

  run_arm_split_mice(
    df_impute = df_impute,
    predictor_matrix = pred_matrix_full,
    methods = methods_full,
    seed = method_config("imputation", "full_seed"),
    output_prefix = "mids_imputation_full",
    out_dir = out_dir,
    write_first_completion = TRUE
  )
}

run_basic_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the configured basic MICE sensitivity")
  pred_matrix_basic <- build_basic_mice_predictors(df_impute)
  methods_basic <- build_mice_methods(df_impute)

  run_arm_split_mice(
    df_impute = df_impute,
    predictor_matrix = pred_matrix_basic,
    methods = methods_basic,
    seed = method_config("imputation", "basic_seed"),
    output_prefix = "mids_imputation_basic",
    out_dir = out_dir,
    write_first_completion = FALSE
  )
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
    variant = c("full_mice", "basic_mice", "simple_within_arm", "complete_cases"),
    canonical_artifact = c(
      canonical_artifact_path("imputation"),
      canonical_artifact_path("sensitivity"),
      canonical_artifact_path("sensitivity"),
      canonical_artifact_path("cleaning")
    ),
    n_rows = c(
      nrow(df_impute),
      nrow(df_impute),
      nrow(simple_imputed),
      complete_cases_n
    ),
    stringsAsFactors = FALSE
  )

  variant_summary
}
