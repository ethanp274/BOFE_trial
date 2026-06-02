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
      methods[[nm]] <- "pmm"
    } else if (is.factor(x)) {
      methods[[nm]] <- if (nlevels(x) <= 2) "logreg" else "polyreg"
    } else {
      methods[[nm]] <- "pmm"
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
    predictor_audit_path = NULL,
    diagnostics_path = NULL,
    write_first_completion = FALSE) {
  df_control <- df_impute %>% filter(group == "cg (control group)")
  df_intervention <- df_impute %>% filter(group == "ig (intervention group)")

  methods <- methods
  methods["group"] <- ""

  if (!is.null(predictor_audit_path)) {
    predictor_audit <- summarise_mice_predictors(df_impute, predictor_matrix, methods) %>%
      arrange(timepoint, variable)
    write.csv(predictor_audit, predictor_audit_path, row.names = FALSE)
  }

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

  saveRDS(mids_obj, file = file.path(out_dir, paste0(output_prefix, ".rds")))
  if (!is.null(diagnostics_path)) {
    cat(capture.output(summary(mids_obj), digits = 4), file = diagnostics_path, sep = "\n")
  }
  if (isTRUE(write_first_completion)) {
    completed1 <- mice::complete(mids_obj, 1)
    write.csv(completed1, file.path(out_dir, "imputed_dataset_1.csv"), row.names = FALSE)
  }

  mids_obj
}

run_full_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the full chained MICE with all available predictors")
  pred_matrix_full <- build_time_aware_mice_predictors(df_impute, id_col = "patient", group_col = "group")
  methods_full <- build_mice_methods(df_impute)

  run_arm_split_mice(
    df_impute = df_impute,
    predictor_matrix = pred_matrix_full,
    methods = methods_full,
    seed = 123,
    output_prefix = "mids_imputation_full",
    out_dir = out_dir,
    predictor_audit_path = audit_path("imputation_predictor_audit.csv"),
    diagnostics_path = file.path(out_dir, "imputation_diagnostics_full.txt"),
    write_first_completion = TRUE
  )
}

run_basic_mice_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the basic MICE sensitivity with legacy predictors")
  pred_matrix_basic <- build_basic_mice_predictors(df_impute)
  methods_basic <- build_mice_methods(df_impute)

  run_arm_split_mice(
    df_impute = df_impute,
    predictor_matrix = pred_matrix_basic,
    methods = methods_basic,
    seed = 456,
    output_prefix = "mids_imputation_basic",
    out_dir = out_dir,
    predictor_audit_path = audit_path("imputation_predictor_audit_basic.csv"),
    diagnostics_path = file.path(out_dir, "imputation_diagnostics_basic.txt"),
    write_first_completion = FALSE
  )
}

run_simple_within_arm_imputation <- function(df_impute, out_dir = "data_processed") {
  pipeline_phase_info("02_imputation", "running the simple within-arm mean/mode sensitivity")
  simple_imputed <- groupwise_simple_imputation(df_impute)
  saveRDS(simple_imputed, file = file.path(out_dir, "simple_imputed_wide.rds"))
  write.csv(simple_imputed, file.path(out_dir, "simple_imputed_wide.csv"), row.names = FALSE)
  simple_imputed
}

write_imputation_variant_summary <- function(
    df_impute,
    simple_imputed,
    complete_cases_path = "data_processed/complete_cases.rds",
    out_dir = "data_processed") {
  if (!file.exists(complete_cases_path)) {
    stop("write_imputation_variant_summary: missing ", complete_cases_path, ".")
  }

  complete_cases_n <- nrow(readRDS(complete_cases_path))
  variant_summary <- data.frame(
    variant = c("full_mice", "basic_mice", "simple_within_arm", "complete_cases"),
    object_path = c(
      file.path(out_dir, "mids_imputation_full.rds"),
      file.path(out_dir, "mids_imputation_basic.rds"),
      file.path(out_dir, "simple_imputed_wide.rds"),
      complete_cases_path
    ),
    n_rows = c(
      nrow(df_impute),
      nrow(df_impute),
      nrow(simple_imputed),
      complete_cases_n
    ),
    stringsAsFactors = FALSE
  )

  write.csv(variant_summary, audit_path("imputation_variant_summary.csv"), row.names = FALSE)
  variant_summary
}
