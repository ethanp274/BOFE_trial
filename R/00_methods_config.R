# R/00_methods_config.R
# Central source of truth for BOFE analysis method choices.
#
# Keep methodological decisions here rather than in script comments. Pipeline
# scripts should read values from this config, while README/AGENTS can summarize
# the current configuration for humans.

BOFE_METHODS_CONFIG <- list(
  study = list(
    timepoints = c(0, 3, 6, 9, 12),
    followup_timepoints = c(3, 6, 9, 12),
    group_levels = c("cg (control group)", "ig (intervention group)"),
    rationale = "BOFE has baseline plus 3-, 6-, 9-, and 12-month follow-up with control as the reference arm."
  ),
  cleaning = list(
    structural_zero_rules = list(
      T0 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
      T3 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
      T6 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
      T9 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\."),
      T12 = c("^D2\\.", "^D3\\.10", "^D3\\.11", "^D5\\.")
    ),
    manual_exclusions = c("PR2B"),
    manual_disease_corrections = list(OH5A = 2),
    complete_questionnaire_exclusions = c("JR4B", "LP5B", "PJ8A", "QF8A", "QX7A", "SV3B", "VB4A", "XY5A", "KJ2A", "KK1A"),
    rationale = "Structural zeros follow questionnaire skip logic; PR2B is excluded because the patient appears only at 12 months; OH5A is corrected to COPD; listed questionnaire exclusions are carried as explicit completeness-rule exceptions."
  ),
  outcomes = list(
    asthma_act_control_threshold = 20,
    copd_ccq_control_threshold = 2,
    controlled_definition = "Asthma is controlled when ACT >= 20; COPD is controlled when CCQ < 2.",
    rationale = "Disease-specific control thresholds are harmonised into controlled_* for combined asthma/COPD effectiveness models."
  ),
  imputation = list(
    main_variant = "analytic",
    main_effectiveness_variant = "effectiveness_analytic",
    main_cea_variant = "cea_analytic",
    replicates = 20L,
    split_by_arm = TRUE,
    numeric_method = "pmm",
    binary_factor_method = "logreg",
    multicategory_factor_method = "polyreg",
    predictor_rule = "analysis_specific_time_aware_predictors",
    predictor_selection = list(
      max_predictor_missing_fraction = 0.50,
      quickpred_min_correlation = 0.10,
      quickpred_min_usable_cases = 0.25,
      baseline_predictors = c(
        "condition", "gender", "age", "BMI", "smoking", "ihd"
      ),
      excluded_baseline_predictors = c(
        "ethnicity", "location", "education", "selection", "live_alone",
        "FVC_0", "FEV1_0", "height", "weight", "BMI_range",
        "diabetes", "employed", "num_meds"
      ),
      effectiveness = list(
        frame_note = "Primary effectiveness imputation keeps only patient ID, arm, core baseline covariates, controlled outcomes, and EQindex summaries.",
        include_patterns = c("^controlled_[0-9]+$", "^EQindex_[0-9]+$"),
        include_roles = c("id_design", "baseline_covariate", "effectiveness_outcome", "utility_index"),
        exclude_roles = c("cost", "resource_use", "utility_item", "adherence"),
        allow_cost_predictors = FALSE,
        allow_qol_item_predictors = FALSE,
        rationale = "The primary effectiveness model only needs controlled outcomes plus categorical age, sex, and baseline control. Condition, categorical age, BMI, smoking, IHD, and EQindex summaries are retained as parsimonious clinically relevant auxiliary measures; vague, weak, redundant, or non-informative baseline auxiliaries such as selection, num_meds, and constant diabetes are excluded to avoid auxiliary-variable sprawl and unnecessary conditioning."
      ),
      cea = list(
        frame_note = "CEA imputation keeps patient ID, arm, core baseline covariates, controlled outcomes, raw EQ-5D item columns, and canonical half-year cost summaries.",
        include_patterns = c("^controlled_[0-9]+$", "^EQ5D5L\\.[1-5]_[0-9]+$", "^cost_[CMFHO](6|12)$"),
        include_roles = c("id_design", "baseline_covariate", "effectiveness_outcome", "utility_item", "cost"),
        exclude_roles = c("resource_use", "utility_index", "adherence"),
        allow_cost_predictors = TRUE,
        allow_qol_item_predictors = TRUE,
        rationale = "The CEA imputation jointly imputes costs and EQ-5D item responses so QALYs can be recomputed under configured tariffs and cost-effect correlation is preserved. Sparse questionnaire resource-use auxiliaries and weak extra baseline auxiliaries are excluded because raw cost files, longitudinal EQ-5D, and core clinical/demographic predictors, including categorical age, BMI, smoking, and IHD, are the explainable primary imputation source."
      )
    ),
    cost_predictor_policy = "Cost summaries are imputed only in the CEA branch; they are excluded from primary effectiveness imputation and may predict cost/QALY targets only inside the CEA-specific imputation matrix.",
    full_seed = 123L,
    sensitivity_variants = c("full", "simple", "complete_cases"),
    rationale = "The main MICE stage preserves ITT through two explicit analysis-specific branches: a parsimonious effectiveness imputation and a CEA imputation for costs plus EQ-5D items. Sensitivity variants isolate simpler imputation assumptions."
  ),
  effectiveness = list(
    primary_model_family = "gee",
    correlation_structure = "exchangeable",
    model_timepoints = c(3, 6, 9, 12),
    use_followup_only = TRUE,
    unadjusted_formula = "controlled_t ~ group * time",
    adjusted_formula = "controlled_t ~ controlled_0 + age + gender + group * time",
    adjusted_covariates = c("controlled_0", "age", "gender"),
    default_imputation_variant = "full",
    mixed_effects_optimizer = "bobyqa",
    mixed_effects_formula_suffix = "+ (1 | patient)",
    rationale = "The primary effectiveness analysis is a marginal GEE on follow-up visits, adjusted for baseline control, age, and sex; mixed-effects logistic regression is retained as sensitivity."
  ),
  economics = list(
    perspective = "Italian health system / Sicilian Regional Health Service",
    cost_months_first_half = c("2022_06", "2022_07", "2022_08", "2022_09", "2022_10", "2022_11"),
    cost_months_second_half = c("2022_12", "2023_01", "2023_02", "2023_03", "2023_04", "2023_05"),
    cost_summary_columns = c(
      "cost_C6", "cost_C12",
      "cost_M6", "cost_M12",
      "cost_F6", "cost_F12",
      "cost_H6", "cost_H12",
      "cost_O6", "cost_O12"
    ),
    cost_completeness_rule = list(
      source = "legacy regression_script.R CEA cohort",
      complete_if_any_raw_cost_file_present = TRUE,
      zero_fill_absent_cost_categories_for_complete_patients = TRUE,
      keep_all_cost_summaries_missing_for_no_source_patients = TRUE,
      medication_file_used_as_audit_anchor = TRUE,
      preserve_invalid_period_values_for_imputation = TRUE,
      rationale = "Legacy CEA defined cost_complete_pts from patients with any non-empty economic-data row, then summed absent cost components with na.rm=TRUE. The active pipeline mirrors that by treating absent cost categories as zero for patients present in any raw cost file, while preserving NA for patients absent from all economic cost files and for invalid in-file period values such as 9999 in the 2023 medication months."
    ),
    intervention_cost_per_consultation = 40,
    intervention_consultations = 2L,
    wtp_threshold_eur_per_qaly = 29000,
    main_cost_family = "gaussian_identity",
    available_cost_families = c("gaussian_identity", "gamma_log"),
    main_eq5d_tariff = "italian",
    tariff_sensitivity = "uk",
    qaly_method = "Trapezoidal area under the EQ-5D utility curve across 0, 3, 6, 9, and 12 months.",
    bootstrap_iterations = 5000L,
    nested_mi_bootstrap = TRUE,
    intervention_cost_sweep = seq(40, 200, by = 20),
    eq5d_tariff_lookup = list(
      italian = list(
        source = "Finch et al, 2022",
        mobility = c("1" = 0, "2" = 0.051, "3" = 0.064, "4" = 0.244, "5" = 0.329),
        selfcare = c("1" = 0, "2" = 0.046, "3" = 0.056, "4" = 0.216, "5" = 0.257),
        activity = c("1" = 0, "2" = 0.050, "3" = 0.064, "4" = 0.225, "5" = 0.255),
        pain = c("1" = 0, "2" = 0.047, "3" = 0.088, "4" = 0.353, "5" = 0.408),
        anxiety = c("1" = 0, "2" = 0.044, "3" = 0.109, "4" = 0.318, "5" = 0.322)
      ),
      uk = list(
        source = "Rowen et al, 2026",
        mobility = c("1" = 0, "2" = 0.032, "3" = 0.058, "4" = 0.179, "5" = 0.279),
        selfcare = c("1" = 0, "2" = 0.038, "3" = 0.060, "4" = 0.162, "5" = 0.206),
        activity = c("1" = 0, "2" = 0.049, "3" = 0.086, "4" = 0.184, "5" = 0.212),
        pain = c("1" = 0, "2" = 0.056, "3" = 0.066, "4" = 0.371, "5" = 0.479),
        anxiety = c("1" = 0, "2" = 0.041, "3" = 0.126, "4" = 0.313, "5" = 0.391)
      )
    ),
    rationale = "The main CEA uses the complex-MICE cohort, Gaussian-identity cost GLM on total_cost, Gaussian-identity QALY GLM, and a nested MI bootstrap over shared patient IDs."
  ),
  environment_overrides = list(
    imputation_variant = "BOFE_IMPUTATION_VARIANT",
    bootstrap_iterations = "BOFE_BOOTSTRAP_ITERATIONS",
    sensitivity_bootstrap_iterations = "BOFE_SENSITIVITY_BOOTSTRAP_ITERATIONS",
    smoke_run_cleaning = "BOFE_SMOKE_RUN_CLEANING",
    smoke_skip_cea = "BOFE_SMOKE_SKIP_CEA",
    smoke_cea_bootstraps = "BOFE_SMOKE_CEA_BOOTSTRAPS"
  ),
  validation = list(
    max_abs_incremental_cost = 1e7,
    smoke_default_cea_bootstraps = 1L,
    rationale = "Smoke tests should catch shape/pooling explosions quickly without running expensive MICE or 5000-bootstrap analyses by default."
  )
)

method_config <- function(...) {
  path <- list(...)
  value <- BOFE_METHODS_CONFIG
  for (key in path) {
    if (!is.list(value) || !key %in% names(value)) {
      stop("method_config: unknown config path ", paste(path, collapse = "."), ".")
    }
    value <- value[[key]]
  }
  value
}
