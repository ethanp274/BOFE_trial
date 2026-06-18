# BOFE Robustness and Sensitivity Analysis Master Note

Generated: 2026-06-18

## Purpose

This note consolidates the robustness checks, sensitivity analyses, and exploratory model comparisons built into the BOFE analysis pipeline. It is intended as a reviewer-facing methods memory aid: it records what was tested, why it was tested, what is currently manuscript-facing, and what should be rerun before numerical reporting.

The guiding principle is that sensitivity analyses should assess whether conclusions are stable under defensible alternative assumptions. They should not be used to choose the most favourable estimate.

## Current Primary Analysis Anchor

The current clean manuscript pipeline uses:

- Primary effectiveness analysis: marginal logistic GEE on multiply imputed follow-up disease-control outcomes.
- Primary adjusted formula: `controlled_t ~ controlled_0 + age + gender + group * time`.
- Primary imputation branch: arm-split MICE with branch-specific, time-aware predictors.
- Primary CEA branch: CEA-specific MICE, Gaussian-identity GLM for costs, Gaussian-identity GLM for QALYs, and nested MI bootstrap.

Current main manuscript summary:

| Domain | Main result |
| --- | --- |
| Adjusted 12-month GEE OR | 1.414, 95% CI 1.010 to 1.979, p = 0.044 |
| Unadjusted 12-month GEE OR | 1.229, 95% CI 0.913 to 1.654, p = 0.173 |
| Incremental cost | -116.75 EUR, pooled CI -1366.77 to 1133.27 |
| Incremental QALY | 0.0224, pooled CI -0.0204 to 0.0652 |
| ICER | -5211.86 EUR/QALY, bootstrap CI -148368.57 to 136654.37 |
| Probability cost-effective at 29000 EUR/QALY | 0.861 |

Interpretation guardrail: the primary effectiveness result favours the intervention but is close to the null boundary. The economic result favours the intervention directionally through lower mean costs and higher QALYs, but uncertainty is wide. The paper should emphasize robustness of direction and uncertainty, not overstate precision.

## Robustness Checks by Family

### 1. Adjusted vs Unadjusted Effectiveness Models

Status: current result available from main pipeline.

Purpose:

- Tests whether the intervention contrast depends on baseline adjustment.
- The unadjusted model estimates the marginal treatment-by-time contrast without baseline control, age, or sex.
- The adjusted model is the primary manuscript model because it aligns with the configured analysis plan and improves precision by accounting for baseline disease control and core demographics.

Current results:

| Model | 12-month OR | 95% CI | p-value |
| --- | ---: | --- | ---: |
| Unadjusted GEE | 1.229 | 0.913 to 1.654 | 0.173 |
| Adjusted GEE | 1.414 | 1.010 to 1.979 | 0.044 |

Reviewer-facing interpretation:

Adjustment materially increases precision and the estimated intervention contrast. This should be described transparently: the primary result is adjusted for baseline control, age, and sex, while the unadjusted contrast is directionally consistent but weaker and imprecise.

Relevant files:

- `R/04b_gee.R`
- `R/04_effectiveness_helpers.R`
- `results/model_gee_timepoint_effects.csv`
- `results/manuscript_results_summary.csv`

### 2. GEE vs Mixed-Effects Longitudinal Effectiveness Models

Status: implemented/runnable; current mixed-effectiveness artifact is not present in `models/`.

Purpose:

- GEE estimates a marginal population-average intervention effect.
- Mixed-effects logistic regression estimates a subject-specific effect with patient-level random intercepts.
- This addresses reviewer concern that conclusions may depend on the repeated-measures correlation structure.

Current pipeline position:

- GEE is the primary effectiveness model.
- `R/04_models.R` retains the mixed-effects logistic model as sensitivity analysis.
- `R/08_sensitivity_analyses.R` can run both GEE and mixed-effects models across full MICE, simple imputation, and complete-case variants.

Recommended reporting:

- Present GEE as primary because it is protocol-style and population-average.
- Use mixed-effects results as supportive sensitivity once rerun.
- Do not mix marginal and subject-specific ORs as if they estimate the same estimand; compare direction, magnitude, and inference qualitatively.

Relevant files:

- `R/04b_gee.R`
- `R/04_models.R`
- `R/04_effectiveness_helpers.R`
- `R/08_sensitivity_analyses.R`

### 3. MICE Predictor-Matrix Design and Leakage Audit

Status: current diagnostic result available.

Purpose:

- Tests whether the primary-effectiveness MICE predictor matrix is defensible.
- Addresses concerns about future-time leakage, overfitting/noisy auxiliaries, and empirical variable selection.

Current audit findings:

- The current analytic effectiveness matrix has 152 predictor links.
- The current analytic matrix has zero future-timepoint links.
- All configured baseline predictors are present: `condition`, `gender`, `age`, `BMI`, `smoking`, and `ihd`.
- Raw `mice::quickpred()` suggested future-time links, confirming that quickpred should remain advisory rather than a design rule.

Matrix sensitivity results:

| Matrix variant | Predictor links | Adjusted 12-month OR | 95% CI | p-value |
| --- | ---: | ---: | --- | ---: |
| Current analytic | 152 | 1.414 | 1.010 to 1.979 | 0.044 |
| No utility-index auxiliaries | 121 | 1.419 | 1.016 to 1.982 | 0.040 |
| No same-visit auxiliaries | 130 | 1.396 | 0.999 to 1.951 | 0.051 |
| History-only controlled | 117 | 1.443 | 1.029 to 2.024 | 0.033 |
| Sanitized quickpred | 63 | 1.403 | 0.999 to 1.970 | 0.051 |

Reviewer-facing interpretation:

The adjusted 12-month OR remained directionally stable across predictor-matrix variants, ranging from 1.396 to 1.443. Two specifications were borderline at the conventional 0.05 threshold, so this is best described as directionally robust but statistically close to the null boundary under more restrictive imputation assumptions.

Relevant files:

- `R/09_imputation_predictor_matrix_audit.R`
- `R/10_imputation_matrix_sensitivity.R`
- `audit/imputation_predictor_matrix_audit_protocol.md`
- `audit/imputation_matrix_sensitivity_gee_12mo.csv`
- `audit/imputation_matrix_sensitivity_manuscript_note.md`

### 4. Full MICE vs Simple Imputation vs Complete-Case Effectiveness Analysis

Status: implemented/runnable in `R/08_sensitivity_analyses.R`; current sensitivity artifact is missing and should be regenerated before quoting numeric results.

Purpose:

- Tests whether the primary effectiveness result depends on the MICE missing-data strategy.
- Compares:
  - full branch-specific MICE;
  - simple within-arm mean/mode imputation;
  - complete-case analysis.

Design logic:

- Full MICE is the primary approach because it preserves the ITT population and models missingness using observed baseline and longitudinal information.
- Simple within-arm imputation is deliberately naive and should be treated as a stress test, not a preferred method.
- Complete-case analysis is useful for transparency but may be biased if missingness is related to outcome, disease severity, or follow-up behavior.

Recommended reporting:

- Use full MICE as primary.
- Report whether simple and complete-case results are directionally consistent.
- Avoid presenting simple imputation as a valid equal alternative; it is a robustness stress test.

Relevant files:

- `R/02_imputation.R`
- `R/02_imputation_helpers.R`
- `R/08_sensitivity_analyses.R`
- expected output: `results/effectiveness_sensitivity_summary.csv`

### 5. CEA Full MICE vs Simple Imputation vs Complete-Case Analysis

Status: implemented/runnable in `R/08_sensitivity_analyses.R`; current sensitivity artifact is missing and should be regenerated before quoting numeric results.

Purpose:

- Tests whether CEA conclusions depend on how missing cost and EQ-5D information are handled.
- Compares:
  - full CEA-specific MICE;
  - simple within-arm imputation;
  - complete-case CEA.

Design logic:

- The main CEA branch imputes costs and EQ-5D items jointly within a CEA-specific imputation frame.
- Cost summaries are kept out of the primary effectiveness imputation but included in the CEA branch.
- Complete-case CEA is useful as a conservative comparator but can lose patients and may not preserve the ITT estimand.

Recommended reporting:

- Use full CEA-specific MICE as primary.
- Summarize whether incremental cost, incremental QALY, and probability cost-effective have the same direction under simple and complete-case assumptions.

Relevant files:

- `R/05_cost_effectiveness.R`
- `R/05_cost_effectiveness_helpers.R`
- `R/08_sensitivity_analyses.R`
- expected output: `results/cea_sensitivity_summary.csv`

### 6. Economic Model Family: Gaussian-Identity vs Gamma-Log Cost GLM

Status: Gaussian-identity is current primary; Gamma-log remains available in helper/config pathways for targeted model comparison.

Purpose:

- Tests whether cost conclusions depend on distributional assumptions for skewed cost data.
- Gaussian-identity estimates adjusted mean cost differences directly.
- Gamma-log models multiplicative cost ratios and may behave differently with zero or near-zero costs.

Current pipeline position:

- Main CEA uses Gaussian-identity cost GLM, configured in `R/00_methods_config.R`.
- `R/05_cost_effectiveness_helpers.R::cost_model_spec()` supports both `gaussian_identity` and `gamma_log`.

Recommended reporting:

- Defend Gaussian-identity as directly aligned with mean cost differences for economic evaluation.
- Use Gamma-log only as a sensitivity/model-comparison branch if rerun and documented.
- If Gamma-log is reported, distinguish cost ratios from cost differences.

Relevant files:

- `R/00_methods_config.R`
- `R/05_cost_effectiveness_helpers.R`
- `results/cea_model_comparison.csv`

### 7. CEA GLM vs GEE/Repeated-Cost Exploratory Branches

Status: historical/exploratory; not part of the current clean main CEA pipeline.

Purpose:

- Earlier refactor work explored interval-level GEE-style CEA comparisons.
- The current CEA has been simplified back to patient-level GLM models for total cost and QALYs.

Current pipeline position:

- Main CEA uses patient-level GLMs.
- The active notes indicate there is no long-form CEA audit path and no main CEA GEE branch.

Reviewer-facing interpretation:

If asked, state that repeated-cost GEE-style CEA formulations were explored during code refactoring but were not retained because the manuscript-facing economic estimand is patient-level total cost and QALY over 12 months. The retained model is simpler, interpretable, and aligned with the final economic endpoint.

Relevant context:

- `AGENTS.md` historical activity log.
- `R/05_cost_effectiveness.R`
- `R/05_cost_effectiveness_helpers.R`

### 8. EQ-5D Tariff Sensitivity

Status: implemented/runnable in `R/08_sensitivity_analyses.R`; current sensitivity artifact is missing and should be regenerated before quoting numeric results.

Purpose:

- Tests whether QALY and cost-effectiveness conclusions depend on the EQ-5D-5L value set.
- Main CEA uses the configured Italian tariff.
- Sensitivity branch recomputes QALYs under the configured UK tariff.

Design logic:

- The Italian tariff is primary because the economic perspective is Italian/Sicilian health service.
- UK tariff sensitivity tests whether conclusions are robust to a different utility valuation system.

Recommended reporting:

- Report tariff sensitivity only as sensitivity, not as competing primary valuation.
- Focus on whether incremental QALY and probability cost-effective remain directionally similar.

Relevant files:

- `R/00_methods_config.R`
- `R/outcome_qol_helpers.R`
- `R/08_sensitivity_analyses.R`
- expected output: `results/cea_tariff_sensitivity_summary.csv`

### 9. Intervention-Cost Sweep

Status: implemented/runnable in `R/08_sensitivity_analyses.R`; current sensitivity artifact is missing and should be regenerated before quoting numeric results.

Purpose:

- Tests economic conclusions under alternative assumptions about intervention delivery cost.
- Main assumption: 40 EUR per consultation, two consultations.
- Sensitivity sweep: 40 to 200 EUR per consultation in 20 EUR increments.

Design logic:

- This addresses uncertainty in pharmacist consultation cost and implementation cost assumptions.
- It is especially useful for sceptical reviewers because the intervention appears economically favourable under the current mean estimates.

Recommended reporting:

- Report whether probability cost-effective remains acceptable across plausible intervention-cost assumptions.
- If there is a threshold at which conclusions change, report it clearly.

Relevant files:

- `R/00_methods_config.R`
- `R/08_sensitivity_analyses.R`
- expected output: `results/cea_cost_sensitivity_summary.csv`

### 10. Nested MI Bootstrap and Acceptability Curve

Status: current result available in canonical CEA artifact.

Purpose:

- Quantifies uncertainty in cost-effectiveness while respecting multiple imputation.
- Resamples the same patients across all imputations within each bootstrap draw.
- Produces incremental cost, incremental QALY, ICER interval, and cost-effectiveness acceptability curve.

Current result:

- 5000 bootstrap draws are present in `models/cea_artifact.rds`.
- Probability cost-effective at 29000 EUR/QALY is 0.861.
- Note: `results/cea_bootstrap_results.csv` is stale/small and should be regenerated before use; rely on the canonical RDS artifact.

Recommended reporting:

- Prefer net benefit/acceptability framing over overinterpreting the ICER because the incremental QALY interval crosses zero and the ICER interval is wide.

Relevant files:

- `R/05_cost_effectiveness.R`
- `R/05_cost_effectiveness_helpers.R`
- `models/cea_artifact.rds`
- `results/cea_acceptability_curve.csv`

### 11. Secondary Medication-Adherence Outcomes

Status: deprecated/disabled pending methods revision.

Purpose:

- Earlier exploratory work attempted medication-adherence and last-missed-dose longitudinal binary models.
- These were disabled because skip-logic/coding interpretation was not sufficiently stable for clean manuscript reporting.

Current pipeline position:

- `R/00_methods_config.R` sets `effectiveness.secondary_outcomes.enabled = FALSE`.
- Existing secondary CSVs are stale and should not be used as current manuscript outputs.

Reviewer-facing interpretation:

If asked, state that adherence variables were examined during exploratory development but were not retained as manuscript-facing secondary model outcomes because their coding and skip logic were not robust enough to support defensible inference.

Relevant files:

- `R/00_methods_config.R`
- stale outputs: `results/secondary_effectiveness_summary.csv`, `results/model_gee_secondary_summaries.csv`

## How to Use This Note

For a paper response or methods appendix, the most defensible structure is:

1. State the primary model and estimand first.
2. Group sensitivity analyses by concern:
   - missing-data assumptions;
   - predictor-matrix/leakage assumptions;
   - longitudinal model family;
   - economic model and costing assumptions;
   - utility tariff assumptions.
3. Report whether direction, magnitude, and conclusion are stable.
4. Avoid selecting the primary model based on statistical significance.
5. Clearly mark deprecated exploratory analyses as not manuscript-facing.

## Suggested Reviewer-Response Language

We conducted a series of sensitivity and robustness checks to examine whether the trial conclusions depended on modelling, imputation, or economic assumptions. For effectiveness, we compared adjusted and unadjusted GEE models, retained mixed-effects logistic regression as a subject-specific sensitivity model, and evaluated alternative MICE predictor matrices designed to test the influence of utility auxiliaries, same-visit predictors, prior-outcome-only history, and time-sanitized empirical predictor selection. Future-timepoint predictors were prohibited in all primary-effectiveness imputation specifications. The adjusted 12-month intervention effect remained directionally consistent across these matrix variants, with odds ratios ranging from 1.40 to 1.44, although some specifications were close to the conventional 0.05 threshold.

For the economic analysis, the primary CEA used a CEA-specific MICE branch, patient-level Gaussian-identity GLMs for total cost and QALYs, and a nested MI bootstrap with 5000 draws. Additional implemented sensitivity checks examine complete-case and simple-imputation CEA, alternative intervention-cost assumptions, and alternate EQ-5D tariff valuation. These checks are intended to assess robustness of the direction and economic interpretation rather than to select a preferred model post hoc.

## Refresh Checklist Before Manuscript Submission

- Run `Rscript R/run_smoke_tests.R`.
- Regenerate the full main pipeline with `./run_full_pipeline.ps1`.
- Run `Rscript R/04_models.R` if mixed-effects sensitivity is to be reported numerically.
- Run `Rscript R/08_sensitivity_analyses.R` before quoting complete-case/simple/tariff/cost-sweep values.
- Run `Rscript R/09_imputation_predictor_matrix_audit.R` and `Rscript R/10_imputation_matrix_sensitivity.R` if the imputation artifacts change.
- Regenerate `results/cea_bootstrap_results.csv` from `models/cea_artifact.rds` before using it; the current CSV is stale.

## Key Files

- `R/00_methods_config.R`
- `R/02_imputation.R`
- `R/04b_gee.R`
- `R/04_models.R`
- `R/05_cost_effectiveness.R`
- `R/08_sensitivity_analyses.R`
- `R/09_imputation_predictor_matrix_audit.R`
- `R/10_imputation_matrix_sensitivity.R`
- `results/manuscript_results_summary.csv`
- `audit/imputation_matrix_sensitivity_gee_12mo.csv`
