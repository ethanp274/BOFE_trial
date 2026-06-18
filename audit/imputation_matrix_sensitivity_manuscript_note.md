# MICE Predictor-Matrix Sensitivity Note

Generated: 2026-06-18

## Purpose

This sensitivity analysis assessed whether the primary effectiveness result was robust to reasonable alternative MICE predictor matrices. It was conducted as a diagnostic sensitivity exercise only. The primary analysis remains the pre-specified/current analytic MICE branch used in the main pipeline.

The concern addressed was that the primary-effectiveness MICE predictor matrix should:

- avoid future-timepoint information leakage;
- include the variables needed for congeniality with the analysis model;
- avoid adding noisy auxiliary predictors that might destabilize or obscure the treatment effect;
- avoid choosing predictors because they improve the treatment estimate.

## Matrix Variants Tested

All variants used the same primary GEE analysis model after imputation:

`controlled_t ~ controlled_0 + age + gender + group * time`

The tested imputation predictor matrices were:

1. `current_analytic`: the current rule-based primary-effectiveness matrix.
2. `no_utility_index_auxiliaries`: current matrix with EQindex predictors removed.
3. `no_same_visit_auxiliaries`: current matrix with same-visit auxiliary outcome predictors removed.
4. `history_only_controlled`: baseline covariates plus earlier controlled outcomes only.
5. `sanitized_quickpred`: `mice::quickpred()` suggestions after removing future-timepoint and role-disallowed links.

The canonical primary matrix was not changed.

## Adjusted 12-Month GEE Results

| Matrix variant | Predictor links | Adjusted OR | 95% CI | p-value |
| --- | ---: | ---: | --- | ---: |
| Current analytic | 152 | 1.414 | 1.010 to 1.979 | 0.044 |
| No utility-index auxiliaries | 121 | 1.419 | 1.016 to 1.982 | 0.040 |
| No same-visit auxiliaries | 130 | 1.396 | 0.999 to 1.951 | 0.051 |
| History-only controlled | 117 | 1.443 | 1.029 to 2.024 | 0.033 |
| Sanitized quickpred | 63 | 1.403 | 0.999 to 1.970 | 0.051 |

The adjusted 12-month odds ratios ranged from 1.396 to 1.443. All variants favoured the intervention. Two variants lay just above the conventional 0.05 p-value threshold, so the analysis should be described as directionally stable but statistically borderline under some imputation specifications.

## Possible Manuscript Wording

Sensitivity analyses examined the robustness of the primary effectiveness result to alternative MICE predictor matrices. These included removal of EQ-5D utility-index auxiliaries, removal of same-visit auxiliary predictors, a more restrictive history-only disease-control matrix, and a time-sanitized empirical `quickpred` matrix. Future-timepoint predictors were prohibited in all tested specifications. The adjusted 12-month intervention effect was directionally consistent across variants, with odds ratios ranging from 1.40 to 1.44. Confidence intervals were close to the null in the most restrictive specifications, indicating that the result was robust in direction but borderline in precision.

## Files

- `R/09_imputation_predictor_matrix_audit.R`
- `R/10_imputation_matrix_sensitivity.R`
- `audit/imputation_predictor_matrix_audit_protocol.md`
- `audit/imputation_matrix_sensitivity_gee_12mo.csv`
- `audit/imputation_matrix_sensitivity_artifact.rds`
