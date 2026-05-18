# Audit Report: BOFE Analysis Pipeline Red Team Review

This report outlines the findings of a "red team" audit comparing the current R analysis pipeline (modular and legacy) against the study protocol and draft manuscript.

## 1. Divergence in Primary Outcome Estimates
**Finding:** The manuscript draft reports a primary outcome Odds Ratio (OR) of **1.81** (95% CI 1.14 to 2.87). The new modular pipeline, following the multiple imputation (MI) branch, produces an OR of approximately **1.70 - 1.73**.
- **Probable Cause:** The manuscript's reported 1.81 estimate appears to match the **Complete Case (CC)** analysis more closely than the **Multiple Imputation (MI)** analysis. Using CC results for the primary ITT conclusion deviates from the MI-based ITT strategy described in the protocol.
- **Divergence:** Protocol specifies ITT via Multiple Imputation. If the manuscript reports CC, it is a divergence from the planned analysis.
- **Recommendation:**
    - Verify if the manuscript intended to report CC or MI.
    - If MI is the primary analysis, the manuscript text and tables should be updated to reflect the pooled MI estimate (~1.73).
    - If CC was intended, the rationale for this change (e.g., poor imputation performance) must be documented.

## 2. Structural Zero Handling
**Finding:** The pipeline relies on a fragile `apply_na_if_from_legacy` function that scrapes `new_data_cleaning_pipe.R` to identify which variables should have zeros converted to `NA`.
- **Concern:** This makes the pipeline dependent on the formatting of a legacy script. Any change in the legacy script could silently break the cleaning logic.
- **Divergence:** The protocol implies a systematic handling of structural zeros based on questionnaire logic, but the current implementation is ad-hoc.
- **Recommendation:** 
    - Move structural-zero definitions into a formal mapping table (e.g., a CSV or a structured list in `utils.R`).
    - Explicitly document which questionnaire items have structural zeros (e.g., resource use when no visits occurred vs. missing values).

## 3. Multiple Imputation Fidelity
**Finding:** Legacy scripts (`multiple_imputation.R`) were found to have dropped the 10th imputation during the data reconstruction phase. The new pipeline (`R/02_imputation.R` and `R/04_models.R`) corrects this and uses all 10 imputations.
- **Impact:** This correction explains a small portion of the drift in results compared to older runs.
- **Recommendation:** Maintain the use of all 10 imputations to ensure statistical robustness. Consider increasing to 20 or 50 imputations if convergence or stability is an issue.

## 4. Manual Patient Exclusions and Corrections
**Finding:** Critical data cleaning steps (excluding patient `PR2B`, correcting disease for `OH5A`, and the list of 10 patients in `add_completeness_flags`) are hardcoded.
- **Concern:** While consistent across scripts, the clinical/data-quality justification for these specific exclusions is not documented in the code or the manuscript.
- **Recommendation:**
    - Add comments to `R/01_cleaning.R` and `R/utils.R` explaining the specific data quality failure for each excluded patient (e.g., "JR4B: excessive missingness in outcome items").
    - Ensure these exclusions are mentioned in the "Participants" or "Statistical Analysis" section of the manuscript to satisfy reviewer transparency requirements.

## 5. Statistical Model Specification
**Finding:** The current `R/04_models.R` uses `relevel(group, ref = 2)`, which sets the Intervention Group (`ig`) as the reference level. 
- **Concern:** This is counter-intuitive for reporting. Usually, the Control Group (`cg`) is the reference so that the `group` coefficient directly represents the effect of the intervention.
- **Recommendation:** Change to `relevel(group, ref = "cg (control group)")` and update the coefficient extraction logic to ensure the reported OR is `ig vs cg`.

## 6. Economic Evaluation Fidelity
**Finding:** The QALY calculation correctly implements the 3-month interval trapezoidal AUC approach. Costing uses the Italian health system perspective with an €80 intervention cost.
- **Fidelity:** Matches the protocol and `AGENTS.md` descriptions.
- **Recommendation:** Ensure that the "Complete Case" cost-effectiveness analysis is clearly labeled as such, as costs are not imputed, unlike the effectiveness outcomes.

## 7. Next Steps for Fidelity
1. **Regenerate Outputs:** Run the full pipeline and generate the `pipeline_validation_summary.csv`.
2. **Manuscript Alignment:** Update the draft manuscript with the latest MI-pooled estimates and GEE results (as per the protocol's original plan).
3. **Refactor Cleaning:** Replace the legacy scraping logic with a centralized variable map.
