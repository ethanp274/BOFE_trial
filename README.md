# BOFE Trial Analysis Repository

## Overview

This repository contains the statistical analysis workflow for the **Better Outcomes for Everybody (BOFE)** trial.

The BOFE trial is a pragmatic randomised controlled trial evaluating whether a pharmacist-led medicines use review intervention improves disease control and cost-effectiveness among adults with asthma and COPD.

Core outputs include:
- Effectiveness analyses
- Cost-effectiveness analyses
- Longitudinal models
- Publication tables
- Figures and diagnostics

Current state:
- The active main pipeline runs `R/01_cleaning.R`, `R/02_imputation.R`, `R/03_descriptives.R`, `R/04b_gee.R`, `R/05_cost_effectiveness.R`, `R/06_outputs.R`, and `R/07_manuscript_report.R`; `R/04_models.R` is reserved for sensitivity analysis.
- The master full-pipeline launcher is PowerShell (`run_full_pipeline.ps1`).
- `bootstrap_bofe_vm.ps1` installs R, Rtools, and the CRAN packages needed to run the pipeline on a fresh Windows VM.
- Each stage reports progress in the console and writes the same markers to `logs/pipeline_progress.log`.
- Trained model objects are written to `models/`; manuscript-facing tables and summaries are written to `results/`; runtime logs are written to `logs/`.
- The main CEA branch now uses the complex-MICE imputed wide cohort with a nested bootstrap that resamples the same patients across all imputations before pooling within each draw; the main cost model is currently Gaussian-identity on `total_cost`, and secondary CEA/effectiveness sensitivity analyses reuse the same core helpers wherever possible.
- The CEA acceptability threshold is now set to `29000` euros per QALY, which better matches the manuscript's `25000` GBP comparison point after currency conversion.
- The CEA ICER point estimate is reported as pooled incremental cost divided by pooled incremental QALY, while the bootstrap ICER percentiles still come from the iteration-level ratio distribution.
- The manuscript summary now labels uncertainty explicitly with `pooled_ci_lower` / `pooled_ci_upper` and `bootstrap_ci_lower` / `bootstrap_ci_upper`.

---

# Study Summary

## Trial Design

- Pragmatic parallel-group RCT
- Sicily, Italy
- 100 community pharmacies
- 12-month follow-up
- 2:1 intervention:control allocation

## Conditions
- Asthma
- COPD

## Intervention
CRC-MUR:
- Community pharmacist-delivered medicines use review
- Delivered at baseline and 6 months

## Primary Outcome
Disease control at 12 months:
- Asthma: ACT >= 20
- COPD: CCQ < 2

---

# Repository Structure

## Core Scripts

### Modular pipeline in `R/`

Current refactor entry points:
- `R/01_cleaning.R`: reads raw questionnaire/cost files, applies manual corrections, derives `controlled_*`, `EQindex_*`, cost summaries, and completeness flags.
- `R/02_imputation.R`: creates the MICE input dataset for outcome/QoL and cost imputation with 20 PMM repetitions. The `full` MICE branch is the pipeline standard; cost summaries are imputed but are not allowed to predict non-cost targets. The `basic` MICE sensitivity mirrors the legacy predictor matrix (baseline age/sex, baseline control, baseline EQindex, and lagged control only).
- `R/03_descriptives.R`: writes the analyzed-population Table 1 with disease-aware missingness columns, plus a separate disease-aware missingness summary; continuous Table 1 entries are reported as means with 95% CIs and p-values are rounded to 3 decimals.
- `R/04b_gee.R`: fits the protocol-style GEE effectiveness model on the selected imputation variant with per-imputation progress checkpoints, using follow-up observations only and sorting rows so each patient cluster is contiguous. This is the default effectiveness analysis.
- `R/04_models.R`: fits the mixed-effects sensitivity model on the selected imputation variant with the same follow-up-only long-panel reconstruction helper and a faster `bobyqa` optimizer.
- `R/05_cost_effectiveness.R`: fits the main wide-imputed CEA branch using the complex MICE dataset and writes the manuscript-facing CEA outputs.
- `R/08_sensitivity_analyses.R`: runs the secondary effectiveness and CEA sensitivity analyses, including alternate imputation variants, intervention-cost sweeps, and the UK EQ-5D tariff sensitivity branch. CEA sensitivity branches reuse the Gaussian-identity cost model, and MI CEA sensitivity branches use the nested bootstrap path where applicable.
- `R/06_outputs.R`: exports the manuscript-ready summary table (`manuscript_results_summary.csv`), using the shared wide-to-long helper only for the effectiveness export.
- `R/07_manuscript_report.R`: compiles a readable manuscript brief with the key effectiveness and cost-effectiveness results without creating an extra overview CSV.
- `R/utils.R`: central constants, variable derivations, long-format construction, cost aggregation, and shared helpers.
- `R/utils.R::build_economic_data()` collapses the raw monthly cost panels into canonical half-year `cost_*` summaries and returns only the patient identifier plus those summaries.

Recommended run order from the project root:
1. `source("R/01_cleaning.R")`
2. `source("R/02_imputation.R")`
3. `source("R/03_descriptives.R")`
4. `source("R/04b_gee.R")`
5. `source("R/05_cost_effectiveness.R")`
6. `source("R/06_outputs.R")`
7. `source("R/07_manuscript_report.R")`
8. Optional sensitivity pass: `source("R/08_sensitivity_analyses.R")`

For PowerShell on Windows, use `.\run_full_pipeline.ps1`.
For a fresh VM bootstrap, use `.\bootstrap_bofe_vm.ps1` (add `-SkipRtools` only if you already have a suitable toolchain).

Progress tracking:
- Each stage prints `START`, `INFO`, and `DONE` messages in the console.
- `R/utils.R` writes the same messages to `logs/pipeline_progress.log`.
- `R/04b_gee.R` now reports progress while reconstructing each imputation and while fitting the unadjusted and adjusted GEE models.
- `R/04_models.R` is reserved for sensitivity runs and uses the same shared long-panel reconstruction helper as the GEE script.
- The main CEA branch now runs directly from `R/05_cost_effectiveness.R`; the optional sensitivity runner handles heavier alternate scenarios separately.
- The sensitivity runner emits its own progress checkpoints for alternate CEA and effectiveness scenarios.
- The manuscript-ready summary CSV preserves the pooled CEA uncertainty summary in `results/manuscript_results_summary.csv`.
- The latest rerun after fixing the legacy long-data helper shows nonzero bootstrap variation in incremental cost again.
- `run_full_pipeline.ps1` stops on the first failed phase and reports the failed step.
- The final manuscript brief is written to `results/manuscript_results_brief.md`; the concise CSV summary is `results/manuscript_results_summary.csv`.

### `deprecated/new_data_cleaning_pipe.R`
Legacy data-cleaning and reconstruction pipeline.

Responsibilities:
- Read raw SPSS datasets
- Clean questionnaire data
- Convert structural zeros to NA
- Merge longitudinal datasets
- Prepare analysis-ready data

Historical reference only; the modular pipeline now keeps structural-zero rules in `R/utils.R`.

---

### `deprecated/regression_script.R`
Legacy inferential analysis script.

Responsibilities:
- Power calculations
- Longitudinal reshaping
- Mixed-effects logistic regression
- Cost-effectiveness modelling
- QALY construction

Most important file for statistical revisions.

---

### `deprecated/Tables and figures.R`
Publication table generation.

Responsibilities:
- Baseline characteristics
- Outcome tables
- Statistical summaries
- Controlled/uncontrolled analyses

Produces manuscript-ready outputs.

---

### `deprecated/Plots.R`
Exploratory analysis and visualisation.

Responsibilities:
- Diagnostic plots
- Missingness exploration
- EQindex trajectory inspection
- ACT/CCQ visualisations

Useful for QA and exploratory work.

---

### `deprecated/BOFE script_copy.R`
Legacy master workflow.

Contains:
- Early data pipeline logic
- Merging workflow
- Exploratory checks
- Historical analysis code

Useful for tracing development history.

---

# Data Structure

## Longitudinal Format

Data are collected at:
- T0
- T3
- T6
- T9
- T12

Variable naming convention:
- `variable_0`
- `variable_3`
- `variable_6`
- etc.

Example:
- `ACT.SCORE_0`
- `EQindex_12`

---

# Key Variables

## IDs
- `D1.1` = pharmacy ID
- `D1.2` = patient ID

## Group assignment
- `D1.4` in modular processed datasets
  - intervention
  - control

## Disease
- `D1.3` in modular processed datasets
  - 1 = asthma
  - 2 = COPD

## Outcomes
- `ACT.SCORE_*`
- `CCQ.SCORE_*`
- `controlled_*`

## Quality of life
- `EQindex_*`

---

# Statistical Approach

## Missing Data
Outcome data:
- Multiple imputation (MICE)
- 20 imputations
- Split by treatment arm before imputation
- Time-aware predictor matrix to prevent later variables from imputing earlier ones

Cost data:
- Included in the main MICE stage as cost components and resource-utilisation auxiliaries
- Secondary CEA scenarios are handled in `R/08_sensitivity_analyses.R`

## Primary Model
Main effectiveness analysis in `R/04b_gee.R`:
- GEE logistic regression on the imputed repeated-measures dataset
- Fixed effects:
  - treatment
  - time
  - treatment x time
  - age
  - sex
  - baseline control

Manuscript-aligned refactor:
- `R/01_cleaning.R` reads only raw questionnaire and cost files, then derives a single wide analysis dataset for downstream stages
- `R/02_imputation.R` builds one wide MICE frame, splits by arm before imputation, uses a time-aware predictor matrix for the pipeline-standard `full` branch, and writes `audit/imputation_predictor_audit.csv` for line-by-line auditability; cost summaries are excluded as predictors for non-cost targets, and sensitivity imputation variants now live in `R/08_sensitivity_analyses.R`, where `audit/imputation_predictor_audit_basic.csv`, `audit/imputation_variant_summary.csv`, and the simple within-arm imputed dataset are generated only when the secondary analyses are run
- `R/04b_gee.R` reconstructs the follow-up repeated-measures analysis set, keeps patient clusters contiguous, and pools the protocol-style GEE analyses fit to the selected imputation variant through a shared helper
- `R/04_models.R` keeps the mixed-effects logistic regression as a sensitivity analysis only and also delegates to the shared effectiveness helper
- Both `R/04_models.R` and `R/04b_gee.R` accept `BOFE_IMPUTATION_VARIANT` values of `full` (default), `basic`, `simple`, or `complete_cases`; the main scripts now call reusable helpers instead of shelling out to variant subprocesses
- `R/05_cost_effectiveness.R` combines the complex-MICE imputed wide cohort with the main CEA branch and leaves secondary scenarios to `R/08_sensitivity_analyses.R`; the main branch currently uses the Gaussian-identity cost model on `total_cost`, preserves negative QALYs in the Gaussian QALY model, and uses strict cost-component handling
- `R/05_cost_effectiveness_helpers.R` prepares one patient-level CEA frame per completed imputation before bootstrapping, then each nested bootstrap draw resamples the same patient IDs across those prepared frames and pools the GLM results across imputations
- `R/06_outputs.R` writes one manuscript-ready summary CSV for the effectiveness comparisons and the main cost-effectiveness results, using the shared wide-to-long helper only for the effectiveness export
- `R/07_manuscript_report.R` reads only the canonical current GEE and CEA artifacts and fails fast if they are missing, so it cannot silently rebuild a report from stale fallbacks
- Key CEA outputs include `results/cea_model_summaries.csv`, `results/cea_model_comparison.csv`, `results/cea_bootstrap_results.csv`, and `results/cea_acceptability_curve.csv`
- The bootstrap cost coefficient column is now named `cost_group_effect` so the Gaussian-identity branch is not mislabeled as a log ratio in exported tables
- Cost models are available as the main complex-MICE branch, with alternate CEA scenarios reported separately in `R/08_sensitivity_analyses.R` using the same cost model unless the sensitivity explicitly tests a different model

---

# Economic Evaluation

Perspective:
- Italian health system

Included costs:
- inpatient
- outpatient
- medication
- laboratory
- delivery

Intervention cost:
- €40 per consultation
- €80 total intervention cost

QALYs:
- Calculated using EQ-5D-5L area-under-curve approach

Cost inputs:
- Raw monthly cost panels are summed into half-year `cost_*` summaries in `build_economic_data()`
- Downstream code works from those canonical half-year summaries rather than the monthly raw panel columns
- Complete-case CEA now requires all canonical half-year cost summaries; partially missing cost components are not silently treated as zero

Current modular output:
- `R/05_cost_effectiveness.R` uses the complex-MICE imputed wide cohort for the main CEA branch, and `R/08_sensitivity_analyses.R` contains the secondary CEA scenarios using the same core data source and model helpers where possible.

---

# Reproducibility Notes

## Important
Many scripts assume:
- existing objects in memory
- specific working directories
- local raw-data paths

Recommended improvements:
- use `here::here()`
- centralise paths
- modularise scripts
- add renv lockfile

---

# Known Issues

## Structural zeros
Many questionnaire variables use:
- `0 = structurally missing`

The cleaning script now applies these rules from a centralized mapping in `R/utils.R`.

## Legacy duplication
There is substantial repeated cleaning code across scripts.

## Mixed conventions
Some scripts use:
- `complete_cases`
Others use:
- `df_complete`

Careful object tracking is required.

---

# Suggested Refactor Priorities

1. Modularise data cleaning
2. Centralise variable definitions
3. Reduce repeated code
4. Improve reproducibility
5. Add validation tests
6. Separate exploratory vs production code
7. Create reusable modelling functions

---

# Dependencies

Core packages:
- tidyverse
- mice
- lme4
- ggplot2
- haven
- labelled
- survey
- openxlsx
- summarytools

---

# Manuscript Context

Target journal:
- BJGP

Supporting documents included:
- manuscript draft
- reviewer responses
- original protocol paper

These documents explain:
- analytic rationale
- methodological decisions
- reviewer concerns
- interpretation choices

---

# Recommended Workflow for New Contributors

1. Read protocol paper
2. Read manuscript Methods
3. Run cleaning pipeline
4. Inspect merged datasets
5. Run descriptives
6. Run regression models
7. Validate outputs against manuscript tables

---

# References

Protocol publication:
Manfrin et al. (2021)

Main manuscript:
BJGP revision draft

Protocol details: :contentReference[oaicite:7]{index=7}  
Main manuscript details: :contentReference[oaicite:8]{index=8}
