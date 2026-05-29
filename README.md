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
- The active pipeline lives in `R/01_cleaning.R` through `R/07_manuscript_report.R`.
- The master full-pipeline launcher is PowerShell (`run_full_pipeline.ps1`).
- `bootstrap_bofe_vm.ps1` installs R, Rtools, and the CRAN packages needed to run the pipeline on a fresh Windows VM.
- Each stage reports progress in the console and writes the same markers to `outputs/pipeline_progress.log`.
- The CEA branch now reconstructs the legacy cost-complete cohort, uses arm-stratified GLM bootstrap inference only, and exports the reconstructed patient-level CEA cohort for validation.

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
- `R/02_imputation.R`: creates the MICE input dataset for outcome/QoL imputation with 20 PMM repetitions; cost summaries are explicitly excluded.
- `R/03_descriptives.R`: writes baseline, missingness, and disease-stratified descriptive outputs.
- `R/04_models.R`: fits the mixed-effects sensitivity model on the imputed data.
- `R/04b_gee.R`: fits the protocol-style GEE effectiveness model on the imputed data.
- `R/05_cost_effectiveness.R`: reconstructs the legacy cost-complete CEA cohort, fits complete-case cost/QALY GLMs, and runs 5000 arm-stratified bootstrap CEA simulations using the GLM branch only.
- `R/05_cost_effectiveness_parallel.R`: wrapper that enables the parallel bootstrap mode used by the master runner.
- `R/06_outputs.R`: exports legacy-style CSVs, manuscript-ready comparison tables (`manuscript_results_summary.csv`, `manuscript_results_effectiveness.csv`, `manuscript_results_cea.csv`, `manuscript_results_cea_summary.csv`), the reconstructed 745-patient CEA cohort, and a validation summary.
- `R/07_manuscript_report.R`: compiles a readable manuscript brief with the key effectiveness and cost-effectiveness results, including GLMM vs GEE comparison notes.
- `R/utils.R`: central constants, variable derivations, long-format construction, cost aggregation, and shared helpers.

Recommended run order from the project root:
1. `source("R/01_cleaning.R")`
2. `source("R/02_imputation.R")`
3. `source("R/03_descriptives.R")`
4. `source("R/04_models.R")`
5. `source("R/04b_gee.R")`
6. `source("R/05_cost_effectiveness_parallel.R")`
7. `source("R/06_outputs.R")`
8. `source("R/07_manuscript_report.R")`

For PowerShell on Windows, use `.\run_full_pipeline.ps1`.
For a fresh VM bootstrap, use `.\bootstrap_bofe_vm.ps1` (add `-SkipRtools` only if you already have a suitable toolchain).

Progress tracking:
- Each stage prints `START`, `INFO`, and `DONE` messages in the console.
- `R/utils.R` writes the same messages to `outputs/pipeline_progress.log`.
- The CEA bootstrap emits periodic iteration checkpoints so long runs stay visible.
- The master runner uses the parallel CEA bootstrap wrapper for faster runtime.
- The CEA outputs preserve the family split in `manuscript_results_cea.csv` and `manuscript_results_cea_summary.csv`.
- The latest rerun after fixing the legacy long-data helper shows nonzero bootstrap variation in incremental cost again.
- `run_full_pipeline.ps1` stops on the first failed phase and reports the failed step.
- The final manuscript brief is written to `outputs/manuscript_results_brief.md` and `outputs/manuscript_results_brief.txt`, with a compact table in `outputs/manuscript_results_overview.csv`.

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

Cost data:
- Complete-case only
- No imputation

## Primary Model
Legacy ITT analysis in `deprecated/regression_script.R`:
- Mixed-effects logistic regression on the complete-case longitudinal dataset
- Fixed effects:
  - treatment
  - time
  - treatment x time
  - age
  - sex
  - baseline control

Manuscript-aligned refactor:
- `R/02_imputation.R` reproduces the legacy wide imputation frame, splits by arm and disease, and runs 20 PMM imputations per subset before recombining them
- `R/04_models.R` reconstructs the repeated-measures analysis set and pools mixed-effects logistic regressions fit to the 20 imputed datasets
- `R/04b_gee.R` runs the protocol-style GEE analysis separately on the same imputed datasets
- `R/05_cost_effectiveness.R` combines the mixed-effects and GEE effectiveness outputs with complete-case CEA GLMs, reconstructs the legacy cost-complete patient cohort, and bootstraps the GLM CEA branch with separate intervention/control resampling
- `R/06_outputs.R` writes manuscript-ready summary CSVs for the effectiveness comparisons and the legacy-style cost-effectiveness results
- Key CEA outputs include `cea_patient_level.csv`, `cea_longitudinal.csv`, `cea_model_summaries.csv`, `cea_model_comparison.csv`, `cea_summary.csv`, and `cea_bootstrap_results.csv`
- Cost models remain complete-case

Validation note:
- The cleaned pipeline now preserves legacy time-specific `D1.3_*` and `D1.4_*` columns rather than merging on disease/group.
- After restoring that behavior, the pooled imputed mixed-effects 12-month OR is about `1.73`, while the legacy-style complete-case mixed model is about `1.83`.
- The manuscript draft's reported `1.81` estimate is much closer to the complete-case branch than to the imputed branch.

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

Current modular output:
- `R/05_cost_effectiveness.R` creates the legacy cost-complete patient-level CEA data, Gamma-log cost models, Gaussian-identity QALY models, arm-stratified GLM bootstrap results, and cost-effectiveness acceptability probabilities.

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
