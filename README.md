# BOFE Trial Analysis Repository

This repository contains the statistical analysis workflow for the Better Outcomes for Everybody (BOFE) trial, a pragmatic randomised controlled trial evaluating a community pharmacist-led medicines use review intervention for adults with asthma or COPD.

The code reproduces the main effectiveness analysis, cost-effectiveness analysis, manuscript-facing tables, and selected robustness checks.

## Dataset DOI

**Clean analysis dataset:** https://dx.doi.org/10.5287/ora-rjkzjgg4k

The publication export is generated after cleaning and before imputation. It replaces source patient IDs with random short IDs, omits pharmacy IDs, anonymizes residence as letter categories, keeps age/gender/BMI/smoking as interpretable categorical labels, and writes missing values as literal `NA`.

## Study Summary

BOFE was a pragmatic, parallel-group randomized controlled trial conducted through community pharmacies in Sicily, Italy.

Key design features:

- Population: adults with asthma or COPD
- Intervention: pharmacist-led chronic respiratory condition medicines use review
- Comparator: usual care
- Follow-up: baseline, 3, 6, 9, and 12 months
- Allocation: 2:1 intervention to control
- Primary outcome: disease control at 12 months

Disease control is harmonized across conditions:

- Asthma: controlled if ACT score >= 20
- COPD: controlled if CCQ score < 2

## Repository Layout

Core directories:

- `R/`: active analysis scripts and helper modules
- `data_processed/`: cleaned and derived analysis datasets
- `models/`: fitted model artifacts
- `results/`: manuscript tables, summaries, figures, and exported diagnostics
- `audit/`: robustness notes, imputation audits, and methodological checks
- `logs/`: runtime progress logs
- `deprecated/`: historical scripts retained for traceability

## Reproducing the Main Analysis

### Quick Start: Using the Public Dataset

For full reproducibility without access to raw SPSS files, use the published anonymized dataset:

1. Download the public dataset from: https://dx.doi.org/10.5287/ora-rjkzjgg4k
2. Extract `bofe_publication_anonymized_long.csv` to `data_processed/`
3. Set the data source to public and run the pipeline starting from imputation:

```r
# Set the public data source for this session
Sys.setenv(BOFE_DATA_SOURCE = "public")

# Run the pipeline (R/01_cleaning.R is skipped since public data is already clean)
source("R/02_imputation.R")
source("R/03_descriptives.R")
source("R/04b_gee.R")
source("R/05_cost_effectiveness.R")
source("R/06_outputs.R")
source("R/07_manuscript_report.R")
```

**Why skip R/01_cleaning.R?** The public dataset is already post-cleaning (anonymized and pre-derived outcomes/utilities). The data-source decision happens in `R/02_imputation.R`, which loads the public CSV directly if `BOFE_DATA_SOURCE="public"`, bypassing unnecessary re-processing.

All downstream analyses (imputation, GEE, CEA, tables) remain identical. The public dataset includes:

- Reproducible random patient IDs
- Anonymized residence locations
- Categorical age/gender/BMI/smoking (suitable for regression)
- Missing values as literal `NA`
- Pre-computed disease control and EQ-5D utilities
- Half-year cost summaries

See `data_processed/bofe_publication_anonymized_long_codebook.md` for full variable documentation.

### Standard Analysis: Using Raw Data

To run the analysis with access to raw data:

Run scripts from the repository root.

Main pipeline:

```r
source("R/01_cleaning.R")
source("R/02_imputation.R")
source("R/03_descriptives.R")
source("R/04b_gee.R")
source("R/05_cost_effectiveness.R")
source("R/06_outputs.R")
source("R/07_manuscript_report.R")
```

On Windows, the full pipeline can also be run with:

```powershell
.\run_full_pipeline.ps1
```

Optional reviewer/data-sharing export:

```r
source("R/01b_publication_long_dataset.R")
```

Optional robustness and diagnostic scripts:

```r
source("R/04_models.R")
source("R/08_sensitivity_analyses.R")
source("R/09_imputation_predictor_matrix_audit.R")
source("R/10_imputation_matrix_sensitivity.R")
source("R/11_control_transition_sankey.R")
```

For a quick validation pass before expensive model runs:

```r
Rscript R/run_smoke_tests.R
```

## Key Analysis Scripts

- `R/01_cleaning.R`: reads raw questionnaire and cost files, applies cleaning rules, derives disease-control outcomes and EQ-5D utilities, attaches cost summaries, and writes `data_processed/cleaning_artifact.rds`.
- `R/01b_publication_long_dataset.R`: creates the anonymized long-form publication dataset and codebook from the cleaning artifact.
- `R/02_imputation.R`: builds branch-specific MICE datasets for primary effectiveness and cost-effectiveness, then writes `data_processed/imputation_artifact.rds`.
- `R/03_descriptives.R`: creates baseline and missingness summaries.
- `R/04b_gee.R`: fits the primary marginal GEE effectiveness model.
- `R/04_models.R`: fits mixed-effects sensitivity models.
- `R/05_cost_effectiveness.R`: fits the primary cost-effectiveness analysis using the CEA-specific MICE branch.
- `R/06_outputs.R`: assembles manuscript-facing summary outputs.
- `R/07_manuscript_report.R`: writes a concise manuscript results brief.
- `R/08_sensitivity_analyses.R`: runs configured effectiveness and economic sensitivity analyses.
- `R/09_imputation_predictor_matrix_audit.R`: audits the primary-effectiveness MICE predictor matrix.
- `R/10_imputation_matrix_sensitivity.R`: tests pre-specified predictor-matrix variants.
- `R/11_control_transition_sankey.R`: creates aggregated Sankey-style transition figures for observed baseline-to-12-month disease-control status.

Shared helpers and configuration:

- `R/00_methods_config.R`: central source of methodological settings and rationale notes
- `R/00_contracts.R`: data-shape contracts used by pipeline validation
- `R/utils.R`: compatibility loader for focused helper modules
- `R/validation_helpers.R`: reusable validation checks

## Main Statistical Approach

### Missing Data

The main analyses use multiple imputation by chained equations, configured in `R/00_methods_config.R`.

Primary effectiveness imputation:

- Arm-split MICE
- Time-aware predictor matrix
- Core baseline covariates: condition, gender, categorical age, BMI, smoking, and IHD
- Longitudinal disease-control outcomes and EQ-5D utility summaries
- Costs and raw EQ-5D item variables excluded from the primary effectiveness branch

Cost-effectiveness imputation:

- Separate CEA-specific MICE branch
- Includes core baseline covariates, disease-control outcomes, raw EQ-5D-5L item responses, and canonical half-year cost summaries
- Enables QALYs to be recomputed under configured tariff assumptions

### Effectiveness Analysis

The primary effectiveness model is a marginal logistic GEE fitted to follow-up disease-control outcomes after multiple imputation.

Adjusted model:

```r
controlled_t ~ controlled_0 + age + gender + group * time
```

The mixed-effects logistic model is retained as a sensitivity analysis rather than the primary analysis.

### Economic Evaluation

The cost-effectiveness analysis uses:

- Perspective: Italian health system / Sicilian Regional Health Service
- Cost categories: outpatient, laboratory, medication, delivery, inpatient, and intervention cost
- Intervention cost: 40 EUR per consultation, two consultations for intervention patients
- QALYs: trapezoidal area under the EQ-5D-5L utility curve
- Cost model: Gaussian-identity GLM for mean costs
- QALY model: Gaussian-identity GLM
- Uncertainty: nested multiple-imputation bootstrap
- Cost-effectiveness threshold: configured in `R/00_methods_config.R`

## Main Outputs

Common reviewer-facing outputs:

- `results/manuscript_results_brief.md`
- `results/manuscript_results_summary.csv`
- `results/model_gee_summaries.csv`
- `results/model_gee_timepoint_effects.csv`
- `results/cea_summary.csv`
- `results/cea_acceptability_curve.csv`
- `results/sankey_control_transition_control.png`
- `results/sankey_control_transition_intervention.png`

Robustness and audit outputs:

- `audit/master_robustness_and_sensitivity_note.md`
- `audit/imputation_matrix_sensitivity_manuscript_note.md`
- `audit/imputation_predictor_audit_effectiveness.csv`
- `audit/imputation_predictor_audit_cea.csv`
- `audit/imputation_matrix_sensitivity_gee_12mo.csv`
- `results/effectiveness_sensitivity_summary.csv`
- `results/cea_sensitivity_summary.csv`
- `results/cea_cost_sensitivity_summary.csv`
- `results/cea_tariff_sensitivity_summary.csv`

## Dependencies

The main pipeline uses R and common CRAN packages, including:

- tidyverse
- mice
- lme4
- geepack
- ggplot2
- haven
- labelled
- survey
- openxlsx
- summarytools

For a fresh Windows VM, use:

```powershell
.\bootstrap_bofe_vm.ps1
```

## Notes for Reviewers

The repository distinguishes between canonical RDS artifacts and exported CSV/Markdown deliverables. The RDS artifacts are the internal source of truth for pipeline stages; the CSV and Markdown files are generated for inspection, manuscript tables, and figures.

The anonymized publication CSV is intended for model testing and transparency. It is intentionally pre-imputation, so missing values are retained as `NA` rather than filled.

The optional Sankey figures use observed, pre-imputation disease-control status at baseline and 12 months. Missing 12-month disease-control status is shown explicitly rather than dropped.

## References

Protocol publication:

- Manfrin et al. (2021)

Main manuscript:

- BJGP revision draft

## Maintainer Notes and Current State

This section is primarily for project maintainers and future analysis agents. It is placed at the end so reviewers can first find the study summary, reproducibility instructions, and dataset DOI.

### Current Pipeline State

- The active main pipeline runs `R/01_cleaning.R`, `R/02_imputation.R`, `R/03_descriptives.R`, `R/04b_gee.R`, `R/05_cost_effectiveness.R`, `R/06_outputs.R`, and `R/07_manuscript_report.R`.
- `R/01b_publication_long_dataset.R` is an optional post-cleaning anonymized publication export.
- `R/04_models.R` is reserved for mixed-effects sensitivity analysis.
- `R/08_sensitivity_analyses.R` handles optional effectiveness and economic sensitivity scenarios.
- `R/09_imputation_predictor_matrix_audit.R` and `R/10_imputation_matrix_sensitivity.R` document and test MICE predictor-matrix robustness.
- `R/11_control_transition_sankey.R` writes aggregate transition figures and patient-level trace CSVs from the anonymized publication dataset.
- Pharmacy-level clustering is handled in the sensitivity bundle through a mixed-effects model with patient and pharmacy random intercepts. Pharmacy-id GEE is skipped by design because `geeglm` supports only one clustering id; using `id = pharmacy` would replace the patient repeated-measures cluster rather than add a second clustering level.
- The refreshed pharmacy-clustering GLMM sensitivity remains supportive at 12 months: OR 1.687, 95% CI 1.016 to 2.802, p = 0.043.
- `run_full_pipeline.ps1` is the master PowerShell launcher.
- `logs/pipeline_progress.log` records stage progress.
- `models/` stores fitted model artifacts; `results/` stores manuscript-facing summaries and figures; `audit/` stores methodological diagnostics.

### Current Manuscript-Facing Results

Primary effectiveness analysis:

- Marginal logistic GEE on follow-up disease-control outcomes after multiple imputation.
- Adjusted model: `controlled_t ~ controlled_0 + age + gender + group * time`.
- Main adjusted 12-month OR: 1.414, 95% CI 1.010 to 1.979, p = 0.044.
- Unadjusted 12-month OR: 1.229, 95% CI 0.913 to 1.654, p = 0.173.

Main CEA:

- CEA-specific MICE branch.
- Patient-level Gaussian-identity GLM for total cost.
- Gaussian-identity GLM for QALYs.
- Nested MI bootstrap with 5000 draws.
- CEA sensitivity branches default to 1500 bootstrap draws for future reruns.
- Incremental cost: -116.75 EUR, pooled CI -1366.77 to 1133.27.
- Incremental QALY: 0.0224, pooled CI -0.0204 to 0.0652.
- Probability cost-effective at 29000 EUR/QALY: 0.861.

### Robustness Checks

The main robustness dossier is:

- `audit/master_robustness_and_sensitivity_note.md`

Implemented checks include:

- Adjusted vs unadjusted GEE
- GEE vs mixed-effects logistic regression
- Full MICE vs simple imputation vs complete-case analysis
- Predictor-matrix sensitivity variants
- Complete-case and simple-imputation CEA
- Intervention-cost sweep
- EQ-5D tariff sensitivity
- Nested MI bootstrap uncertainty

Predictor-matrix sensitivity testing found adjusted 12-month ORs ranging from 1.396 to 1.443 across tested variants. All variants favoured the intervention, although some were close to the conventional p = 0.05 threshold.

CEA sensitivity outputs remained directionally favourable. Complete-case CEA estimated incremental cost -492.89 EUR, incremental QALY 0.0320, and probability cost-effective 0.971 at 29000 EUR/QALY. Simple-imputation CEA estimated incremental cost -132.40 EUR, incremental QALY 0.0276, and probability cost-effective 0.910. UK tariff sensitivity estimated incremental QALY 0.0190 and probability cost-effective 0.839.

### Caveats for Maintainers

- `results/secondary_effectiveness_summary.csv` and related secondary adherence outputs are stale/deprecated. Current config sets `effectiveness.secondary_outcomes.enabled = FALSE`.
- After the 2026-06-23 sensitivity run, `models/cea_artifact.rds` and `results/cea_summary.csv` contain a 10-bootstrap main CEA artifact, while `results/manuscript_results_summary.csv` still contains the older 5000-bootstrap manuscript-facing CEA values. Rerun `R/05_cost_effectiveness.R`, then `R/06_outputs.R` and `R/07_manuscript_report.R`, before final economic reporting.
- The configured bootstrap policy is 5000 draws for the main CEA and 1500 draws for future CEA sensitivity analyses in `R/08_sensitivity_analyses.R`. Override sensitivity draws only deliberately via `BOFE_SENSITIVITY_BOOTSTRAP_ITERATIONS`.
- If upstream data or methods change, rerun the full pipeline before quoting manuscript-facing results.
- Generated CSV/Markdown outputs should be regenerated from canonical artifacts rather than manually edited.
- Method choices should be changed first in `R/00_methods_config.R`, then reflected in code if needed.

Suggested refresh sequence before final manuscript edits:

```powershell
Rscript R/run_smoke_tests.R
.\run_full_pipeline.ps1
Rscript R/04_models.R
Rscript R/08_sensitivity_analyses.R
Rscript R/09_imputation_predictor_matrix_audit.R
Rscript R/10_imputation_matrix_sensitivity.R
Rscript R/11_control_transition_sankey.R
```
