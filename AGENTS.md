# AGENTS.md — BOFE Trial Analysis Project

## Project Overview

**Project name:** Better Outcomes for Everybody (BOFE) Trial  
**Study type:** Pragmatic, parallel-group randomised controlled trial (RCT)  
**Clinical area:** Chronic respiratory disease (Asthma + COPD)  
**Intervention:** Community pharmacist-led medicines use review (CRC-MUR)  
**Primary objective:** Evaluate effectiveness and cost-effectiveness of pharmacist-led intervention versus usual care.  
**Primary outcome:** Disease control at 12 months.  
**Secondary outcomes:** Longitudinal disease control, QALYs, healthcare utilisation, costs, medication adherence, pharmaceutical care issues.

Core manuscripts and protocol located in `/context_source_files/`

Key references:
- Manuscript describes final analytic decisions and reported models. :contentReference[oaicite:0]{index=0}
- Protocol defines original design and planned analyses. :contentReference[oaicite:1]{index=1}
- Reviewer responses explain rationale for deviations and interpretation decisions. :contentReference[oaicite:2]{index=2}

Historical legacy scripts now live in `deprecated/`; the active pipeline is under `R/`.

Current pipeline status:
- The main pipeline runs `R/01_cleaning.R`, `R/02_imputation.R`, `R/03_descriptives.R`, `R/04b_gee.R`, `R/05_cost_effectiveness.R`, `R/06_outputs.R`, and `R/07_manuscript_report.R`.
- `R/04_models.R` is retained for sensitivity analysis only.
- `run_full_pipeline.ps1` is the master launcher for the full sequence.
- `bootstrap_bofe_vm.ps1` provisions R, Rtools, and the CRAN package set needed for a fresh Windows VM.
- Each stage writes console progress markers and appends them to `logs/pipeline_progress.log`.
- Trained model artefacts are stored in `models/`; tables, summaries, and audits are stored in `results/`; runtime logs are stored in `logs/`.
- `R/01_cleaning.R` reads only raw questionnaire `.sav` files and raw cost `.csv` files, then writes `data_processed/cleaning_artifact.rds` as the canonical cleaning artifact.
- `R/utils.R` is now a compatibility loader for focused helper modules rather than a monolithic utility file.
- `R/00_contracts.R` defines explicit contracts for the canonical wide analysis data, economic cost summaries, MICE wide frames, effectiveness long frames, and CEA patient-level frames.
- `R/00_methods_config.R` is the central source of truth for method choices and rationale notes, including outcome thresholds, imputation rules, effectiveness model family/covariates, CEA cost family, EQ-5D tariffs, intervention cost, WTP threshold, bootstrap counts, and sensitivity settings.
- `R/cost_helpers.R::build_economic_data()` collapses the raw monthly cost panels into half-year `cost_*` summary columns plus explicit cost-source flags. Patients present in any raw cost file are `cost_complete`; absent cost categories are set to zero for those patients, while invalid in-file period values such as F-file `9999` keep the affected half-year summary as `NA` for imputation.
- `R/02_imputation.R` builds one wide MICE frame using the configured imputation settings and writes `data_processed/imputation_artifact.rds` as the canonical imputation artifact. Sensitivity imputation variants now live inside `results/sensitivity_analyses_artifact.rds` when `R/08_sensitivity_analyses.R` is run.
- `R/04b_gee.R` reconstructs each imputation explicitly through the shared effectiveness helper and fits the configured primary effectiveness model.
- `R/04_models.R` reconstructs each imputation explicitly through the same shared helper and fits the configured mixed-effects sensitivity model.
- `R/03_descriptives.R` writes `results/descriptives_artifact.rds`, bundling the analyzed-population baseline table, disease-aware missingness summary, cost summary, and resource-use summary.
- `R/05_cost_effectiveness.R` uses the complex-MICE imputed wide cohort for the configured main CEA branch, runs a nested bootstrap that resamples the same patients across all imputations within each draw, and writes `models/cea_artifact.rds`.
- `R/08_sensitivity_analyses.R` contains the configured secondary effectiveness and CEA sensitivity analyses, including alternate imputation variants, intervention-cost sweeps, and EQ-5D tariff sensitivity.
- `R/06_outputs.R` writes `results/manuscript_outputs_artifact.rds`, and `R/07_manuscript_report.R` writes `results/manuscript_report_artifact.rds`; both fail fast if the canonical upstream artifacts are missing.
- `R/validation_helpers.R` contains reusable fast checks for canonical artifacts, and `R/run_smoke_tests.R` runs the lightweight debugging suite, writing `results/smoke_test_artifact.rds`.
- The CEA acceptability threshold is declared in `R/00_methods_config.R`.
- The CEA ICER point estimate is now computed as pooled incremental cost divided by pooled incremental QALY; the bootstrap ICER interval still uses the iteration-level ratio distribution.
- The manuscript summary labels uncertainty explicitly with `pooled_ci_lower` / `pooled_ci_upper` and `bootstrap_ci_lower` / `bootstrap_ci_upper`.
- `R/05_cost_effectiveness.R` now keeps the complex-MICE wide-imputed CEA branch as the main path; secondary CEA scenarios live in `R/08_sensitivity_analyses.R` and there is no long-form CEA audit path.
- `R/07_manuscript_report.R` writes a human-readable manuscript brief bundle from the current GEE effectiveness artifact and the main CEA artifact, without adding CSV duplicates.
- `R/05_cost_effectiveness.R` now uses the complex-MICE imputed wide cohort for the main CEA branch and does not fit a CEA GEE branch.

---

# Scientific Summary

## Population

Adult asthma and COPD patients recruited through 100 community pharmacies in Sicily, Italy.

Final analysed sample:
- N = 835 patients
- ~49% asthma
- ~51% COPD

Randomisation:
- 2:1 intervention:control allocation

Study timepoints:
- T0 (baseline)
- T3
- T6
- T9
- T12

---

# Intervention

CRC-MUR (Chronic Respiratory Condition Medicines Use Review)

Intervention delivered:
- Immediately after baseline
- Again at 6 months

Format:
- Primarily face-to-face
- Some remote delivery during COVID

Core components:
- Medication review
- Symptom review
- Adherence support
- Healthy lifestyle advice
- Referral/recommendation communication to physicians

Protocol description: :contentReference[oaicite:3]{index=3}

---

# Primary Outcome Logic

Disease control is dichotomised:

## Asthma
Controlled if:
- ACT score >= 20

## COPD
Controlled if:
- CCQ score < 2

Combined binary variable:
- `controlled_*`
- Used as harmonised endpoint across diseases

This is central to all regression analyses.

---

# Core Statistical Approach

## Main effectiveness analysis

Mixed-effects logistic regression:
- Binary outcome: disease controlled
- Random intercept: patient
- Fixed effects:
  - treatment group
  - time
  - interaction(time × treatment)
  - age
  - sex
  - baseline control

Optimizer:
- L-BFGS-B

Implementation referenced in:
- `deprecated/regression_script.R`

Key manuscript description: :contentReference[oaicite:4]{index=4}

---

# Missing Data Strategy

## Outcome data
- Multiple imputation by chained equations (MICE), with exact settings declared in `R/00_methods_config.R`
- Current configured main branch uses arm-split imputation, time-aware predictors to avoid later-to-earlier leakage, and PMM for numeric variables

Variables used:
- sex
- age
- baseline QoL
- prior outcome values

## Cost data
- Included in the main MICE stage as canonical half-year cost summaries
- Cost missingness follows the legacy regression CEA cohort definition: source-present patients are cost-complete even when some categories are absent; absent categories are zero-cost categories, while invalid in-file period values such as medication-file `9999` remain imputation targets
- Secondary cost analyses are isolated in `R/08_sensitivity_analyses.R`

Reason:
- Preserve the ITT cohort for the main analysis while retaining a complete-case robustness check

This distinction is important and repeatedly defended in reviewer responses.

---

# Economic Evaluation

Perspective:
- Italian health system / Sicilian Regional Health Service

Cost categories:
- outpatient
- inpatient
- laboratory
- medication
- delivery

Intervention cost:
- €40 per consultation
- €80 total for intervention patients

QALY calculation:
- Area under EQ-5D-5L curve
- Trapezoidal approximation

Models:
- Gaussian-identity GLM for costs in the main CEA branch, with Gamma-log still available via the helper switch for targeted model comparison
- Gaussian-identity GLM for QALYs

Sensitivity:
- 5000 bootstrap simulations
- Main CEA summaries come from the complex-MICE branch; alternate CEA scenarios are reported separately in `R/08_sensitivity_analyses.R`

---

# R Codebase Structure

## High-level workflow

### 1. Raw data cleaning
File:
- `deprecated/new_data_cleaning_pipe.R`

Purpose:
- Read SPSS datasets
- Recode variables
- Convert structural zeros to NA
- Merge longitudinal datasets
- Detect inconsistencies
- Create analysis-ready dataset

This is effectively the ETL layer.

---

### 2. Main analysis pipeline
File:
- `deprecated/BOFE script_copy.R`

Purpose:
- Original master workflow
- Loads and merges all questionnaires
- Cleans variables
- Creates complete cases
- Handles inconsistencies
- Generates merged datasets

Contains many legacy steps and exploratory procedures.

---

### 3. Regression analyses
File:
- `deprecated/regression_script.R`

Purpose:
- Power calculations
- Longitudinal restructuring
- Mixed-effects models
- QALY construction
- Cost aggregation
- Main inferential analyses

This is the most important script for methodological revision.

---

### 4. Tables and manuscript outputs
File:
- `deprecated/Tables and figures.R`

Purpose:
- Table 1 baseline characteristics
- Outcome summary tables
- Statistical testing
- Difference calculations
- Publication outputs

Includes many manuscript-specific transformations.

---

### 5. Plots and diagnostics
File:
- `deprecated/Plots.R`

Purpose:
- Exploratory plots
- Missingness exploration
- EQindex trajectory checks
- ACT/CCQ histograms
- Controlled vs uncontrolled visualisations

Useful for QC and debugging.

---

# Key Dataset Conventions

## Patient identifiers
- `D1.2` = patient ID
- `D1.1` = pharmacy ID

## Time suffix convention
Variables use suffixes:
- `_0`
- `_3`
- `_6`
- `_9`
- `_12`

Example:
- `ACT.SCORE_0`
- `ACT.SCORE_12`

---

# Important Derived Variables

## Disease type
- `D1.3_0`
  - 1 = asthma
  - 2 = COPD

## Treatment group
- `D1.4_0`
  - `"ig (intervention group)"`
  - `"cg (control group)"`

## Combined disease control
- `controlled_0`
- `controlled_3`
- etc.

## QoL
- `EQindex_*`

## Resource use
Examples:
- `D3.10_1_*` = GP visits
- `D3.10_4_*` = emergency visits
- `D3.10_6_*` = inpatient visits

---

# Known Data Issues

## Structural missingness
Large numbers of structural zeros were converted to `NA`.

This is intentional.

Do NOT reverse without understanding questionnaire logic.

---

## Patient inconsistency
One patient (`OH5A`) had disease recoding inconsistency corrected manually.

See:
- `deprecated/BOFE script_copy.R`

---

## Missing cost data
Economic datasets are incomplete for some patients.

The main cost analysis imputes canonical half-year cost summaries for patients with no raw economic cost-source row and for source-present patients with invalid in-file period values such as medication-file `9999`. Patients present in any raw cost file are treated as cost-complete; absent cost categories are assigned zero before imputation, mirroring the legacy regression CEA cohort and avoiding structural category absence being misclassified as missing data.

---

# Preferred Revision Priorities for Agents

When assisting with revisions, prioritise:

1. Reproducibility
2. Reduction of duplicated code
3. Explicit variable dictionaries
4. Functionalisation
5. Safer joins/merges
6. Model transparency
7. Missingness diagnostics
8. Separation of exploratory vs production code
9. Centralised constants/thresholds
10. Script modularity

---

# Recommended Refactor Architecture

Recommended future structure:

project/
│
├── data_raw/
├── data_processed/
├── R/
│   ├── 00_constants.R
│   ├── 00_contracts.R
│   ├── 01_cleaning.R
│   ├── 02_imputation.R
│   ├── 03_descriptives.R
│   ├── 04_models.R
│   ├── 05_cost_effectiveness.R
│   ├── 06_figures.R
│   ├── *_helpers.R
│   └── utils.R
│
├── models/
├── results/
├── manuscript/
├── AGENTS.md
└── README.md

---

# Common Agent Tasks

## Appropriate tasks
- Refactor repeated code
- Improve regression robustness
- Add comments/documentation
- Improve plotting
- Add reproducibility
- Convert scripts to functions
- Add diagnostics
- Improve naming consistency
- Validate statistical assumptions

## High-risk tasks
Require human confirmation:
- Changing outcome definitions
- Changing imputation strategy
- Altering regression families/link functions
- Modifying disease-control thresholds
- Altering complete-case logic
- Changing QALY methodology

---

# Statistical Notes

## Important modeling decisions
The manuscript evolved from originally planned GEE models to a mixed-effects sensitivity branch during revision work. The current default report path has returned to protocol-style GEE as the primary effectiveness analysis, while mixed-effects logistic regression remains available as a sensitivity analysis.

## Disease-control endpoint
Dichotomisation was chosen to harmonise asthma and COPD outcomes into one binary endpoint.

---

# Manuscript Context

Target journal:
- BJGP (British Journal of General Practice)

The revision files contain:
- reviewer criticisms
- methodological justifications
- wording changes
- unresolved disputes

Useful for understanding why certain analytic choices were retained.

---

# Important Source Files

## Scientific
- Manuscript draft
- Protocol paper
- Reviewer responses

## Analysis
- `deprecated/regression_script.R`
- `deprecated/new_data_cleaning_pipe.R`
- `deprecated/Tables and figures.R`
- `deprecated/Plots.R`

---

# Practical Guidance for Future Agents

Before editing models:
1. Read manuscript Methods section
2. Read reviewer responses
3. Confirm reported estimates
4. Preserve reproducibility

Before editing cleaning:
1. Check whether zeros are structural
2. Preserve time suffix conventions
3. Validate merges carefully

Before editing economics:
1. Preserve intervention costing assumptions
2. Preserve complete-case design unless explicitly changing methodology
3. Validate QALY calculations against manuscript text

For every change in any component, ALWAYS update AGENTS.md context file and README.md file to reflect most up-to-date versions of all files. 

---

# Log of agent activities

Use this space to create a running list of brief summaries of each action taken by the AI agent, as well as the date.

- 2026-05-18: Updated User PATH to prefer Anaconda (python), R 4.4.1 (Rscript), Julia in Program Files, and Rtools; removed old Julia path.
- 2026-05-18: Created R/01_cleaning.R and R/utils.R; applied explicit legacy na_if rules and ran R/01_cleaning.R producing data_processed/all_cases.rds and data_processed/complete_cases.rds.
- 2026-05-18: Created R/02_imputation.R (MICE pipeline scaffold) and set todo 'refactor_imputation' to in_progress.
- 2026-05-18: Saved plan.md to session-state and inserted pipeline todos (translate_pipeline and subtasks) into session DB.
- 2026-05-18: Ran initial validation comparing data_processed outputs with legacy clean_data CSVs; observed schema/shape mismatches (different columns and patient set sizes). Plan: map processed RDS → legacy clean_data formats and add automated validation steps.
- 2026-05-18: Investigated data integrity: found patient PR2B present only in T12 (no baseline) and OH5A with inconsistent disease coding and duplicate rows. Applied fixes in R/01_cleaning.R:
  - Excluded PR2B from all_cases (not ITT-eligible)
  - Corrected OH5A D1.3_6 to 2 (COPD)
  - Removed duplicated rows (kept first occurrence)
  Re-ran R/01_cleaning.R: data_processed/all_cases.rds now N=835 (matches baseline T0), data_processed/complete_cases.rds N=756.
- 2026-05-18: Created R/03_descriptives.R (baseline tables, missingness summaries). Addressed haven_labelled issues by stripping labels before comparisons and fixed missingness_summary aggregation bug. Ran R/03_descriptives.R to produce the analyzed-population Table 1 and the missingness summary.
- 2026-05-18: Checked the original analysis scripts for the ITT branch: regression_script.R uses a complete-case mixed-effects model on df_complete_long, while multiple_imputation.R is the separate imputed workflow that pools glmer fits across 10 imputed datasets. Updated R/04_models.R to use the imputed branch directly without re-deriving outcomes.
- 2026-05-18: Reworked R/02_imputation.R and R/04_models.R to mirror the legacy multiple_imputation.R flow more closely: build the wide imputation frame, split by arm and disease, run PMM per subset, recombine the mids objects, reconstruct the repeated-measures model data, and pool the mixed-effects fits. Also restored the 10th imputation that the legacy script had been dropping during reconstruction.
- 2026-05-18: Validated the rebuilt imputation model stage. Current pooled 12-month intervention-versus-control OR from R/04_models.R is about 1.70 (95% CI about 1.05 to 2.74), still below the manuscript draft's 1.81 (1.14 to 2.87). The pipeline now follows the legacy imputation flow closely and uses all 20 imputations.
- 2026-05-18: Directly edited files (per user request; no pipeline scripts run). Expanded R/utils.R with central constants, primary outcome derivation, EQ-5D index scoring, cost aggregation, long-format construction, CEA patient-level helpers, and model summary helpers. Updated R/01_cleaning.R to derive controlled outcomes/EQindex, attach observed cost summaries, save economic_data.rds, and mark completeness flags. Replaced R/04_models.R placeholder with protocol GEE plus mixed-effects sensitivity model. Added R/05_cost_effectiveness.R and R/06_outputs.R. Updated R/02_imputation.R to exclude cost summaries from MICE.
- 2026-05-18: Centralized structural-zero handling in R/utils.R, removed legacy script scraping from R/01_cleaning.R, increased imputation to 20 repetitions in R/02_imputation.R, refactored R/04_models.R to handle any imputation count, and split the GEE branch into standalone R/04b_gee.R for independent tuning.
- 2026-05-18: Extended R/05_cost_effectiveness.R and R/06_outputs.R to ingest both mixed-effects and GEE model outputs and emit manuscript-ready comparison tables alongside the complete-case CEA summaries.
- 2026-05-18: Added manuscript-facing exports for mixed/GEE effectiveness comparison and CEA summaries: `results/effectiveness_model_comparison.csv`, `results/effectiveness_12mo_comparison.csv`, `results/manuscript_results_summary.csv`, `results/manuscript_results_effectiveness.csv`, and `results/manuscript_results_cea.csv`.
- 2026-05-18: Added parallel interval-level GEE sensitivity models to the CEA stage in R/05_cost_effectiveness.R and updated R/06_outputs.R to export both the CEA model comparison (`manuscript_results_cea.csv`) and the bootstrap summary (`manuscript_results_cea_summary.csv`).
- 2026-05-18: Fixed the GEE directionality so intervention-vs-control ORs match the mixed-effects convention, and refreshed the CEA documentation/output layer to include both GLM and GEE sensitivity summaries.
- 2026-05-18: Archived the legacy scripts into `deprecated/` to keep the active `R/` pipeline focused on the maintained modules.
- 2026-05-18: Reworked the CEA manuscript output so `manuscript_results_cea.csv` is an explicit GLM-vs-GEE comparison table, while `manuscript_results_cea_summary.csv` reports bootstrap summaries for both GLM and GEE CEA branches.
- 2026-05-18: Updated `R/05_cost_effectiveness.R` to bootstrap both GLM and GEE CEA models from the same resampled patients, and updated `R/06_outputs.R` to preserve `model_family` in the manuscript-ready bootstrap summary.
- 2026-05-18: Began re-aligning the CEA GEE branch back onto the same patient-level `group + age + gender` structure as the GLM branch so the two bootstrap families are directly comparable. The current state is code-only and has not yet been revalidated end-to-end after the latest refactor.
- 2026-05-21: Added shared pipeline progress tracking helpers in `R/utils.R`, stage-level console/log reporting to `R/01_cleaning.R` through `R/07_manuscript_report.R`, periodic bootstrap checkpoints in `R/05_cost_effectiveness.R`, and the Windows runner `run_full_pipeline.ps1`.
- 2026-05-21: Refreshed repository docs to describe the current modular pipeline, progress logging, and the `run_full_pipeline.ps1` entrypoint.
- 2026-05-21: Promoted `run_full_pipeline.ps1` to the master pipeline runner and removed the legacy Bash launcher.
- 2026-05-21: Added `R/07_manuscript_report.R` to produce a readable manuscript-facing brief with the key effectiveness and CEA summaries.
- 2026-05-21: Added `R/05_cost_effectiveness_parallel.R` plus a parallel bootstrap path in `R/05_cost_effectiveness.R`, and updated the master runner to use it.
- 2026-05-21: Added `bootstrap_bofe_vm.ps1` to provision R, Rtools, and required CRAN packages on a fresh Windows VM.
- 2026-05-29: Rewrote `R/05_cost_effectiveness.R` to reconstruct the legacy 745-patient cost-complete cohort, fix baseline group assignment for the CEA sample, and bootstrap intervention/control arms separately using GLM-only CEA models.
- 2026-05-29: Added legacy CEA helper functions to `R/utils.R`, aligned `R/06_outputs.R` with the reconstructed cohort, and updated `README.md` to describe the GLM-only legacy-faithful CEA path.
- 2026-05-29: Fixed a vector recycling bug in the legacy CEA long-data helper that had flattened bootstrap costs; reran `R/05_cost_effectiveness.R` and confirmed the incremental cost now varies across bootstrap iterations.
- 2026-05-29: Added verbose per-imputation checkpoints to `R/04_models.R` and switched the mixed-effects optimizer to `bobyqa` to reduce runtime and make the slow step easier to monitor.
- 2026-05-29: Added verbose per-imputation checkpoints to `R/04b_gee.R` and confirmed the GEE branch is much faster than the mixed-effects branch because it avoids random-effects optimization.
- 2026-05-29: Added an opt-in prebuilt long-form CEA/export path using `data_processed/BOFE_data_clean_longform.csv`, plus patient-set audit outputs to make inclusion differences explicit and safe even when only one side of the comparison has differences.
- 2026-05-29: Added pooled CEA uncertainty so each bootstrap row carries model-based variance for incremental cost and QALY, and final summary tables combine within-bootstrap and between-bootstrap variance.
- 2026-05-29: Split generated artifacts into `models/` and `results/`, moved current model/result files out of `outputs/`, and retired the obsolete `outputs/` sink from the active pipeline.
- 2026-06-01: Added an alias dictionary for the wide MICE frame, removed retired long-form CEA helpers, and split the CEA work into a main complex-MICE branch plus a separate sensitivity runner.
- 2026-06-01: Reworked the `basic` MICE sensitivity to match the legacy predictor matrix exactly: baseline age/sex, baseline control, baseline EQindex, and lagged control predictors only.
- 2026-06-02: Rewired the main pipeline so GEE is the default effectiveness analysis, moved `R/04_models.R` to sensitivity-only usage, and factored the shared effectiveness wide-to-long reconstruction into `R/utils.R` so both effectiveness scripts reuse the same helper instead of duplicating the imputation expansion loop.
- 2026-06-02: Split the main full-MICE stage from the sensitivity imputation variants, added reusable helper layers for both the imputation and effectiveness branches, and removed the sensitivity shell-out orchestration in favor of in-process function calls.
- 2026-06-02: Tightened the manuscript reporting path so `R/07_manuscript_report.R` only reads the canonical current GEE and CEA artifacts, removed dead validation bookkeeping from `R/06_outputs.R`, and stripped unused `file_prefix` / local ICER plumbing from the CEA helper layer.
- 2026-06-02: Moved the pipeline progress log out of `results/` into `logs/`, updated the shared log path in `R/utils.R`, and refreshed the repository notes so manuscript-facing outputs stay separate from runtime logs.
- 2026-06-02: Restored the main MI CEA to a nested bootstrap over the same sampled patients in every imputation, moved imputation audit outputs into `audit/`, and refreshed the manuscript brief/results so `cea_bootstrap_results.csv` again contains 5000 bootstrap rows.
- 2026-06-02: Updated `R/03_descriptives.R` so Table 1 now includes disease-aware missingness columns sourced from the ITT data, while ACT and CCQ missingness are counted only on their applicable disease subsets.
- 2026-06-02: Simplified `build_economic_data()` so it now returns only the patient identifier plus the half-year `cost_*` summary columns; the raw monthly cost panels stay in the raw files and are only used to build those summaries.
- 2026-06-02: Renamed the bootstrap cost coefficient field in `R/05_cost_effectiveness_helpers.R` from `cost_group_log_ratio` to `cost_group_effect` so the Gaussian main branch output is not mislabeled.
- 2026-06-02: Red-team refactor of the active pipeline: sorted effectiveness long data by patient/time, restricted effectiveness models to follow-up observations with baseline control as a covariate, prevented cost summaries from predicting non-cost MICE targets, made simple imputation preserve EQ-5D item levels, removed retired cost helper paths and the stale parallel CEA wrapper, tightened complete-cost handling, removed QALY truncation, and aligned CEA sensitivity branches around the Gaussian-identity cost model.
- 2026-06-02: Optimized the nested MI CEA bootstrap so it resamples already-prepared patient-level CEA frames across imputations instead of reconstructing patient-level costs/QALYs from the wide completed datasets inside every bootstrap iteration; a 3-iteration smoke run passed.
- 2026-06-02: Added explicit pipeline data contracts in `R/00_contracts.R`, split the former monolithic `R/utils.R` into focused helper modules, kept `R/utils.R` as a compatibility loader, wired contract assertions into cleaning, imputation-frame construction, economic summaries, effectiveness long reconstruction, and CEA patient-level construction, and validated the split with parsing, helper-load, cleaning, imputation-frame, effectiveness-long, CEA patient-level, and 2-iteration nested CEA smoke checks.
- 2026-06-02: Refactored active stages so each script writes one canonical RDS artifact: cleaning, imputation, descriptives, GEE effectiveness, mixed-effectiveness sensitivity, main CEA, manuscript outputs, manuscript report, and sensitivity analyses. Removed direct script writes to legacy per-table CSV/RDS sidecars and updated downstream reads to use canonical artifacts.
- 2026-06-02: Added fast validation helpers plus `R/run_smoke_tests.R`; the smoke runner refreshes cleaning, validates canonical contracts, builds the imputation frame without running MICE, validates existing downstream artifacts, and writes `results/smoke_test_artifact.rds`. The current smoke pass completed with 10 passed checks and 6 skipped downstream checks because the expensive imputation/model artifacts have not yet been regenerated.
- 2026-06-02: Added `R/00_methods_config.R` as the central method-configuration file and moved hard-coded method choices/rationale notes into it, including outcome thresholds, structural-zero/manual-cleaning rules, imputation methods/seeds, effectiveness model formulas/defaults, CEA cost family/tariffs/bootstrap settings, environment override names, and validation thresholds. Updated constants/scripts/helpers to derive those values from the config.
- 2026-06-03: Restored and refined the legacy cost-completeness rule in `R/cost_helpers.R` and `R/00_methods_config.R`: patients present in any raw cost file are cost-complete, absent cost categories are zero-filled, patients absent from every raw cost file retain missing cost summaries, and invalid in-file period values such as medication-file `9999` remain missing for imputation. Re-ran `R/01_cleaning.R` and `R/02_imputation.R`; all_cases has 822 cost-complete patients, 13 no-source patients, 28 source-present period-summary cells still requiring imputation, and 158 cost-summary cells to impute overall. `R/run_smoke_tests.R` passed with 30 checks and 1 optional sensitivity skip.
- 2026-06-03: Audited `data_processed/`, `models/`, and `results/` against the canonical artifact map. Removed 44 stale untracked sidecar artifacts plus 5 downstream canonical artifacts generated before the cost/imputation correction, then restored an explicit export layer so fresh stage reruns produce necessary CSV/Markdown deliverables for plots, tables, CEA planes, acceptability curves, and sensitivity comparisons. The rule is now: canonical RDS artifacts are the source of truth, while named CSV/Markdown exports generated from current artifacts are expected deliverables, not clutter.

Next steps:
1. Re-run `source("R/02_imputation.R")` and confirm `data_processed/imputation_artifact.rds` contains the full MICE object, missingness report, predictor audit, diagnostics, and first completed dataset.
2. Run `R/08_sensitivity_analyses.R` and confirm it creates `results/sensitivity_analyses_artifact.rds` with the basic/simple imputation sensitivity objects and the secondary CEA/effectiveness summaries.
3. Re-run `R/05_cost_effectiveness.R` on its own first and confirm `models/cea_artifact.rds` contains the complex-MICE main CEA branch.
4. If that succeeds, run the full pipeline in order via `.\run_full_pipeline.ps1`.
5. Run `Rscript R/run_smoke_tests.R` before and after expensive stages to catch shape or artifact regressions quickly.
6. Then compare `results/manuscript_outputs_artifact.rds` and `results/manuscript_report_artifact.rds` against the manuscript draft.

## Handoff summary for next agent

Summary: The modular pipeline now has cleaning derivations, descriptives, follow-up-only GEE/mixed-effects models, a complex-MICE CEA main branch, and a separate `R/08_sensitivity_analyses.R` runner for alternate imputation and CEA scenarios. The former monolithic `R/utils.R` has been split into focused helper modules, `R/00_contracts.R` defines/enforces data-shape contracts, and `R/00_methods_config.R` is the source of truth for method choices. Each active stage writes one canonical RDS artifact, and downstream scripts read those artifacts instead of legacy per-table CSV/RDS sidecars. The nested MI CEA bootstrap now resamples prepared patient-level CEA frames rather than rebuilding them inside each draw. Targeted syntax, helper-load, cleaning, descriptives, imputation-frame, and small CEA interface checks passed; the full 5000-bootstrap pipeline still needs to be rerun before manuscript outputs are used.

Important manual fixes applied in R/01_cleaning.R:
- Excluded patient PR2B (only appears at T12; not in baseline)
- Corrected OH5A disease coding to COPD (`D1.3` and `D1.3_6`)
- Removed duplicate rows (kept first occurrence)

Immediate next tasks (priority order):
1. Re-run the downstream main pipeline from the fresh cleaning/imputation artifacts: `source("R/03_descriptives.R")`, `source("R/04b_gee.R")`, `source("R/05_cost_effectiveness.R")`, `source("R/06_outputs.R")`, and `source("R/07_manuscript_report.R")`; or run `.\run_full_pipeline.ps1` if you want to refresh everything.
2. Inspect `readRDS("data_processed/imputation_artifact.rds")$full_predictor_audit` to confirm cost summaries are imputed but do not predict non-cost targets.
3. Inspect `models/cea_artifact.rds` and `results/manuscript_outputs_artifact.rds` to confirm the regenerated main CEA artifacts show `GLM_Gaussian_identity_cost` and `cost_group_effect`.
4. Run `source("R/08_sensitivity_analyses.R")` after the main artifacts are fresh, then inspect `results/sensitivity_analyses_artifact.rds`.
5. Compare regenerated `clean_data/*_from_pipeline.csv` files with legacy CSVs before replacing manuscript values.

Where to look:
- Legacy reference scripts: `deprecated/BOFE script_copy.R`, `deprecated/new_data_cleaning_pipe.R`, `deprecated/regression_script.R`
- Plan & tracking: session-state plan.md and session DB todos
- Outputs & data: data_processed/, models/, results/, audit/, and logs/

Notes: Preserve outcome definitions, imputation strategies, and complete-case rules unless explicit sign-off is provided. Contact the original author for domain questions or ambiguous manual corrections.
When a method choice changes, update `R/00_methods_config.R` first and only then update code if a new implementation path is required.

---

# Citations

Protocol publication:
Manfrin et al., 2021

Main manuscript:
BJGP revision draft (2025)

Key protocol details: :contentReference[oaicite:5]{index=5}  
Primary manuscript analysis details: :contentReference[oaicite:6]{index=6}
