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
- `regression_script.R`

Key manuscript description: :contentReference[oaicite:4]{index=4}

---

# Missing Data Strategy

## Outcome data
- Multiple imputation by chained equations (MICE)
- 10 imputations
- Predictive mean matching

Variables used:
- sex
- age
- baseline QoL
- prior outcome values

## Cost data
- NOT imputed
- Complete-case analysis only

Reason:
- Poor predictiveness for second-half resource utilisation

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
- Gamma-log GLM for costs
- Gaussian-identity GLM for QALYs

Sensitivity:
- 5000 bootstrap simulations

---

# R Codebase Structure

## High-level workflow

### 1. Raw data cleaning
File:
- `new_data_cleaning_pipe.R`

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
- `BOFE script_copy.R`

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
- `regression_script.R`

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
- `Tables and figures.R`

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
- `Plots.R`

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
- `BOFE script_copy.R`

---

## Missing cost data
Economic datasets are incomplete for some patients.

Cost analysis intentionally uses complete-case design.

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
│   ├── 01_cleaning.R
│   ├── 02_imputation.R
│   ├── 03_descriptives.R
│   ├── 04_models.R
│   ├── 05_cost_effectiveness.R
│   ├── 06_figures.R
│   └── utils.R
│
├── outputs/
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
The manuscript evolved from:
- originally planned GEE models
to:
- mixed-effects logistic regression

Reviewer responses discuss this transition. For the revision, we now plan on using GEE models again, per the protocol. Mixed-effect logistic regressions can still be included or may be moved to appendix, depending on results.

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
- `regression_script.R`
- `new_data_cleaning_pipe.R`
- `Tables and figures.R`
- `Plots.R`

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
- 2026-05-18: Created R/03_descriptives.R (baseline tables, missingness summaries). Addressed haven_labelled issues by stripping labels before comparisons and fixed missingness_summary aggregation bug. Ran R/03_descriptives.R to produce:
  - outputs/table1_all_cases_characteristics.csv (ITT, N=835)
  - outputs/table1_complete_cases_characteristics.csv (Analyzed, N=756)
  - outputs/missingness_summary.csv
  - outputs/summary_by_disease.csv

Next steps: continue pipeline with R/04_models.R, R/05_cost_effectiveness.R, and R/06_outputs.R. Update todos and plan.md accordingly.

Next steps: continue implementing the modular pipeline (imputation, descriptives, models, costs, outputs) and add automated validation. Document manual corrections (e.g., OH5A) in 01_cleaning.R and AGENTS.md.

## Handoff summary for next agent

Summary: The cleaning and initial descriptives stages are complete. Key files changed: R/01_cleaning.R (manual fixes), R/utils.R, R/03_descriptives.R, R/04_models.R (placeholder). Outputs produced: data_processed/all_cases.rds (ITT, N=835), data_processed/complete_cases.rds (analyzed, N=756), outputs/table1_all_cases_characteristics.csv, outputs/table1_complete_cases_characteristics.csv, outputs/missingness_summary.csv, outputs/summary_by_disease.csv.

Important manual fixes applied in R/01_cleaning.R:
- Excluded patient PR2B (only appears at T12; not in baseline)
- Corrected OH5A D1.3_6 -> 2 (COPD)
- Removed duplicate rows (kept first occurrence)

Immediate next tasks (priority order):
1. Finalize model formulas in R/04_models.R using regression_script.R and run GEE (ITT) and mixed-effects models; save results to outputs/
2. Implement R/05_cost_effectiveness.R (complete-case cost analyses, QALYs, bootstraps)
3. Implement R/06_outputs.R to map data_processed RDS -> legacy clean_data CSVs and add automated validation tests
4. Resume R/02_imputation.R after pipeline validated

Where to look:
- Legacy reference scripts: BOFE script_copy.R, new_data_cleaning_pipe.R, regression_script.R
- Plan & tracking: session-state plan.md and session DB todos
- Outputs & data: data_processed/ and outputs/

Notes: Preserve outcome definitions, imputation strategies, and complete-case rules unless explicit sign-off is provided. Contact the original author for domain questions or ambiguous manual corrections.

---

# Citations

Protocol publication:
Manfrin et al., 2021

Main manuscript:
BJGP revision draft (2025)

Key protocol details: :contentReference[oaicite:5]{index=5}  
Primary manuscript analysis details: :contentReference[oaicite:6]{index=6}