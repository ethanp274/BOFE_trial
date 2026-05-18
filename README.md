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

### `new_data_cleaning_pipe.R`
Primary data-cleaning and reconstruction pipeline.

Responsibilities:
- Read raw SPSS datasets
- Clean questionnaire data
- Convert structural zeros to NA
- Merge longitudinal datasets
- Prepare analysis-ready data

Recommended entry point for understanding the data structure.

---

### `regression_script.R`
Primary inferential analysis script.

Responsibilities:
- Power calculations
- Longitudinal reshaping
- Mixed-effects logistic regression
- Cost-effectiveness modelling
- QALY construction

Most important file for statistical revisions.

---

### `Tables and figures.R`
Publication table generation.

Responsibilities:
- Baseline characteristics
- Outcome tables
- Statistical summaries
- Controlled/uncontrolled analyses

Produces manuscript-ready outputs.

---

### `Plots.R`
Exploratory analysis and visualisation.

Responsibilities:
- Diagnostic plots
- Missingness exploration
- EQindex trajectory inspection
- ACT/CCQ visualisations

Useful for QA and exploratory work.

---

### `BOFE script_copy.R`
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
- `D1.4_0`
  - intervention
  - control

## Disease
- `D1.3_0`
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
- 10 imputations

Cost data:
- Complete-case only
- No imputation

## Primary Model
Mixed-effects logistic regression:
- Random intercept for patient
- Fixed effects:
  - treatment
  - time
  - treatment × time
  - age
  - sex
  - baseline control

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

Scripts intentionally convert these to `NA`.

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