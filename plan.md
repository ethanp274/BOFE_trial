# Plan: Consolidate and Refactor R Scripts

Problem statement
- The repository contains multiple overlapping R scripts (legacy copies and per-dataset variations) that duplicate functionality. This makes maintenance and reproducibility harder.

Goal
- Merge duplicate/prior-version R files while preserving all current operations and outputs, remove unnecessary repetition, and produce a modular R/ folder with clear responsibilities.

Proposed approach
1. Inventory all R scripts and identify overlapping functionality (cleaning, imputation, models, tables/figures, plots).
2. Create a target modular layout under R/:
   - 01_cleaning.R (data ingestion + cleaning)
   - 02_imputation.R (MICE workflows)
   - 03_descriptives.R (tables/summary)
   - 04_models.R (regression models)
   - 05_cost_effectiveness.R (economic analyses)
   - 06_figures.R (plotting)
   - utils.R (shared functions/constants)
3. Move/refactor code into modules, keeping behavior unchanged. Add unit checks and small example runs to validate outputs match current results.
4. Remove legacy duplicates after verification and update README/AGENTS.md with mapping.

Todos (high-level)
- analyze and map existing scripts to target modules - done
- implement refactor for cleaning and extraction of utils - done
- consolidate regression/analysis code into 04_models.R - done
- consolidate cost-effectiveness code into 05_cost_effectiveness.R - done
- consolidate legacy output mapping and validation into 06_outputs.R - done
- validate outputs and remove duplicates - in progress
- update documentation (README, AGENTS.md) - done

Notes & considerations
- Preserve data conventions (time suffixes, patient IDs).
- Do not change outcome definitions, imputation strategies, or complete-case logic without explicit confirmation.
- Keep commits small and verifiable; include tests or checks comparing key result numbers pre/post change.

Next steps
- Run the modular scripts from the IDE in order:
  1. `source("R/01_cleaning.R")`
  2. `source("R/02_imputation.R")`
  3. `source("R/03_descriptives.R")`
  4. `source("R/04_models.R")`
  5. `source("R/04b_gee.R")`
  6. `source("R/05_cost_effectiveness.R")`
  7. `source("R/06_outputs.R")`
- First validate the revised CEA branch on its own, because it is being re-aligned to the same patient-level covariate structure as the GLM branch.
- Inspect `outputs/cea_model_summaries.csv`, `outputs/cea_bootstrap_results.csv`, `outputs/manuscript_results_cea.csv`, and `outputs/manuscript_results_cea_summary.csv` after the next run.
- Then inspect `outputs/pipeline_validation_summary.csv`, `outputs/manuscript_results_summary.csv`, and `outputs/manuscript_results_effectiveness.csv`.
- Do not remove legacy duplicate scripts until regenerated outputs are validated against manuscript/legacy values.
