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
- analyze and map existing scripts to target modules
- implement refactor for cleaning and extraction of utils
- consolidate regression/analysis code into 04_models.R
- consolidate tables/figures and plotting into 03/06
- validate outputs and remove duplicates
- update documentation (README, AGENTS.md)

Notes & considerations
- Preserve data conventions (time suffixes, patient IDs).
- Do not change outcome definitions, imputation strategies, or complete-case logic without explicit confirmation.
- Keep commits small and verifiable; include tests or checks comparing key result numbers pre/post change.

Next steps
- Start by running a repository inventory (list R files and size) and mapping each file to the target modules.
- After user approval, begin implementing the first merge (cleaning scripts).
