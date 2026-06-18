# BOFE MICE predictor-matrix audit protocol

Purpose: assess the primary effectiveness MICE predictor matrix without selecting variables because they strengthen or weaken the treatment effect.

Decision rules:
1. Hard exclusions: patient ID, randomised group inside arm-split MICE, future-time predictors, role-disallowed variables, and predictors with no observed data.
2. Protected anchors: variables in the primary analysis model and prior disease-control history should not be removed solely because their empirical association is weak.
3. Candidate additions: variables suggested by quickpred are sensitivity candidates only after removing future-time and role-disallowed links.
4. Candidate removals: current non-anchor links with minimal or non-estimable value/missingness association are tested only as pre-specified sensitivity variants.
5. Treatment-effect results from candidate matrices should be summarized as robustness checks, not used to choose the primary matrix.

Recommended stress tests:
- Static matrix audit: confirm zero future links and explain every current link by role, time, missingness, and empirical association.
- Overimputation check: mask an observed subset of target outcomes within arm and timepoint, impute under each pre-specified matrix, and compare calibration/error without looking at treatment-effect direction.
- Primary-result stability check: run the GEE under current_analytic, no_utility_index_auxiliaries, no_same_visit_auxiliaries, history_only_controlled, and sanitized_quickpred; report the 12-month OR range transparently.
- Sensitivity conclusion rule: prefer the current rule-based matrix unless an alternative shows clear diagnostic failure of the current matrix and remains clinically/methodologically defensible.

Generated files:
- audit/imputation_baseline_predictor_presence_effectiveness.csv
- audit/imputation_predictor_pair_scores_effectiveness.csv
- audit/imputation_predictor_matrix_decisions_effectiveness.csv
- audit/imputation_predictor_matrix_variant_summary.csv
