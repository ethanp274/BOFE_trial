#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$repo_root"

steps=(
  "R/01_cleaning.R"
  "R/02_imputation.R"
  "R/03_descriptives.R"
  "R/04_models.R"
  "R/04b_gee.R"
  "R/05_cost_effectiveness.R"
  "R/06_outputs.R"
)

for step in "${steps[@]}"; do
  echo "Running ${step}..."
  Rscript "$step"
done

echo "Pipeline complete."
