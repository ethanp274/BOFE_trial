# R/run_smoke_tests.R
# Tiny smoke-test runner for fast BOFE pipeline debugging.
#
# Defaults:
#   - runs R/01_cleaning.R because it is fast and establishes the canonical input
#   - builds the imputation frame but does not run MICE
#   - validates any existing canonical downstream artifacts
#   - runs a tiny nested CEA bootstrap only if data_processed/imputation_artifact.rds exists
#
# Environment toggle names are declared in R/00_methods_config.R:
#   BOFE_SMOKE_RUN_CLEANING=0      skip the cleaning stage run
#   BOFE_SMOKE_SKIP_CEA=1          skip the tiny CEA bootstrap smoke
#   BOFE_SMOKE_CEA_BOOTSTRAPS=2    change the tiny CEA bootstrap iteration count

source("R/02_imputation_helpers.R")
source("R/05_cost_effectiveness_helpers.R")

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "smoke_tests",
  "running fast validation checks"
)

run_cleaning <- Sys.getenv(method_config("environment_overrides", "smoke_run_cleaning"), "1") != "0"
skip_cea <- Sys.getenv(method_config("environment_overrides", "smoke_skip_cea"), "0") == "1"
cea_bootstraps <- suppressWarnings(as.integer(Sys.getenv(
  method_config("environment_overrides", "smoke_cea_bootstraps"),
  as.character(method_config("validation", "smoke_default_cea_bootstraps"))
)))
if (!is.finite(cea_bootstraps) || cea_bootstraps < 1L) {
  cea_bootstraps <- method_config("validation", "smoke_default_cea_bootstraps")
}

checks <- combine_validation_reports()
add_checks <- function(report) {
  checks <<- combine_validation_reports(checks, report)
  invisible(TRUE)
}

pipeline_phase_info("smoke_tests", "checking canonical cleaning stage")
if (isTRUE(run_cleaning)) {
  add_checks(run_validation_check("smoke: run cleaning stage", {
    source("R/01_cleaning.R")
    "R/01_cleaning.R completed"
  }))
} else {
  add_checks(skip_validation_check("smoke: run cleaning stage", "skipped by BOFE_SMOKE_RUN_CLEANING=0"))
}

if (file.exists(canonical_artifact_path("cleaning"))) {
  cleaning_artifact <- read_canonical_artifact("cleaning")
  add_checks(validate_cleaning_artifact(cleaning_artifact))

  pipeline_phase_info("smoke_tests", "building imputation frame without running MICE")
  df_impute <- NULL
  add_checks(run_validation_check("smoke: build imputation frame", {
    df_impute <<- build_imputation_wide_frame(cleaning_artifact$all_cases)
    paste(nrow(df_impute), "rows and", ncol(df_impute), "columns")
  }))
  if (!is.null(df_impute)) {
    add_checks(validate_imputation_frame(df_impute))
  }
} else {
  add_checks(run_validation_check("smoke: cleaning artifact exists", {
    stop("missing ", canonical_artifact_path("cleaning"))
  }))
}

pipeline_phase_info("smoke_tests", "checking optional canonical artifacts")
if (file.exists(canonical_artifact_path("imputation"))) {
  imputation_artifact <- read_canonical_artifact("imputation")
  add_checks(validate_imputation_artifact(imputation_artifact))

  if (!isTRUE(skip_cea)) {
    cea_smoke <- NULL
    add_checks(run_validation_check("smoke: tiny nested CEA bootstrap", {
      cea_mids <- if ("cea_mids" %in% names(imputation_artifact)) {
        imputation_artifact$cea_mids
      } else {
        imputation_artifact$full_mids
      }
      cea_smoke <<- run_nested_mi_cea_branch(
        mids_obj = cea_mids,
        branch_label = "smoke_cea",
        bootstrap_iterations = cea_bootstraps,
        cost_family = method_config("economics", "main_cost_family")
      )
      paste(nrow(cea_smoke$summary), "summary rows from", cea_bootstraps, "bootstrap iteration(s)")
    }))
    if (!is.null(cea_smoke)) {
      add_checks(validate_cea_artifact(cea_smoke, check_prefix = "smoke CEA"))
    }
  } else {
    add_checks(skip_validation_check("smoke: tiny nested CEA bootstrap", "skipped by BOFE_SMOKE_SKIP_CEA=1"))
  }
} else {
  add_checks(skip_validation_check("imputation artifact", paste("missing", canonical_artifact_path("imputation"))))
  add_checks(skip_validation_check("smoke: tiny nested CEA bootstrap", "requires canonical imputation artifact"))
}

optional_checks <- list(
  descriptives = validate_descriptives_artifact,
  effectiveness_gee = validate_effectiveness_gee_artifact,
  cea = validate_cea_artifact,
  manuscript_outputs = validate_manuscript_outputs_artifact,
  sensitivity = validate_sensitivity_artifact
)

for (artifact_name in names(optional_checks)) {
  path <- canonical_artifact_path(artifact_name)
  if (file.exists(path)) {
    add_checks(optional_checks[[artifact_name]](read_canonical_artifact(artifact_name)))
  } else {
    add_checks(skip_validation_check(paste0(artifact_name, " artifact"), paste("missing", path)))
  }
}

summary_table <- validation_summary(checks)
write_canonical_artifact(
  "smoke_tests",
  list(
    stage = "smoke_tests",
    checks = checks,
    summary = summary_table
  )
)

cat("\n=== Smoke-Test Summary ===\n")
print(summary_table, row.names = FALSE)
cat("\n=== Smoke-Test Checks ===\n")
print(checks[, c("status", "check", "detail")], row.names = FALSE)

if (validation_has_failures(checks)) {
  pipeline_phase_end(
    "smoke_tests",
    pipeline_started,
    "smoke tests failed"
  )
  stop("Smoke tests failed. Inspect ", canonical_artifact_path("smoke_tests"), ".")
}

pipeline_phase_end(
  "smoke_tests",
  pipeline_started,
  "smoke tests passed"
)
