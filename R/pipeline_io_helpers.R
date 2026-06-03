# R/pipeline_io_helpers.R
# Artifact paths and pipeline progress logging helpers.

ensure_artifact_dirs <- function() {
  dir.create(DATA_PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(MODELS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(AUDIT_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(LOGS_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(CLEAN_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

result_path <- function(...) {
  file.path(RESULTS_DIR, ...)
}

model_path <- function(...) {
  file.path(MODELS_DIR, ...)
}

audit_path <- function(...) {
  file.path(AUDIT_DIR, ...)
}

canonical_artifact_path <- function(name) {
  paths <- list(
    cleaning = file.path(DATA_PROCESSED_DIR, "cleaning_artifact.rds"),
    imputation = file.path(DATA_PROCESSED_DIR, "imputation_artifact.rds"),
    descriptives = result_path("descriptives_artifact.rds"),
    effectiveness_gee = model_path("effectiveness_gee_artifact.rds"),
    effectiveness_mixed = model_path("effectiveness_mixed_artifact.rds"),
    cea = model_path("cea_artifact.rds"),
    manuscript_outputs = result_path("manuscript_outputs_artifact.rds"),
    manuscript_report = result_path("manuscript_report_artifact.rds"),
    sensitivity = result_path("sensitivity_analyses_artifact.rds"),
    smoke_tests = result_path("smoke_test_artifact.rds")
  )
  if (!name %in% names(paths)) {
    stop(
      "canonical_artifact_path: unknown artifact '", name, "'. Available artifacts: ",
      paste(names(paths), collapse = ", ")
    )
  }
  unname(paths[[name]])
}

write_canonical_artifact <- function(name, artifact) {
  ensure_artifact_dirs()
  if (!is.list(artifact)) {
    artifact <- list(value = artifact)
  }
  artifact$artifact_name <- name
  artifact$created_at <- Sys.time()
  path <- canonical_artifact_path(name)
  saveRDS(artifact, file = path)
  invisible(path)
}

write_result_csv <- function(x, filename) {
  ensure_artifact_dirs()
  path <- result_path(filename)
  write.csv(x, path, row.names = FALSE)
  invisible(path)
}

write_result_text <- function(lines, filename) {
  ensure_artifact_dirs()
  path <- result_path(filename)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

read_canonical_artifact <- function(name) {
  path <- canonical_artifact_path(name)
  if (!file.exists(path)) {
    stop("Missing canonical artifact ", path, ". Run the corresponding pipeline stage first.")
  }
  readRDS(path)
}

pipeline_log_line <- function(phase, status, detail = NULL, elapsed = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pieces <- c(
    sprintf("[%s]", timestamp),
    sprintf("[%s]", toupper(status)),
    phase
  )
  if (!is.null(detail) && nzchar(detail)) {
    pieces <- c(pieces, "-", detail)
  }
  if (!is.null(elapsed) && is.finite(elapsed)) {
    pieces <- c(pieces, sprintf("(elapsed %.1fs)", elapsed))
  }
  line <- paste(pieces, collapse = " ")
  cat(line, "\n")
  try(flush.console(), silent = TRUE)

  if (!is.null(log_file) && nzchar(log_file)) {
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    cat(line, "\n", file = log_file, append = TRUE)
  }

  invisible(line)
}

pipeline_phase_start <- function(phase, detail = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  pipeline_log_line(phase, "start", detail = detail, log_file = log_file)
  Sys.time()
}

pipeline_phase_info <- function(phase, detail, log_file = PIPELINE_PROGRESS_LOG) {
  pipeline_log_line(phase, "info", detail = detail, log_file = log_file)
}

pipeline_phase_end <- function(phase, started_at = NULL, detail = NULL, log_file = PIPELINE_PROGRESS_LOG) {
  elapsed <- if (!is.null(started_at)) as.numeric(difftime(Sys.time(), started_at, units = "secs")) else NA_real_
  pipeline_log_line(phase, "done", detail = detail, elapsed = elapsed, log_file = log_file)
}
