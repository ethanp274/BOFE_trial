###########################################################################
# R/07_manuscript_report.R
# Purpose: Produce a manuscript-facing results brief that is easy to scan.
# Inputs:
#   - outputs/models_mixed_imputed.rds
#   - outputs/models_gee_imputed.rds
#   - outputs/cea_models.rds
#   - outputs/manuscript_results_effectiveness.csv
#   - outputs/manuscript_results_cea_summary.csv
#   - outputs/cea_model_summaries.csv
#   - outputs/cea_bootstrap_results.csv
# Outputs:
#   - outputs/manuscript_results_brief.md
#   - outputs/manuscript_results_brief.txt
#   - outputs/manuscript_results_overview.csv
###########################################################################

source("R/utils.R")

library(dplyr)

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

pipeline_started <- pipeline_phase_start(
  "07_manuscript_report",
  "assembling a readable manuscript results brief"
)

safe_read_rds <- function(path) {
  if (file.exists(path)) readRDS(path) else NULL
}

safe_read_csv <- function(path) {
  if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else NULL
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_ci <- function(est, low, high, digits = 3) {
  paste0(fmt_num(est, digits), " [", fmt_num(low, digits), ", ", fmt_num(high, digits), "]")
}

pick_family_row <- function(df, family_values) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  for (fam in family_values) {
    row <- df[df$model %in% fam, , drop = FALSE]
    if (nrow(row) > 0) return(row[1, , drop = FALSE])
  }
  NULL
}

normalize_effectiveness <- function(df, model_label) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  out <- df
  if (!"model" %in% names(out)) out$model <- model_label
  if (!"analysis" %in% names(out)) out$analysis <- "effectiveness"
  if (!"log_or" %in% names(out) && "odds_ratio" %in% names(out)) {
    out$log_or <- log(out$odds_ratio)
  }
  out
}

load_effectiveness <- function() {
  mixed_rds <- safe_read_rds("outputs/models_mixed_imputed.rds")
  gee_rds <- safe_read_rds("outputs/models_gee_imputed.rds")
  mixed_df <- normalize_effectiveness(
    if (!is.null(mixed_rds)) mixed_rds$timepoint_effects else NULL,
    "mixed_effects"
  )
  gee_df <- normalize_effectiveness(
    if (!is.null(gee_rds)) gee_rds$gee_timepoint_effects else NULL,
    "gee"
  )

  combined <- bind_rows(mixed_df, gee_df)
  if (nrow(combined) == 0) {
    combined <- safe_read_csv("outputs/manuscript_results_effectiveness.csv")
  }

  combined
}

load_cea_summary <- function() {
  cea_rds <- safe_read_rds("outputs/cea_models.rds")
  summary_df <- NULL
  comparison_df <- NULL
  bootstrap_df <- NULL
  accept_df <- NULL
  model_terms_df <- NULL

  if (!is.null(cea_rds)) {
    summary_df <- cea_rds$summary
    comparison_df <- cea_rds$cea_model_comparison
    bootstrap_df <- cea_rds$bootstrap_results
    accept_df <- cea_rds$acceptability_curve
    model_terms_df <- cea_rds$model_summaries
  }

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    summary_df <- safe_read_csv("outputs/cea_summary.csv")
  }
  if (is.null(comparison_df) || nrow(comparison_df) == 0) {
    comparison_df <- safe_read_csv("outputs/cea_model_comparison.csv")
  }
  if (is.null(bootstrap_df) || nrow(bootstrap_df) == 0) {
    bootstrap_df <- safe_read_csv("outputs/cea_bootstrap_results.csv")
  }
  if (is.null(accept_df) || nrow(accept_df) == 0) {
    accept_df <- safe_read_csv("outputs/cea_acceptability_curve.csv")
  }
  if (is.null(model_terms_df) || nrow(model_terms_df) == 0) {
    model_terms_df <- safe_read_csv("outputs/cea_model_summaries.csv")
  }

  list(
    summary = summary_df,
    comparison = comparison_df,
    bootstrap = bootstrap_df,
    acceptability = accept_df,
    model_terms = model_terms_df
  )
}

effectiveness <- load_effectiveness()
cea <- load_cea_summary()

if (is.null(effectiveness) || nrow(effectiveness) == 0) {
  stop("No effectiveness results were found in outputs/.")
}

effectiveness <- effectiveness %>%
  mutate(
    model = ifelse(model %in% c("mixed", "mixed_effects", "GLMM", "glmm"), "mixed_effects", model),
    model = ifelse(model %in% c("gee", "GEE"), "gee", model)
  )

timepoint_table <- effectiveness %>%
  arrange(time, model) %>%
  select(model, time, odds_ratio, ci_low, ci_high, n_imputations)

timepoint_12 <- timepoint_table %>% filter(time == 12)
mixed_12 <- pick_family_row(timepoint_12, c("mixed_effects"))
gee_12 <- pick_family_row(timepoint_12, c("gee"))

effectiveness_overview <- data.frame(
  section = c("effectiveness", "effectiveness", "effectiveness", "effectiveness"),
  item = c("mixed_effects_12mo_or", "gee_12mo_or", "or_difference_mixed_minus_gee", "or_ratio_mixed_over_gee"),
  estimate = c(
    if (!is.null(mixed_12)) mixed_12$odds_ratio else NA_real_,
    if (!is.null(gee_12)) gee_12$odds_ratio else NA_real_,
    if (!is.null(mixed_12) && !is.null(gee_12)) mixed_12$odds_ratio - gee_12$odds_ratio else NA_real_,
    if (!is.null(mixed_12) && !is.null(gee_12)) mixed_12$odds_ratio / gee_12$odds_ratio else NA_real_
  ),
  lower_95 = c(
    if (!is.null(mixed_12)) mixed_12$ci_low else NA_real_,
    if (!is.null(gee_12)) gee_12$ci_low else NA_real_,
    NA_real_,
    NA_real_
  ),
  upper_95 = c(
    if (!is.null(mixed_12)) mixed_12$ci_high else NA_real_,
    if (!is.null(gee_12)) gee_12$ci_high else NA_real_,
    NA_real_,
    NA_real_
  ),
  note = c(
    "Primary mixed-effects 12-month estimate",
    "Protocol-style GEE 12-month estimate",
    "Difference in odds ratios",
    "Ratio of odds ratios"
  ),
  stringsAsFactors = FALSE
)

add_cea_family_summary <- function(bootstrap_df, summary_df) {
  if (!is.null(bootstrap_df) && nrow(bootstrap_df) > 0 && "model_family" %in% names(bootstrap_df)) {
    out <- bootstrap_df %>%
      group_by(model_family) %>%
      summarise(
        n_boot = n(),
        incremental_cost = mean(incremental_cost, na.rm = TRUE),
        cost_low = quantile(incremental_cost, 0.025, na.rm = TRUE),
        cost_high = quantile(incremental_cost, 0.975, na.rm = TRUE),
        incremental_qaly = mean(incremental_qaly, na.rm = TRUE),
        qaly_low = quantile(incremental_qaly, 0.025, na.rm = TRUE),
        qaly_high = quantile(incremental_qaly, 0.975, na.rm = TRUE),
        icer = mean(icer, na.rm = TRUE),
        icer_low = quantile(icer, 0.025, na.rm = TRUE),
        icer_high = quantile(icer, 0.975, na.rm = TRUE),
        probability_acceptable = mean((incremental_qaly * WTP_THRESHOLD_EUR_PER_QALY - incremental_cost) > 0, na.rm = TRUE),
        .groups = "drop"
      )
    return(out)
  }

  if (!is.null(summary_df) && nrow(summary_df) > 0) {
    out <- summary_df
    if (!"model_family" %in% names(out)) out$model_family <- "overall"
    out <- out %>%
      filter(metric %in% c("incremental_cost", "incremental_qaly", "ICER", "probability_acceptable_at_25000")) %>%
      rename(
        estimate_mean = estimate,
        lower = lower_95,
        upper = upper_95
      )
    return(out)
  }

  data.frame()
}

cea_family_summary <- add_cea_family_summary(cea$bootstrap, cea$summary)
cea_model_terms <- cea$model_terms
if (!is.null(cea_model_terms) && nrow(cea_model_terms) > 0 && !("model_family" %in% names(cea_model_terms))) {
  cea_model_terms$model_family <- ifelse(grepl("^GEE", cea_model_terms$model), "gee",
                                         ifelse(grepl("^GLM", cea_model_terms$model), "glm", "overall"))
}

if (!is.null(cea_model_terms) && nrow(cea_model_terms) > 0) {
  group_terms <- cea_model_terms %>%
    filter(grepl("^group", term)) %>%
    mutate(outcome = ifelse(grepl("cost", model), "cost", "qaly"))
} else {
  group_terms <- data.frame()
}

if (nrow(group_terms) > 0) {
  group_terms_table <- group_terms %>%
    select(model_family, model, outcome, term, estimate, estimate_exp, std_error, p_value)
} else {
  group_terms_table <- data.frame(
    note = "No family-split CEA term table was found in the current outputs.",
    stringsAsFactors = FALSE
  )
}

write_report_table <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return("_No results available._")
  out <- df
  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) out[[nm]] <- fmt_num(out[[nm]], digits)
  }
  cols <- names(out)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  separator <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- apply(out, 1, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, separator, rows), collapse = "\n")
}

report_lines <- c(
  "# BOFE manuscript results brief",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Quick scan",
  if (!is.null(mixed_12) && !is.null(gee_12)) {
    paste0(
      "- 12-month mixed-effects OR: ", fmt_ci(mixed_12$odds_ratio, mixed_12$ci_low, mixed_12$ci_high),
      "\n- 12-month GEE OR: ", fmt_ci(gee_12$odds_ratio, gee_12$ci_low, gee_12$ci_high),
      "\n- Mixed-effects is ", fmt_num(mixed_12$odds_ratio / gee_12$odds_ratio, 3),
      " times the GEE OR at 12 months."
    )
  } else {
    "- Effectiveness comparison could not be fully constructed from the available model outputs."
  },
  "",
  "## Effectiveness by timepoint",
  write_report_table(timepoint_table),
  "",
  "## 12-month comparison",
  write_report_table(effectiveness_overview),
  "",
  "## Cost-effectiveness summary",
  write_report_table(cea_family_summary),
  "",
  "## CEA model-term comparison",
  write_report_table(group_terms_table)
)

md_path <- "outputs/manuscript_results_brief.md"
txt_path <- "outputs/manuscript_results_brief.txt"
csv_path <- "outputs/manuscript_results_overview.csv"

writeLines(report_lines, md_path)
writeLines(report_lines, txt_path)
write.csv(effectiveness_overview, csv_path, row.names = FALSE)

cat(paste(report_lines, collapse = "\n"), "\n")

pipeline_phase_end(
  "07_manuscript_report",
  pipeline_started,
  "saved manuscript brief outputs"
)
