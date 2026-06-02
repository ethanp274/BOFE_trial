###########################################################################
# R/07_manuscript_report.R
# Purpose: Produce a manuscript-facing results brief that is easy to scan.
# Inputs:
#   - models/effectiveness_gee_artifact.rds
#   - models/cea_artifact.rds
# Outputs:
#   - results/manuscript_report_artifact.rds
###########################################################################

source("R/utils.R")

library(dplyr)

ensure_artifact_dirs()

pipeline_started <- pipeline_phase_start(
  "07_manuscript_report",
  "assembling a readable manuscript results brief"
)

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_ci <- function(est, low, high, digits = 3) {
  paste0(fmt_num(est, digits), " [", fmt_num(low, digits), ", ", fmt_num(high, digits), "]")
}

# Pull the first matching row for the requested model family.
pick_family_row <- function(df, family_values) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  adjustments <- if ("adjustment" %in% names(df)) c("adjusted", "unadjusted") else NA_character_
  for (adj in adjustments) {
    for (fam in family_values) {
      row <- if ("model_family" %in% names(df)) {
        if ("adjustment" %in% names(df)) {
          df[df$model_family %in% fam & df$adjustment %in% adj, , drop = FALSE]
        } else {
          df[df$model_family %in% fam, , drop = FALSE]
        }
      } else {
        if ("adjustment" %in% names(df)) {
          df[grepl(fam, df$model) & df$adjustment %in% adj, , drop = FALSE]
        } else {
          df[grepl(fam, df$model), , drop = FALSE]
        }
      }
      if (nrow(row) > 0) return(row[1, , drop = FALSE])
    }
  }
  NULL
}

# Standardise the effectiveness summary columns before tabulation.
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

# The brief reads directly from the current GEE artifact.
load_effectiveness <- function() {
  gee_rds <- read_canonical_artifact("effectiveness_gee")
  gee_df <- normalize_effectiveness(gee_rds$gee_timepoint_effects, "gee")
  gee_df
}

# The brief reads the main CEA artifact and its bundled tables.
load_cea_summary <- function() {
  cea_rds <- read_canonical_artifact("cea")
  summary_df <- cea_rds$summary
  comparison_df <- cea_rds$cea_model_comparison
  bootstrap_df <- cea_rds$bootstrap_results
  accept_df <- cea_rds$acceptability_curve
  model_terms_df <- cea_rds$model_summaries

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
    stop("No effectiveness results were found in results/.")
}

effectiveness <- effectiveness %>%
  mutate(
    model = ifelse(model %in% c("mixed", "mixed_effects", "GLMM", "glmm"), "mixed_effects", model),
    model = ifelse(model %in% c("gee", "GEE"), "gee", model)
  )

if (!"adjustment" %in% names(effectiveness)) {
  effectiveness$adjustment <- "adjusted"
}

effectiveness$adjustment <- factor(effectiveness$adjustment, levels = c("unadjusted", "adjusted"))

timepoint_table <- effectiveness %>%
  arrange(adjustment, time, model) %>%
  select(model_family, model, adjustment, time, odds_ratio, ci_low, ci_high, any_of("p_value"), n_imputations)

timepoint_12 <- timepoint_table %>% filter(time == 12)
gee_12 <- pick_family_row(timepoint_12 %>% filter(adjustment == "adjusted"), c("gee"))

effectiveness_overview <- timepoint_12 %>%
  mutate(
    section = "effectiveness",
    item = paste0(model, "_", adjustment, "_12mo_or"),
    estimate = odds_ratio,
    lower_95 = ci_low,
    upper_95 = ci_high,
    note = ifelse(
      adjustment == "adjusted",
      "Adjusted for baseline control, age, and sex",
      "Unadjusted model"
    )
  ) %>%
  select(section, model, adjustment, item, estimate, lower_95, upper_95, any_of("p_value"), note)

add_cea_family_summary <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0 || !"metric" %in% names(summary_df)) {
    stop("Missing current CEA summary in the canonical CEA artifact.")
  }

  summary_df %>%
    mutate(
      model_family = if (!"model_family" %in% names(summary_df)) "mi_main" else model_family
    ) %>%
    select(
      model_family,
      metric,
      estimate,
      any_of(c(
        "pooled_ci_lower",
        "pooled_ci_upper",
        "bootstrap_ci_lower",
        "bootstrap_ci_upper",
        "within_variance",
        "between_variance",
        "pooled_variance",
        "pooled_std_error",
        "n_boot",
        "uncertainty_method"
      ))
    )
}

cea_family_summary <- add_cea_family_summary(cea$summary)
cea_model_terms <- cea$model_terms
if (!is.null(cea_model_terms) && nrow(cea_model_terms) > 0 && !("model_family" %in% names(cea_model_terms))) {
  cea_model_terms$model_family <- ifelse(grepl("^GEE", cea_model_terms$model), "gee",
                                         ifelse(grepl("^GLM", cea_model_terms$model), "glm", "overall"))
}

if (!is.null(cea_model_terms) && nrow(cea_model_terms) > 0) {
  if (!"std_error" %in% names(cea_model_terms) && "std.error" %in% names(cea_model_terms)) {
    cea_model_terms$std_error <- cea_model_terms$std.error
  }
  if (!"p_value" %in% names(cea_model_terms) && "p.value" %in% names(cea_model_terms)) {
    cea_model_terms$p_value <- cea_model_terms$p.value
  }
  group_terms <- cea_model_terms %>%
    filter(grepl("^group", term)) %>%
    mutate(outcome = ifelse(grepl("cost", model), "cost", "qaly"))
} else {
  group_terms <- data.frame()
}

if (nrow(group_terms) > 0) {
  group_terms_table <- group_terms %>%
    select(model_family, model, outcome, term, estimate, any_of("estimate_exp"), any_of("std_error"), any_of("p_value"))
} else {
  group_terms_table <- data.frame(
    note = "No family-split CEA term table was found in the current outputs.",
    stringsAsFactors = FALSE
  )
}

# Render small data frames as compact markdown tables.
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
  if (!is.null(gee_12)) {
    paste0(
      "\n- Adjusted 12-month GEE OR: ", fmt_ci(gee_12$odds_ratio, gee_12$ci_low, gee_12$ci_high),
      "\n- This is the configured primary effectiveness analysis (", method_config("effectiveness", "primary_model_family"), ")."
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

write_canonical_artifact(
  "manuscript_report",
  list(
    stage = "07_manuscript_report",
    report_lines = report_lines,
    markdown = paste(report_lines, collapse = "\n")
  )
)

cat(paste(report_lines, collapse = "\n"), "\n")

pipeline_phase_end(
  "07_manuscript_report",
  pipeline_started,
  "saved canonical manuscript report artifact"
)
