# R/04_effectiveness_helpers.R
# Shared helpers for the effectiveness analysis branches.

source("R/utils.R")

library(dplyr)
library(mice)
library(lme4)
library(broom.mixed)
library(geepack)

load_effectiveness_imputation <- function(imputation_variant) {
  imputation_variant <- tolower(imputation_variant)
  if (!imputation_variant %in% c("full", "simple", "complete_cases")) {
    stop("Unsupported imputation variant '", imputation_variant, "'.")
  }

  if (imputation_variant == "full") {
    imputation_artifact <- read_canonical_artifact("imputation")
    if ("effectiveness_mids" %in% names(imputation_artifact)) {
      return(imputation_artifact$effectiveness_mids)
    }
    return(imputation_artifact$full_mids)
  }

  if (imputation_variant == "complete_cases") {
    return(read_canonical_artifact("cleaning")$complete_cases)
  }

  sensitivity_artifact <- read_canonical_artifact("sensitivity")
  if (imputation_variant == "simple") {
    return(sensitivity_artifact$imputations$simple_wide)
  }

  stop("Unsupported imputation variant '", imputation_variant, "'.")
}

followup_effectiveness_sets <- function(long_sets, timepoints = FOLLOWUP_TIMEPOINTS) {
  time_levels <- paste0(timepoints, "mo")
  lapply(long_sets, function(long_data) {
    out <- long_data[long_data$time %in% time_levels, , drop = FALSE]
    out <- out[order(as.character(out$patient), factor(out$time, levels = time_levels)), , drop = FALSE]
    out$time <- factor(out$time, levels = time_levels)
    out
  })
}

extract_mixed_timepoint_contrast <- function(fit, time_value, reference_time = FOLLOWUP_TIMEPOINTS[1]) {
  coef_names <- names(fixef(fit))
  group_term <- grep("^group", coef_names, value = TRUE)[1]
  if (is.na(group_term) || !nzchar(group_term)) {
    stop("Could not identify the treatment-group coefficient.")
  }

  if (time_value == reference_time) {
    terms <- c(group_term)
  } else {
    candidate_terms <- c(
      paste0(group_term, ":time", time_value),
      paste0(group_term, ":time", time_value, "mo"),
      paste0(group_term, ":time", time_value, "mo)")
    )
    interaction_term <- candidate_terms[candidate_terms %in% coef_names][1]
    if (is.na(interaction_term) || !nzchar(interaction_term)) {
      stop("Missing interaction term for time ", time_value)
    }
    terms <- c(group_term, interaction_term)
  }

  beta <- sum(fixef(fit)[terms])
  variance <- sum(vcov(fit)[terms, terms, drop = FALSE])
  c(log_or = beta, var = variance)
}

fit_mixed_effects_models <- function(long_sets, model_formula, adjustment_label, n_imputations, timepoints = FOLLOWUP_TIMEPOINTS) {
  fit_list <- vector("list", n_imputations)
  for (i in seq_len(n_imputations)) {
    fit_started <- proc.time()[["elapsed"]]
    pipeline_phase_info(
      "04_models",
      sprintf("fitting glmer (%s) on imputation %d/%d", adjustment_label, i, n_imputations)
    )

    fit_list[[i]] <- glmer(
      formula = model_formula,
      family = "binomial",
        control = glmerControl(optimizer = method_config("effectiveness", "mixed_effects_optimizer")),
      data = long_sets[[i]]
    )

    fit_elapsed <- proc.time()[["elapsed"]] - fit_started
    fit_messages <- fit_list[[i]]@optinfo$conv$lme4$messages
    if (is.null(fit_messages)) {
      pipeline_phase_info(
        "04_models",
        sprintf("finished %s imputation %d/%d in %.1fs", adjustment_label, i, n_imputations, fit_elapsed)
      )
    } else {
      pipeline_phase_info(
        "04_models",
        sprintf(
          "finished %s imputation %d/%d in %.1fs with convergence warning: %s",
          adjustment_label,
          i,
          n_imputations,
          fit_elapsed,
          paste(fit_messages, collapse = " | ")
        )
      )
    }
  }

  if (n_imputations == 1L) {
    pooled_fit <- fit_list[[1]]
    pooled_summary <- broom.mixed::tidy(
      pooled_fit,
      effects = "fixed",
      conf.int = TRUE,
      conf.level = 0.95
    ) |>
      mutate(
        df = NA_real_,
        odds_ratio = exp(estimate),
        ci_low = exp(conf.low),
        ci_high = exp(conf.high)
      ) |>
      select(term, estimate, std.error, statistic, df, p.value, odds_ratio, ci_low, ci_high)
  } else {
    mira_glmm <- mice::as.mira(fit_list)
    pooled_fit <- mice::pool(mira_glmm)
    pooled_summary <- summary(pooled_fit)
  }

  pooled_summary$term <- pooled_summary$term
  pooled_summary$odds_ratio <- exp(pooled_summary$estimate)
  pooled_summary$ci_low <- exp(pooled_summary$estimate - qt(0.975, pooled_summary$df) * pooled_summary$std.error)
  pooled_summary$ci_high <- exp(pooled_summary$estimate + qt(0.975, pooled_summary$df) * pooled_summary$std.error)
  pooled_summary$model_family <- "mixed_effects"
  pooled_summary$adjustment <- adjustment_label
  pooled_summary$model <- paste0("Pooled_mixed_effects_imputed_", adjustment_label)

  contrast_table <- lapply(timepoints, function(tp) {
    contrasts <- t(vapply(
      fit_list,
      extract_mixed_timepoint_contrast,
      numeric(2),
      time_value = tp,
      reference_time = timepoints[1]
    ))
    qbar <- mean(contrasts[, "log_or"])
    ubar <- mean(contrasts[, "var"])
    b <- if (nrow(contrasts) > 1) stats::var(contrasts[, "log_or"]) else 0
    total_var <- ubar + (1 + 1 / nrow(contrasts)) * b
    se <- sqrt(total_var)
    z <- qbar / se
    data.frame(
      model_family = "mixed_effects",
      adjustment = adjustment_label,
      model = paste0("mixed_effects_", adjustment_label),
      time = tp,
      log_or = qbar,
      odds_ratio = exp(qbar),
      ci_low = exp(qbar - 1.96 * se),
      ci_high = exp(qbar + 1.96 * se),
      p_value = 2 * (1 - pnorm(abs(z))),
      n_imputations = nrow(contrasts),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  list(
    fits = fit_list,
    pooled_fit = pooled_fit,
    pooled_summary = pooled_summary,
    timepoint_effects = contrast_table
  )
}

run_mixed_effectiveness_analysis <- function(
    imputation_variant = c("full", "simple", "complete_cases"),
    write_outputs = TRUE,
    imputation_override = NULL) {
  imputation_variant <- match.arg(tolower(imputation_variant), c("full", "simple", "complete_cases"))
  imputation <- if (is.null(imputation_override)) {
    load_effectiveness_imputation(imputation_variant)
  } else {
    imputation_override
  }
  variant_suffix <- if (imputation_variant == "full") "" else paste0("_", imputation_variant)

  if (inherits(imputation, "mids")) {
    pipeline_phase_info("04_models", sprintf("reconstructing %d imputed long datasets", imputation$m))
  } else {
    pipeline_phase_info("04_models", sprintf("using single completed dataset for variant '%s'", imputation_variant))
  }

  pipeline_phase_info("04_models", "reconstructing completed long data for each imputation")
  effectiveness_sets <- prepare_effectiveness_long_sets(imputation)
  long_sets <- followup_effectiveness_sets(effectiveness_sets$long_sets)
  n_imputations <- effectiveness_sets$n_imputations

  if (effectiveness_sets$imputation_is_mids) {
    pipeline_phase_info("04_models", "keeping reconstructed long datasets for audit only")
    mids_data_long <- long_sets
  } else {
    mids_data_long <- long_sets[[1]]
  }

  pipeline_phase_info("04_models", "fitting unadjusted and adjusted mixed-effects models")
  mixed_model_specs <- list(
    unadjusted = as.formula(paste(
      method_config("effectiveness", "unadjusted_formula"),
      method_config("effectiveness", "mixed_effects_formula_suffix")
    )),
    adjusted = as.formula(paste(
      method_config("effectiveness", "adjusted_formula"),
      method_config("effectiveness", "mixed_effects_formula_suffix")
    ))
  )

  mixed_results <- lapply(names(mixed_model_specs), function(adj) {
    fit_mixed_effects_models(long_sets, mixed_model_specs[[adj]], adj, n_imputations)
  })
  names(mixed_results) <- names(mixed_model_specs)

  pooled_summary <- bind_rows(lapply(mixed_results, `[[`, "pooled_summary"))
  contrast_table <- bind_rows(lapply(mixed_results, `[[`, "timepoint_effects"))

  result <- list(
    imputation = imputation,
    imputation_variant = imputation_variant,
    long_reconstruction = mids_data_long,
    fits_unadjusted = mixed_results$unadjusted$fits,
    fits_adjusted = mixed_results$adjusted$fits,
    pooled_fit_unadjusted = mixed_results$unadjusted$pooled_fit,
    pooled_fit_adjusted = mixed_results$adjusted$pooled_fit,
    pooled_summary = pooled_summary,
    timepoint_effects = contrast_table,
    timepoint_effects_unadjusted = subset(contrast_table, adjustment == "unadjusted"),
    timepoint_effects_adjusted = subset(contrast_table, adjustment == "adjusted"),
    manuscript_style_12mo = subset(contrast_table, time == 12 & adjustment == "adjusted")
  )

  if (isTRUE(write_outputs)) {
    write_canonical_artifact("effectiveness_mixed", result)
    write_result_csv(result$pooled_summary, "model_mixed_summaries.csv")
    write_result_csv(result$timepoint_effects, "model_mixed_timepoint_effects.csv")
  }

  result
}

extract_gee_timepoint_contrast <- function(fit, time_value, reference_time = FOLLOWUP_TIMEPOINTS[1]) {
  coef_names <- names(coef(fit))
  group_term <- grep("^group", coef_names, value = TRUE)[1]

  if (is.na(group_term) || !nzchar(group_term)) {
    stop("Could not identify the treatment-group coefficient.")
  }

  if (time_value == reference_time) {
    terms <- c(group_term)
  } else {
    candidate_terms <- c(
      paste0(group_term, ":time", time_value),
      paste0(group_term, ":time", time_value, "mo"),
      paste0(group_term, ":time", time_value, "mo)")
    )
    interaction_term <- candidate_terms[candidate_terms %in% coef_names][1]

    if (is.na(interaction_term) || !nzchar(interaction_term)) {
      return(c(log_or = NA_real_, var = NA_real_))
    }
    terms <- c(group_term, interaction_term)
  }

  beta <- sum(coef(fit)[terms], na.rm = TRUE)
  variance <- sum(vcov(fit)[terms, terms, drop = FALSE], na.rm = TRUE)
  c(log_or = beta, var = variance)
}

pool_gee_models <- function(gee_list, adjustment_label) {
  coefs_list <- lapply(gee_list, coef)
  vars_list <- lapply(gee_list, vcov)
  qmat <- do.call(rbind, coefs_list)
  qbar <- colMeans(qmat)
  ubar <- Reduce("+", vars_list) / length(vars_list)
  bmat <- stats::cov(qmat)
  total_var <- ubar + (1 + 1 / nrow(qmat)) * bmat
  se <- sqrt(diag(total_var))
  z <- qbar / se
  p_values <- 2 * (1 - pnorm(abs(z)))

  data.frame(
    term = names(qbar),
    estimate = unname(qbar),
    std.error = unname(se),
    statistic = unname(z),
    p.value = unname(p_values),
    odds_ratio = exp(unname(qbar)),
    ci_low = exp(unname(qbar) - 1.96 * unname(se)),
    ci_high = exp(unname(qbar) + 1.96 * unname(se)),
    row.names = NULL
  )
}

pool_gee_timepoints <- function(gee_list, adjustment_label, timepoints = FOLLOWUP_TIMEPOINTS) {
  lapply(timepoints, function(tp) {
    contrasts <- t(vapply(
      gee_list,
      extract_gee_timepoint_contrast,
      numeric(2),
      time_value = tp,
      reference_time = timepoints[1]
    ))
    contrasts <- contrasts[!is.na(contrasts[, "log_or"]), , drop = FALSE]

    if (nrow(contrasts) == 0) {
      return(NULL)
    }

    qbar <- mean(contrasts[, "log_or"])
    ubar <- mean(contrasts[, "var"])
    b <- if (nrow(contrasts) > 1) stats::var(contrasts[, "log_or"]) else 0
    total_var <- ubar + (1 + 1 / nrow(contrasts)) * b
    se <- sqrt(total_var)
    z <- qbar / se

    data.frame(
      model_family = "gee",
      adjustment = adjustment_label,
      model = paste0("gee_", adjustment_label),
      time = tp,
      log_or = qbar,
      odds_ratio = exp(qbar),
      ci_low = exp(qbar - 1.96 * se),
      ci_high = exp(qbar + 1.96 * se),
      p_value = 2 * (1 - pnorm(abs(z))),
      n_imputations = nrow(contrasts),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()
}

fit_and_pool_gee_models <- function(long_sets, model_formula, adjustment_label, n_imputations, timepoints = FOLLOWUP_TIMEPOINTS) {
  fit_gee_list <- vector("list", n_imputations)
  for (i in seq_len(n_imputations)) {
    fit_started <- proc.time()[["elapsed"]]
    pipeline_phase_info(
      "04b_gee",
      sprintf("fitting geeglm (%s) on imputation %d/%d", adjustment_label, i, n_imputations)
    )

    fit_gee_list[[i]] <- tryCatch(
      geeglm(
        formula = model_formula,
        family = binomial(link = "logit"),
        id = patient,
        data = long_sets[[i]],
        corstr = "exchangeable",
        std.err = "san.se"
      ),
      error = function(e) {
        stop("GEE model failed for imputation ", i, ": ", conditionMessage(e), call. = FALSE)
      }
    )

    fit_elapsed <- proc.time()[["elapsed"]] - fit_started
    fit_messages <- fit_gee_list[[i]]$geese$conv
    if (is.null(fit_messages) || identical(fit_messages, 0L)) {
      pipeline_phase_info(
        "04b_gee",
        sprintf("finished %s imputation %d/%d in %.1fs", adjustment_label, i, n_imputations, fit_elapsed)
      )
    } else {
      pipeline_phase_info(
        "04b_gee",
        sprintf(
          "finished %s imputation %d/%d in %.1fs with convergence flag %s",
          adjustment_label,
          i,
          n_imputations,
          fit_elapsed,
          paste(fit_messages, collapse = " | ")
        )
      )
    }
  }

  pipeline_phase_info("04b_gee", sprintf("pooling %s GEE contrasts across imputations", adjustment_label))
  gee_pooled_summary <- pool_gee_models(fit_gee_list, adjustment_label)
  gee_pooled_summary$model <- paste0("GEE_exchangeable_imputed_", adjustment_label)
  gee_pooled_summary$model_family <- "gee"
  gee_pooled_summary$adjustment <- adjustment_label

  gee_contrast_table <- pool_gee_timepoints(fit_gee_list, adjustment_label, timepoints = timepoints)

  list(
    fits = fit_gee_list,
    gee_pooled_summary = gee_pooled_summary,
    gee_timepoint_effects = gee_contrast_table
  )
}

run_gee_effectiveness_analysis <- function(
    imputation_variant = c("full", "simple", "complete_cases"),
    write_outputs = TRUE,
    imputation_override = NULL) {
  imputation_variant <- match.arg(tolower(imputation_variant), c("full", "simple", "complete_cases"))
  imputation <- if (is.null(imputation_override)) {
    load_effectiveness_imputation(imputation_variant)
  } else {
    imputation_override
  }
  variant_suffix <- if (imputation_variant == "full") "" else paste0("_", imputation_variant)

  if (inherits(imputation, "mids")) {
    pipeline_phase_info("04b_gee", sprintf("reconstructing %d imputed long datasets", imputation$m))
  } else {
    pipeline_phase_info("04b_gee", sprintf("using single completed dataset for variant '%s'", imputation_variant))
  }

  pipeline_phase_info("04b_gee", "building per-imputation long datasets")
  effectiveness_sets <- prepare_effectiveness_long_sets(imputation)
  long_sets <- followup_effectiveness_sets(effectiveness_sets$long_sets)
  n_imputations <- effectiveness_sets$n_imputations

  pipeline_phase_info("04b_gee", "fitting unadjusted and adjusted GEE models")
  gee_model_specs <- list(
    unadjusted = as.formula(method_config("effectiveness", "unadjusted_formula")),
    adjusted = as.formula(method_config("effectiveness", "adjusted_formula"))
  )

  gee_results <- lapply(names(gee_model_specs), function(adj) {
    fit_and_pool_gee_models(long_sets, gee_model_specs[[adj]], adj, n_imputations)
  })
  names(gee_results) <- names(gee_model_specs)

  gee_pooled_summary <- bind_rows(lapply(gee_results, `[[`, "gee_pooled_summary"))
  gee_contrast_table <- bind_rows(lapply(gee_results, `[[`, "gee_timepoint_effects"))

  result <- list(
    imputation = imputation,
    imputation_variant = imputation_variant,
    gee_fits_unadjusted = gee_results$unadjusted$fits,
    gee_fits_adjusted = gee_results$adjusted$fits,
    gee_pooled_summary = gee_pooled_summary,
    gee_timepoint_effects = gee_contrast_table,
    gee_timepoint_effects_unadjusted = subset(gee_contrast_table, adjustment == "unadjusted"),
    gee_timepoint_effects_adjusted = subset(gee_contrast_table, adjustment == "adjusted"),
    gee_manuscript_style_12mo = subset(gee_contrast_table, time == 12 & adjustment == "adjusted")
  )

  if (isTRUE(write_outputs)) {
    write_canonical_artifact("effectiveness_gee", result)
    write_result_csv(result$gee_pooled_summary, "model_gee_summaries.csv")
    write_result_csv(result$gee_timepoint_effects, "model_gee_timepoint_effects.csv")
  }

  result
}
