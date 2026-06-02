# R/model_summary_helpers.R
# Model coefficient summary helpers.

summarise_model_terms <- function(fit, model_name, exponentiate = FALSE) {
  coef_df <- as.data.frame(summary(fit)$coefficients)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  names(coef_df) <- sub("^Estimate$", "estimate", names(coef_df))
  names(coef_df) <- sub("^Std\\. Error$|^Std\\.err$", "std_error", names(coef_df))
  names(coef_df) <- sub("^Pr\\(>\\|z\\|\\)$|^Pr\\(>\\|W\\|\\)$|^Pr\\(>\\|t\\|\\)$", "p_value", names(coef_df))
  coef_df$model <- model_name
  if (exponentiate && "estimate" %in% names(coef_df)) {
    coef_df$estimate_exp <- exp(coef_df$estimate)
  }
  coef_df[, c("model", "term", setdiff(names(coef_df), c("model", "term"))), drop = FALSE]
}
