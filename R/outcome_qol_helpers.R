# R/outcome_qol_helpers.R
# Clinical outcome, EQ-5D, and baseline derivation helpers.

row_sums_strict <- function(df, cols) {
  cols <- present_cols(df, cols)
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  rowSums(df[, cols, drop = FALSE])
}

recode_height_bmi <- function(df) {
  if (all(c("D3.1_0", "D3.2_0") %in% names(df))) {
    df[["D3.1_0"]] <- as_numeric_safe(df[["D3.1_0"]])
    df[["D3.2_0"]] <- as_numeric_safe(df[["D3.2_0"]])
    df[["D3.1_0"]] <- ifelse(df[["D3.1_0"]] > 100, df[["D3.1_0"]] / 100, df[["D3.1_0"]])
    df[["D3.3_0"]] <- df[["D3.2_0"]] / (df[["D3.1_0"]] ^ 2)
  }
  df
}

derive_controlled_outcomes <- function(df, timepoints = TIMEPOINTS) {
  for (tp in timepoints) {
    disease_col <- paste0("D1.3_", tp)
    disease <- if (disease_col %in% names(df)) {
      as_numeric_safe(df[[disease_col]])
    } else if ("condition" %in% names(df)) {
      as_numeric_safe(df[["condition"]])
    } else {
      as_numeric_safe(df[["D1.3"]])
    }

    act_score <- paste0("ACT.SCORE_", tp)
    ccq_score <- paste0("CCQ.SCORE_", tp)
    act_items <- paste0("ACT.", 1:5, "_", tp)
    ccq_items <- paste0("CCQ.", 1:10, "_", tp)

    df <- ensure_numeric_columns(df, c(act_score, ccq_score, act_items, ccq_items))

    if (act_score %in% names(df)) {
      df[[act_score]][disease == 2] <- NA
      if (tp == 3) {
        df[[act_score]] <- ifelse(df[[act_score]] == 0, NA, df[[act_score]])
      }
    }
    for (col in present_cols(df, act_items)) {
      df[[col]][disease == 2] <- NA
      if (tp == 3 && act_score %in% names(df)) {
        df[[col]] <- ifelse(is.na(df[[act_score]]), NA, df[[col]])
      }
    }

    if (ccq_score %in% names(df)) {
      df[[ccq_score]][disease == 1] <- NA
      if (max(df[[ccq_score]], na.rm = TRUE) > 6.5) {
        df[[ccq_score]] <- df[[ccq_score]] / 10
      }
      if (tp == 3) {
        df[[ccq_score]][df[["D1.2"]] %in% c("KK1A", "KJ2A")] <- NA
      }
    }
    for (col in present_cols(df, ccq_items)) {
      df[[col]][disease == 1] <- NA
      if (tp == 3) {
        df[[col]][df[["D1.2"]] %in% c("KK1A", "KJ2A")] <- NA
      }
    }

    act_controlled <- paste0("ACT_controlled_", tp)
    ccq_controlled <- paste0("CCQ_controlled_", tp)
    controlled <- paste0("controlled_", tp)

    df[[act_controlled]] <- if (act_score %in% names(df)) {
      threshold <- method_config("outcomes", "asthma_act_control_threshold")
      ifelse(is.na(df[[act_score]]), NA, ifelse(df[[act_score]] >= threshold, 1, 0))
    } else {
      NA_real_
    }

    df[[ccq_controlled]] <- if (ccq_score %in% names(df)) {
      threshold <- method_config("outcomes", "copd_ccq_control_threshold")
      ifelse(is.na(df[[ccq_score]]), NA, ifelse(df[[ccq_score]] < threshold, 1, 0))
    } else {
      NA_real_
    }

    asthma_idx <- !is.na(disease) & disease == 1
    copd_idx <- !is.na(disease) & disease == 2

    df[[controlled]] <- NA_real_
    df[[controlled]][asthma_idx] <- df[[act_controlled]][asthma_idx]
    df[[controlled]][copd_idx] <- df[[ccq_controlled]][copd_idx]
  }

  df
}

configured_eq5d_tariffs <- function() {
  names(method_config("economics", "eq5d_tariff_lookup"))
}

score_eq5d_dimension <- function(values, dimension, tariff = method_config("economics", "main_eq5d_tariff")) {
  tariff <- match.arg(tariff, configured_eq5d_tariffs())
  lookup <- method_config("economics", "eq5d_tariff_lookup")[[tariff]]
  if (is.null(lookup)) {
    stop(
      "score_eq5d_dimension: tariff '", tariff, "' is not configured yet. ",
      "Populate BOFE_METHODS_CONFIG$economics$eq5d_tariff_lookup[['", tariff, "']] with the corresponding coefficients."
    )
  }
  values <- as.character(as_numeric_safe(values))
  unname(lookup[[dimension]][values])
}

derive_eqindex <- function(df, timepoints = TIMEPOINTS, tariff = method_config("economics", "main_eq5d_tariff")) {
  tariff <- match.arg(tariff, configured_eq5d_tariffs())
  dimensions <- c(
    mobility = "EQ5D5L.1",
    selfcare = "EQ5D5L.2",
    activity = "EQ5D5L.3",
    pain = "EQ5D5L.4",
    anxiety = "EQ5D5L.5"
  )

  for (tp in timepoints) {
    disutility_cols <- character(0)
    all_dimension_cols <- paste0(dimensions, "_", tp)
    if (!all(all_dimension_cols %in% names(df))) next

    for (dimension in names(dimensions)) {
      source_col <- paste0(dimensions[[dimension]], "_", tp)
      scored_col <- paste0("disut_", dimension, "_", tp)
      df[[source_col]] <- as_numeric_safe(df[[source_col]])
      invalid_response <- !is.na(df[[source_col]]) & !df[[source_col]] %in% 1:5
      df[[source_col]][invalid_response] <- NA_real_
      df[[scored_col]] <- score_eq5d_dimension(df[[source_col]], dimension, tariff = tariff)
      disutility_cols <- c(disutility_cols, scored_col)
    }

    total_col <- paste0("total_disut_", tp)
    eqindex_col <- paste0("EQindex_", tp)
    df[[total_col]] <- row_sums_strict(df, disutility_cols)
    df[[eqindex_col]] <- round(1 - df[[total_col]], 3)

    impossible_profile <- Reduce(`|`, lapply(all_dimension_cols, function(col) !is.na(df[[col]]) & !df[[col]] %in% 1:5))
    impossible_profile[is.na(impossible_profile)] <- FALSE
    df[[eqindex_col]][impossible_profile] <- NA
  }

  df
}

derive_baseline_ccq_domains <- function(df) {
  disease <- if ("D1.3_0" %in% names(df)) {
    as_numeric_safe(df[["D1.3_0"]])
  } else if ("condition" %in% names(df)) {
    as_numeric_safe(df[["condition"]])
  } else {
    as_numeric_safe(df[["D1.3"]])
  }
  ccq_items <- paste0("CCQ.", 1:10, "_0")
  df <- ensure_numeric_columns(df, ccq_items)

  if (all(c("CCQ.1_0", "CCQ.2_0", "CCQ.5_0", "CCQ.6_0") %in% names(df))) {
    df[["CCQ.symptom_0"]] <- NA_real_
    df[["CCQ.symptom_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.1_0", "CCQ.2_0", "CCQ.5_0", "CCQ.6_0"), drop = FALSE] / 4,
      na.rm = TRUE
    )
  }
  if (all(c("CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0") %in% names(df))) {
    df[["CCQ.functional_0"]] <- NA_real_
    df[["CCQ.functional_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0"), drop = FALSE] / 4,
      na.rm = TRUE
    )
  }
  if (all(c("CCQ.3_0", "CCQ.4_0") %in% names(df))) {
    df[["CCQ.mental_0"]] <- NA_real_
    df[["CCQ.mental_0"]][disease == 2] <- rowSums(
      df[disease == 2, c("CCQ.3_0", "CCQ.4_0"), drop = FALSE] / 2.27,
      na.rm = TRUE
    )
  }

  df
}

add_analysis_derivations <- function(df, tariff = method_config("economics", "main_eq5d_tariff")) {
  tariff <- match.arg(tariff, configured_eq5d_tariffs())
  df <- remove_labels(df)
  df <- standardize_core_identifiers(df)
  df <- recode_height_bmi(df)
  df <- derive_controlled_outcomes(df)
  df <- derive_baseline_ccq_domains(df)
  df <- derive_eqindex(df, tariff = tariff)
  df
}
