# R/cleaning_helpers.R
# Identifier, label, structural-zero, and numeric conversion helpers.

# Rename non-id vars by appending suffix (0, 3, 6, 9, 12).
rename_vars <- function(data, suffix) {
  id_vars <- c("D1.1", "D1.2", "D1.3", "D1.4")
  vars_to_rename <- setdiff(names(data), id_vars)
  new_names <- paste0(vars_to_rename, "_", suffix)
  names(data)[names(data) %in% vars_to_rename] <- new_names
  data
}

# Replace exact 0 values with NA for columns matching any of the provided regex patterns.
replace_zeros_with_na_patterns <- function(df, patterns) {
  matched <- sapply(patterns, function(p) grepl(p, names(df)))
  if (is.matrix(matched)) cols <- names(df)[apply(matched, 1, any)] else cols <- names(df)[matched]
  if (length(cols) == 0) cols <- names(df)[sapply(df, is.numeric)]
  for (col in cols) {
    if (col %in% names(df) && is.numeric(df[[col]])) {
      df[[col]] <- ifelse(df[[col]] == 0, NA, df[[col]])
    }
  }
  df
}

apply_structural_zero_rules <- function(df, timepoint_tag) {
  patterns <- STRUCTURAL_ZERO_RULES[[timepoint_tag]]
  if (is.null(patterns)) return(df)
  replace_zeros_with_na_patterns(df, patterns)
}

# Remove labelled attributes safely.
remove_labels <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "labelled") && requireNamespace("labelled", quietly = TRUE)) {
      x <- labelled::remove_val_labels(x)
    }
    if (inherits(x, "haven_labelled")) {
      x <- as.numeric(x)
    }
    x
  })
  df
}

as_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) x <- as.numeric(x)
  if (is.factor(x)) return(suppressWarnings(as.numeric(as.character(x))))
  suppressWarnings(as.numeric(x))
}

ensure_numeric_columns <- function(df, cols) {
  for (col in intersect(cols, names(df))) {
    df[[col]] <- as_numeric_safe(df[[col]])
  }
  df
}

standardize_core_identifiers <- function(df) {
  if ("D1.2" %in% names(df) && !"patient" %in% names(df)) {
    df[["patient"]] <- as.character(df[["D1.2"]])
  }
  if ("D1.1" %in% names(df) && !"pharmacy" %in% names(df)) {
    df[["pharmacy"]] <- as.character(df[["D1.1"]])
  }
  if ("D1.3_0" %in% names(df) && !"condition" %in% names(df)) {
    df[["condition"]] <- as_numeric_safe(df[["D1.3_0"]])
  }
  if ("D1.4_0" %in% names(df) && !"group" %in% names(df)) {
    df[["group"]] <- as.character(df[["D1.4_0"]])
  }
  if ("D2.2_0" %in% names(df) && !"gender" %in% names(df)) {
    df[["gender"]] <- as_numeric_safe(df[["D2.2_0"]])
  }
  if ("D2.3_0" %in% names(df) && !"age" %in% names(df)) {
    df[["age"]] <- as_numeric_safe(df[["D2.3_0"]])
  }
  df
}
