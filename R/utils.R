# R/utils.R
# Shared helper functions for BOFE project cleaning/refactor

# Rename non-id vars by appending suffix (0,3,6,9,12)
rename_vars <- function(data, suffix){
  id_vars <- c('D1.1','D1.2','D1.3','D1.4')
  vars_to_rename <- setdiff(names(data), id_vars)
  new_names <- paste0(vars_to_rename, '_', suffix)
  names(data)[names(data) %in% vars_to_rename] <- new_names
  return(data)
}

# Replace exact 0 values with NA for columns matching any of the provided regex patterns
replace_zeros_with_na_patterns <- function(df, patterns){
  matched <- sapply(patterns, function(p) grepl(p, names(df)))
  if(is.matrix(matched)) cols <- names(df)[apply(matched, 1, any)] else cols <- names(df)[matched]
  # Fallback: if pattern matching fails, try numeric columns
  if(length(cols) == 0){
    cols <- names(df)[sapply(df, is.numeric)]
  }
  for(col in cols){
    # Only replace if column exists and is numeric/integer
    if(col %in% names(df) && is.numeric(df[[col]])){
      df[[col]] <- ifelse(df[[col]] == 0, NA, df[[col]])
    }
  }
  return(df)
}

# Remove labelled attributes safely
remove_labels <- function(df){
  df[] <- lapply(df, function(x){
    if(inherits(x, 'labelled')) x <- remove_val_labels(x)
    x
  })
  return(df)
}
