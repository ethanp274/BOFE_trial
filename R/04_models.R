###########################################################################
# R/04_models.R
# Purpose: Fit primary analysis models (GEE per protocol) and mixed-effects
# Input: data_processed/all_cases.rds (ITT)
# Output: outputs/models_GEE.rds, outputs/models_mixed.rds, outputs/model_summaries.csv
###########################################################################

library(dplyr)
library(geepack)
library(lme4)

# Load data
all_cases_raw <- readRDS('data_processed/all_cases.rds')

# Basic label stripping
remove_labels_safe <- function(df) {
  df[] <- lapply(df, function(x) {
    if (class(x)[1] == 'haven_labelled') {
      as.numeric(x)
    } else {
      x
    }
  })
  df
}

df <- remove_labels_safe(all_cases_raw)
cat('Loaded ITT dataset: N =', nrow(df), '\n')

# Prepare long-format outcome: controlled_* variables expected (controlled_0.._12)
# Example: logistic GEE with exchangeable correlation by patient
# WARNING: Do not change outcome definitions without sign-off

# Define formula (example - adapt to real variable names)
# Outcome: controlled (binary) at all timepoints stacked in long form
# Fixed effects: treatment, time (as factor), treatment:time, age (D2.3_0), sex (D2.2_0), baseline control (controlled_0)

# Placeholder: assemble long dataset if controlled_* variables present
timepoints <- c(0,3,6,9,12)
outcome_name <- 'controlled'
stack_rows <- list()
for(tp in timepoints){
  varname <- paste0(outcome_name, '_', tp)
  if(varname %in% names(df)){
    tmp <- df %>% select(D1.1, D1.2, D1.3, D1.4, D2.3_0, D2.2_0, paste0('controlled_', tp))
    names(tmp)[names(tmp) == paste0('controlled_', tp)] <- 'outcome'
    tmp$time <- tp
    stack_rows[[length(stack_rows)+1]] <- tmp
  }
}

if(length(stack_rows) == 0) stop('No controlled_* variables found; cannot fit models')
long_df <- do.call(rbind, stack_rows)

# Ensure factors
long_df$D1.4 <- factor(long_df$D1.4)
long_df$time <- factor(long_df$time)

# Fit GEE (exchangeable)
gee_formula <- outcome ~ D1.4 * time + D2.3_0 + D2.2_0 + outcome[long_df$time==0]
# NOTE: the above is placeholder - implement baseline control properly when coding finished

cat('Fitting placeholder GEE model (specify correct formula before final run)\n')
# Example (commented out until formula finalized):
# gee_fit <- geeglm(outcome ~ D1.4 * time + D2.3_0 + D2.2_0 + controlled_0, id = D1.2, data = long_df, family = binomial, corstr = 'exchangeable')

# Fit mixed model (random intercept for patient)
cat('Fitting placeholder mixed-effects model (specify correct formula before final run)\n')
# mixed_fit <- glmer(outcome ~ D1.4 * time + D2.3_0 + D2.2_0 + (1 | D1.2), data = long_df, family = binomial)

# Save placeholders to outputs for later replacement
if(!dir.exists('outputs')) dir.create('outputs')
saveRDS(list(note='models placeholder - finalize formula and run'), file = 'outputs/models_placeholder.rds')
write.csv(data.frame(note='models placeholder - finalize formula and run'), 'outputs/model_summaries.csv', row.names = FALSE)

cat('R/04_models.R created (placeholder). Next: finalize formulas using regression_script.R and run GEE and mixed models on long_df.\n')
