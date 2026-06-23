# test_gee.R
# Test script to reproduce the cluster_id issue

source("R/02_imputation_helpers.R")
source("R/04_effectiveness_helpers.R")

library(dplyr)
library(geepack)

main_imputation_artifact <- read_canonical_artifact("imputation")
effectiveness_full_mids <- main_imputation_artifact$effectiveness_mids

print("TESTING WITH PATIENT:")
tryCatch({
  gee_patient <- run_gee_effectiveness_analysis(
    imputation_variant = "full",
    write_outputs = FALSE,
    imputation_override = effectiveness_full_mids,
    cluster_var = "patient"
  )
  print("SUCCESS WITH PATIENT!")
}, error = function(e) {
  print(paste("ERROR PATIENT:", conditionMessage(e)))
})

print("TESTING WITH PHARMACY:")
tryCatch({
  gee_pharm <- run_gee_effectiveness_analysis(
    imputation_variant = "full",
    write_outputs = FALSE,
    imputation_override = effectiveness_full_mids,
    cluster_var = "pharmacy"
  )
  print("SUCCESS WITH PHARMACY!")
}, error = function(e) {
  print(paste("ERROR PHARMACY:", conditionMessage(e)))
})
