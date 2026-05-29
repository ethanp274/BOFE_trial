###########################################################################
# R/05_cost_effectiveness_parallel.R
# Purpose: Parallel bootstrap wrapper for the complete-case CEA pipeline.
# This script enables the parallel bootstrap mode and then runs the main
# cost-effectiveness workflow in R/05_cost_effectiveness.R.
###########################################################################

options(bofe.parallel_bootstrap = TRUE)
options(bofe.bootstrap_workers = 4)
options(bofe.bootstrap_iterations = 5000)

message("05_cost_effectiveness_parallel: enabling parallel bootstrap mode.")
source("R/05_cost_effectiveness.R")
