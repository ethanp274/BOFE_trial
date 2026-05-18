
######################################################
########## Descriptive statistics and tests ##########
######################################################

######################################################
# TABLE 1: BASELINE CHARACTERISTICS AND TESTS ########
######################################################

# By pathology: D1.3 -> 1= Asthma, 2= COPD

# Load data
df_complete <- df[df$complete == 1,]
n_complete <- nrow(df_complete)

df_complete <- df_complete %>% 
  rename("D1.3_0"="D1.3_0.x", "D1.4_0"="D1.4_0.x") %>%
  select(-c("D1.3_0.y", "D1.4_0.y"))

#view(df_complete)

# recode live alone (D2.7_0)
df_complete <- df_complete %>%
  mutate(D2.7_0 = D2.7_0 - 1)

# recode gender (D2.2_0)
df_complete <- df_complete %>%
  mutate(D2.2_0 = D2.2_0 - 1)


# Create data frames by health condition
df_asthma <- df_complete[df_complete$D1.3_0==1,] #Asthma
df_COPD <- df_complete[df_complete$D1.3_0==2,] #COPD

n_asthma <- nrow(df_asthma)
n_COPD <- nrow(df_COPD)

#view(df_asthma)

n_asthma_BOFE <- nrow(df_asthma[df_asthma$D1.4_0 == "ig (intervention group)",])
n_asthma_UC <- nrow(df_asthma[df_asthma$D1.4_0 == "cg (control group)",])
n_COPD_BOFE <- nrow(df_COPD[df_COPD$D1.4_0 == "ig (intervention group)",])
n_COPD_UC <- nrow(df_COPD[df_COPD$D1.4_0 == "cg (control group)",])

n_BOFE <- nrow(df_complete[df_complete$D1.4_0 == "ig (intervention group)",])
n_UC <- nrow(df_complete[df_complete$D1.4_0 == "cg (control group)",])

if(n_asthma_BOFE + n_COPD_BOFE == n_BOFE){
  print("BOFE adds up")
}
if(n_asthma_UC + n_COPD_UC == n_UC){
  print("UC adds up")
}

# Type of variables: 

ordinal_vars <- c("D2.3_0")

ordinal_var_names <- c(
  "D2.3_0" = "Age"
)

continuous_vars <- c("D3.1_0","D3.2_0", "D3.3_0","D3.10_1_0", "D3.10_2_0", 
                     "D3.10_3_0", "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", 
                     "D3.10_7_0","D3.11_1_0", "D3.11_2_0", "ACT.SCORE_0", 
                     "CCQ.SCORE_0", "EQ5D5L.SCORE_0", "D5.2_0")

continuous_var_names <- c(
  "D3.1_0" = "Height",
  "D3.2_0" = "Weight",
  "D3.3_0" = "BMI",
  "D3.10_1_0" = "GP Visits",
  "D3.10_2_0" = "Nurse Visits",
  "D3.10_3_0" = "Therapist Visits",
  "D3.10_4_0" = "ER Visits",
  "D3.10_5_0" = "Hopsital (Outpatient) Visits",
  "D3.10_6_0" = "Hospital (Inpatient) Visits",
  "D3.10_7_0" = "Hospital (Inpatient) Days",
  "D3.11_1_0" = "Social Worker Visits",
  "D3.11_2_0" = "Adult Day Centre (per week)",
  "ACT.SCORE_0" = "Asthma Index",
  "CCQ.SCORE_0" = "COPD Index",
  "EQ5D5L.SCORE_0" = "Quality of Life Index",
  "D5.2_0" = "Number of Medications"
)

proportion_vars <- c("D2.2_0", "D2.7_0","D3.5_1_0","D3.5_2_0","D3.5_3_0", "D3.7_1_0",
                     "D3.7_2_0","D3.7_3_0", "D3.8_0", "D3.9_0","D3.12_0")

proportion_var_names <- c(
  "D2.2_0" = "Gender",
  "D2.7_0" = "Live Alone",
  "D3.5_1_0" = "Flu Vax (12 mo)",
  "D3.5_2_0" = "Pneumonia Vax (12 mo)",
  "D3.5_3_0" = "COVID Vax (12 mo)",
  "D3.7_1_0" = "Diabetes",
  "D3.7_2_0" = "Heart Disease",
  "D3.7_3_0" = "Other Disease",
  "D3.8_0" = "COVID Test",
  "D3.9_0" = "COVID Positive",
  "D3.12_0" = "Employed"
)
                     
                     
categoric_vars <- c("D2.1_0", "D2.4_0", "D2.5_0","D2.6_0","D3.6_0",
                    "D5.5_0","D5.6_0")

categoric_var_names <- c(
  "D2.1_0" = "Location",
  "D2.4_0" = "Ethnicity",
  "D2.5_0" = "Education", 
  "D2.6_0" = "Selection Source",
  "D3.6_0" = "Smoking Status",
  "D5.5_0" = "Medication Problems",
  "D5.6_0" = "Medication Understanding"
)


############ TABLE 1: FUNCTIONS ##############################################
# BASELINE CHARACTERSTICS FOR ASTHMA PATIENTS


library(dplyr)
library(tidyr)


# Function to check if the grouping factor has exactly 2 levels
has_two_levels <- function(data, group_col) {
  length(unique(data[[group_col]])) == 2
}

# Create a function to calculate mean and sd for continuous variables
calculate_continuous_stats <- function(data, var, group_col, group_value) {
  filtered_data <- data %>% filter(!!sym(group_col) == group_value)
  mean_val <- mean(filtered_data[[var]], na.rm = TRUE)
  sd_val <- sd(filtered_data[[var]], na.rm = TRUE)
  return(c(mean = mean_val, sd = sd_val))
}

# Create a function to calculate proportion for categorical variables
calculate_proportion_stats <- function(data, var, group_col, group_value) {
  filtered_data <- data %>% filter(!!sym(group_col) == group_value)
  prop_table <- prop.table(table(filtered_data[[var]]))
  return(prop_table)
}

#Overarching method to generate characteristics table for any patient subgroup
create_characteristics_table <- function(df){
  print("Creating characteristics table")

  # Check unique values in D1.4_0
  print(paste("Number of groups:", length(unique(df$D1.4_0))))

  # Initialize empty data frame
  Table1 <- data.frame(variable_name = character(),
                      intervention_mean = character(),
                      intervention_sd = character(),
                      intervention_pvalue = numeric(),
                      control_mean = character(),
                      control_sd = character(),
                      control_pvalue = numeric(),
                      stringsAsFactors = FALSE)

  # Continuous variables
  for (var in continuous_vars) {
    print(paste("Processing variable:", var))
    if (has_two_levels(df, "D1.4_0")) {
      intervention_stats <- calculate_continuous_stats(df, var, "D1.4_0", "ig (intervention group)")
      control_stats <- calculate_continuous_stats(df, var, "D1.4_0", "cg (control group)")
      
      # Ensure the variable has non-missing values for both groups
      if (all(!is.na(intervention_stats)) && all(!is.na(control_stats))) {
        wilcox_test <- wilcox.test(df[[var]] ~ df$D1.4_0)
        
        Table1 <- Table1 %>% 
          add_row(variable_name = continuous_var_names[var],
                  intervention_mean = as.character(round(intervention_stats["mean"], 2)),
                  intervention_sd = as.character(round(intervention_stats["sd"], 2)),
                  intervention_pvalue = wilcox_test$p.value,
                  control_mean = as.character(round(control_stats["mean"], 2)),
                  control_sd = as.character(round(control_stats["sd"], 2)),
                  control_pvalue = wilcox_test$p.value)
      }
      print(paste("Successfully processed:", var))
    }
    else {
      print("Variable is not shared by IG and CG")
    }
  }

  # Proportion variables
  for (var in proportion_vars) {
    print(paste("Processing variable:", var))
    if (has_two_levels(df, "D1.4_0")) {
      intervention_stats <- calculate_proportion_stats(df, var, "D1.4_0", "ig (intervention group)")
      control_stats <- calculate_proportion_stats(df, var, "D1.4_0", "cg (control group)")
      
      prop_test <- prop.test(x = c(sum(df[df$D1.4_0 == "ig (intervention group)", var], na.rm = TRUE),
                                  sum(df[df$D1.4_0 == "cg (control group)", var], na.rm = TRUE)),
                            n = c(sum(!is.na(df[df$D1.4_0 == "ig (intervention group)", var])),
                                  sum(!is.na(df[df$D1.4_0 == "cg (control group)", var]))))
      
      Table1 <- Table1 %>% 
        add_row(variable_name = proportion_var_names[var],
                intervention_mean = as.character(paste0(round(intervention_stats[1] * 100, 2), "%")),
                intervention_sd = NA,
                intervention_pvalue = prop_test$p.value,
                control_mean = as.character(paste0(round(control_stats[1] * 100, 2), "%")),
                control_sd = NA,
                control_pvalue = prop_test$p.value)
      print(paste("Successfully processed:", var))
    }
    else {
      print("Variable is not shared by IG and CG")
    }
  }

  # Ordinal variables
  for (var in ordinal_vars) {
    print(paste("Processing variable:", var))
    if (has_two_levels(df, "D1.4_0")) {
      intervention_stats <- calculate_proportion_stats(df, var, "D1.4_0", "ig (intervention group)")
      control_stats <- calculate_proportion_stats(df, var, "D1.4_0", "cg (control group)")
      
      # Ensure the variable has non-missing values for both groups
      if (all(!is.na(intervention_stats)) && all(!is.na(control_stats))) {
        wilcox_test <- wilcox.test(df[[var]] ~ df$D1.4_0)
        
        for (i in 1:length(intervention_stats)){
          Table1 <- Table1 %>% 
            add_row(variable_name = as.character(paste0("", ordinal_var_names[var], i)),
                    intervention_mean = as.character(paste0(round(intervention_stats[i] * 100, 2), "%")),
                    intervention_sd = NA,
                    intervention_pvalue = wilcox_test$p.value,
                    control_mean = as.character(paste0(round(control_stats[i] * 100, 2), "%")),
                    control_sd = NA,
                    control_pvalue = wilcox_test$p.value)
        }
        
      }
      print(paste("Successfully processed:", var))
    }
    else {
      print("Variable is not shared by IG and CG")
    }
  }

  # Categorical variables
  for (var in categoric_vars) {
    print(paste("Processing variable:", var))
    if (has_two_levels(df, "D1.4_0")) {
      intervention_stats <- calculate_proportion_stats(df, var, "D1.4_0", "ig (intervention group)")
      control_stats <- calculate_proportion_stats(df, var, "D1.4_0", "cg (control group)")
      
      chisq_test <- chisq.test(table(df[[var]], df$D1.4_0), simulate.p.value = TRUE)
      
      for (j in 1:length(intervention_stats)){
        Table1 <- Table1 %>% 
          add_row(variable_name = as.character(paste0("", categoric_var_names[var], j)),
                  intervention_mean = as.character(paste0(round(intervention_stats[j] * 100, 2), "%")),
                  intervention_sd = NA,
                  intervention_pvalue = chisq_test$p.value,
                  control_mean = as.character(paste0(round(control_stats[j] * 100, 2), "%")),
                  control_sd = NA,
                  control_pvalue = chisq_test$p.value)
      }
        
      print(paste("Successfully processed:", var))
    }
    else {
      print(paste(var, "is not shared by IG and CG"))
    }
  }

  return(Table1)
}

# Create characteristics tables for asthma and COPD subpopulations
Table1_asthma <- create_characteristics_table(df_asthma)
Table1_COPD <- create_characteristics_table(df_COPD)
Table1_total <- create_characteristics_table(df_complete)

view(Table1_asthma)
view(Table1_COPD)
view(Table1_total)

################ FIX ERRORS IN AUTO-GENERATED TABLE 1 #########################

df_asthma_BOFE <- df_asthma[df_asthma$D1.4_0 == "ig (intervention group)", ]
df_asthma_UC <- df_asthma[df_asthma$D1.4_0 == "cg (control group)", ]
df_COPD_BOFE <- df_COPD[df_COPD$D1.4_0 == "ig (intervention group)", ]
df_COPD_UC <- df_COPD[df_COPD$D1.4_0 == "cg (control group)", ]

df_BOFE <- df_complete[df_complete$D1.4_0 == "ig (intervention group)", ]
df_UC <- df_complete[df_complete$D1.4_0 == "cg (control group)", ]


# Age (D2.3)
asthma_BOFE_ages <- prop.table(table(df_asthma_BOFE$D2.3_0))
asthma_UC_ages <- prop.table(table(df_asthma_UC$D2.3_0))
copd_BOFE_ages <- prop.table(table(df_COPD_BOFE$D2.3_0))
copd_UC_ages <- prop.table(table(df_COPD_UC$D2.3_0))

copd_age_significance <- chisq.test(x = df_COPD$D2.3_0, y = df_COPD$D1.4_0, simulate.p.value = TRUE)

# Ethnicity (D2.4)
copd_BOFE_ethnic <- prop.table(table(df_COPD_BOFE$D2.4_0))
copd_UC_ethnic <- prop.table(table(df_COPD_UC$D2.4_0))
asthma_BOFE_ethnic <- prop.table(table(df_asthma_BOFE$D2.4_0))
asthma_UC_ethnic <- prop.table(table(df_asthma_UC$D2.4_0))
total_BOFE_ethnic <- prop.table(table(df_BOFE$D2.4_0))
total_UC_ethnic <- prop.table(table(df_UC$D2.4_0))

asthma_ethnic_signif <- chisq.test(x = df_asthma$D2.4_0, y = df_asthma$D1.4_0, simulate.p.value = TRUE)
copd_ethnic_signif <- chisq.test(x = df_COPD$D2.4_0, y = df_COPD$D1.4_0, simulate.p.value = TRUE)
total_ethnic_signif <- chisq.test(x = df_complete$D2.4_0, y = df_complete$D1.4_0, simulate.p.value = TRUE)


# Education (D2.5)
asthma_BOFE_edu <- prop.table(table(df_asthma_BOFE$D2.5_0))
asthma_UC_edu <- prop.table(table(df_asthma_UC$D2.5_0))

asthma_edu_signif <- chisq.test(x = df_asthma$D2.5_0, y = df_asthma$D1.4_0, simulate.p.value = TRUE)

# Flu Vaccine (D3.5_1)
asthma_BOFE_flu <- prop.table(table(df_asthma_BOFE$D3.5_1_0))
asthma_UC_flu <- prop.table(table(df_asthma_UC$D3.5_1_0))
copd_BOFE_flu <- prop.table(table(df_COPD_BOFE$D3.5_1_0))
copd_UC_flu <- prop.table(table(df_COPD_UC$D3.5_1_0))

total_BOFE_flu <- prop.table(table(df_BOFE$D3.5_1_0))
total_UC_flu <- prop.table(table(df_UC$D3.5_1_0))
# ALL ZERO


# Pneumonia vaccine (D3.5_2)
asthma_BOFE_pneu <- prop.table(table(df_asthma_BOFE$D3.5_2_0))
asthma_UC_pneu <- prop.table(table(df_asthma_UC$D3.5_2_0))
copd_BOFE_pneu <- prop.table(table(df_COPD_BOFE$D3.5_2_0))
copd_UC_pneu <- prop.table(table(df_COPD_UC$D3.5_2_0))
total_BOFE_pneu <- prop.table(table(df_BOFE$D3.5_2_0))
total_UC_pneu <- prop.table(table(df_UC$D3.5_2_0))

asthma_pneu_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.5_2_0), sum(df_asthma_UC$D3.5_2_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_pneu_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.5_2_0), sum(df_COPD_UC$D3.5_2_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_pneu_signif <- prop.test(x = c(sum(df_BOFE$D3.5_2_0), sum(df_UC$D3.5_2_0)), n = c(n_BOFE, n_UC))

# COVID vaccine (D3.5_3)
asthma_BOFE_cvax <- prop.table(table(df_asthma_BOFE$D3.5_3_0))
asthma_UC_cvax <- prop.table(table(df_asthma_UC$D3.5_3_0))
copd_BOFE_cvax <- prop.table(table(df_COPD_BOFE$D3.5_3_0))
copd_UC_cvax <- prop.table(table(df_COPD_UC$D3.5_3_0))
total_BOFE_cvax <- prop.table(table(df_BOFE$D3.5_3_0))
total_UC_cvax <- prop.table(table(df_UC$D3.5_3_0))

asthma_cvax_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.5_3_0), sum(df_asthma_UC$D3.5_3_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_cvax_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.5_3_0), sum(df_COPD_UC$D3.5_3_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_cvax_signif <- prop.test(x = c(sum(df_BOFE$D3.5_3_0), sum(df_UC$D3.5_3_0)), n = c(n_BOFE, n_UC))

# COVID test (D3.8)
asthma_BOFE_ctest <- prop.table(table(df_asthma_BOFE$D3.8_0))
asthma_UC_ctest <- prop.table(table(df_asthma_UC$D3.8_0))
copd_BOFE_ctest <- prop.table(table(df_COPD_BOFE$D3.8_0))
copd_UC_ctest <- prop.table(table(df_COPD_UC$D3.8_0))
total_BOFE_ctest <- prop.table(table(df_BOFE$D3.8_0))
total_UC_ctest <- prop.table(table(df_UC$D3.8_0))

asthma_ctest_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.8_0), sum(df_asthma_UC$D3.8_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_ctest_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.8_0), sum(df_COPD_UC$D3.8_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_ctest_signif <- prop.test(x = c(sum(df_BOFE$D3.8_0), sum(df_UC$D3.8_0)), n = c(n_BOFE, n_UC))


# COVID positive (D3.9)
asthma_BOFE_cpos <- prop.table(table(df_asthma_BOFE$D3.9_0))
asthma_UC_cpos <- prop.table(table(df_asthma_UC$D3.9_0))
copd_BOFE_cpos <- prop.table(table(df_COPD_BOFE$D3.9_0))
copd_UC_cpos <- prop.table(table(df_COPD_UC$D3.9_0))
total_BOFE_cpos <- prop.table(table(df_BOFE$D3.9_0))
total_UC_cpos <- prop.table(table(df_UC$D3.9_0))

asthma_cpos_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.9_0), sum(df_asthma_UC$D3.9_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_cpos_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.9_0), sum(df_COPD_UC$D3.9_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_cpos_signif <- prop.test(x = c(sum(df_BOFE$D3.9_0), sum(df_UC$D3.9_0)), n = c(n_BOFE, n_UC))


# Diabetes (D3.7_1)
asthma_BOFE_dm <- prop.table(table(df_asthma_BOFE$D3.7_1_0))
asthma_UC_dm <- prop.table(table(df_asthma_UC$D3.7_1_0))
copd_BOFE_dm <- prop.table(table(df_COPD_BOFE$D3.7_1_0))
copd_UC_dm <- prop.table(table(df_COPD_UC$D3.7_1_0))
# ALL ZERO


# Heart Disease (D3.7_2)
asthma_BOFE_ihd <- prop.table(table(df_asthma_BOFE$D3.7_2_0))
asthma_UC_ihd <- prop.table(table(df_asthma_UC$D3.7_2_0))
copd_BOFE_ihd <- prop.table(table(df_COPD_BOFE$D3.7_2_0))
copd_UC_ihd <- prop.table(table(df_COPD_UC$D3.7_2_0))
total_BOFE_ihd <- prop.table(table(df_BOFE$D3.7_2_0))
total_UC_ihd <- prop.table(table(df_UC$D3.7_2_0))

asthma_ihd_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.7_2_0), sum(df_asthma_UC$D3.7_2_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_ihd_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.7_2_0), sum(df_COPD_UC$D3.7_2_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_ihd_signif <- prop.test(x = c(sum(df_BOFE$D3.7_2_0), sum(df_UC$D3.7_2_0)), n = c(n_BOFE, n_UC))


# Other chronic disease (D3.7_3)
asthma_BOFE_other <- prop.table(table(df_asthma_BOFE$D3.7_3_0))
asthma_UC_other <- prop.table(table(df_asthma_UC$D3.7_3_0))
copd_BOFE_other <- prop.table(table(df_COPD_BOFE$D3.7_3_0))
copd_UC_other <- prop.table(table(df_COPD_UC$D3.7_3_0))

asthma_other_signif <- prop.test(x = c(sum(df_asthma_BOFE$D3.7_3_0), sum(df_asthma_UC$D3.7_3_0)), n = c(n_asthma_BOFE, n_asthma_UC))
copd_other_signif <- prop.test(x = c(sum(df_COPD_BOFE$D3.7_3_0), sum(df_COPD_UC$D3.7_3_0)), n = c(n_COPD_BOFE, n_COPD_UC))
total_other_signif <- prop.test(x = c(sum(df_BOFE$D3.7_3_0), sum(df_UC$D3.7_3_0)), n = c(n_BOFE, n_UC))


# Medication issues (D5.5)
asthma_BOFE_medissues <- prop.table(table(df_asthma_BOFE$D5.5_0))
asthma_UC_medissues <- prop.table(table(df_asthma_UC$D5.5_0))
copd_BOFE_medissues <- prop.table(table(df_COPD_BOFE$D5.5_0))
copd_UC_medissues <- prop.table(table(df_COPD_UC$D5.5_0))

asthma_medissues_signif <- chisq.test(x = df_asthma$D1.4_0, df_asthma$D5.5_0, simulate.p.value = TRUE)
copd_medissues_signif <- chisq.test(x = df_COPD$D1.4_0, df_COPD$D5.5_0, simulate.p.value = TRUE)

# Medication understanding (D5.6)
asthma_BOFE_medundertand <- prop.table(table(df_asthma_BOFE$D5.6_0))
asthma_UC_medunderstand <- prop.table(table(df_asthma_UC$D5.6_0))
copd_BOFE_medunderstand <- prop.table(table(df_COPD_BOFE$D5.6_0))
copd_UC_medunderstand <- prop.table(table(df_COPD_UC$D5.6_0))

asthma_medunderstand_signif <- chisq.test(x = df_asthma$D1.4_0, df_asthma$D5.6_0, simulate.p.value = TRUE)
copd_medunderstand_signif <- chisq.test(x = df_COPD$D1.4_0, df_COPD$D5.6_0, simulate.p.value = TRUE)


# Selection Source (D2.6)
asthma_BOFE_source <- prop.table(table(df_asthma_BOFE$D2.6_0))
asthma_UC_source <- prop.table(table(df_asthma_UC$D2.6_0))
copd_BOFE_source <- prop.table(table(df_COPD_BOFE$D2.6_0))
copd_UC_source <- prop.table(table(df_COPD_UC$D2.6_0))


# EQ5D Score (EQindex)

asthma_BOFE_eq = c(mean(df_asthma_BOFE$EQindex_0, na.rm = TRUE), sd(df_asthma_BOFE$EQindex_0, na.rm = TRUE))
asthma_UC_eq = c(mean(df_asthma_UC$EQindex_0, na.rm = TRUE), sd(df_asthma_UC$EQindex_0, na.rm = TRUE))
copd_BOFE_eq = c(mean(df_COPD_BOFE$EQindex_0, na.rm = TRUE), sd(df_COPD_BOFE$EQindex_0, na.rm = TRUE))
copd_UC_eq = c(mean(df_COPD_UC$EQindex_0, na.rm = TRUE), sd(df_COPD_UC$EQindex_0, na.rm = TRUE))
total_BOFE_eq = c(mean(df_BOFE$EQindex_0, na.rm = TRUE), sd(df_BOFE$EQindex_0, na.rm = TRUE))
total_UC_eq = c(mean(df_UC$EQindex_0, na.rm = TRUE), sd(df_UC$EQindex_0, na.rm = TRUE))

asthma_eq_signif <- wilcox.test(df_asthma$EQindex_0 ~ df_asthma$D1.4_0)
copd_eq_signif <- wilcox.test(df_COPD$EQindex_0 ~ df_COPD$D1.4_0)
total_eq_signif <- wilcox.test(df_complete$EQindex_0 ~ df_complete$D1.4_0)


################################# Table 2 ########################################

## QUANT OUTCOMES

## FOR CCQ:
# Symptom = (item 1 + 2 + 5 + 6)/4;
# Functional state = (item 7 + 8 + 9 + 10)/4; 
# Mental state = (item 3 + 4)/2.27


for(i in seq(0,12,3)){
  df_COPD[paste0('CCQ_symptom_', i)] <- rowSums(df_COPD[,c(paste0('CCQ.1_',i), paste0('CCQ.2_',i), paste0('CCQ.5_', i), paste0('CCQ.6_', i))]/4, na.rm = TRUE)
  df_COPD[paste0('CCQ_function_', i)] <- rowSums(df_COPD[,c(paste0('CCQ.7_',i), paste0('CCQ.8_',i), paste0('CCQ.9_', i), paste0('CCQ.10_', i))]/4, na.rm = TRUE)
  df_COPD[paste0('CCQ_mental_', i)] <- rowSums(df_COPD[,c(paste0('CCQ.3_',i), paste0('CCQ.4_',i))]/2.27, na.rm = TRUE)
  df_asthma[paste0('CCQ_symptom_', i)] <- NA
  df_asthma[paste0('CCQ_function_', i)] <- NA
  df_asthma[paste0('CCQ_mental_', i)] <- NA
}

df_complete_remake <- rbind(df_COPD, df_asthma)

quant_outcome_variables <- c(
  "ACT.1", "ACT.2", "ACT.3", "ACT.4", "ACT.5", "ACT.SCORE", "ACT_controlled",
  "CCQ_symptom", "CCQ_function", "CCQ_mental", "CCQ.SCORE", "CCQ_controlled",
  "EQindex", "controlled"
)


generate_quant_outcomes <- function(df){
  
  output_table <- data.frame(
    variable = character(),
    ig_num = numeric(),
    ig_mean = numeric(),
    ig_sd = numeric(),
    cg_num = numeric(),
    cg_mean = numeric(),
    cg_sd = numeric(),
    p_val = numeric()
  )
  
  df_ig <- df[df$D1.4_0 == "ig (intervention group)", ]
  df_cg <- df[df$D1.4_0 == "cg (control group)", ]
  
  for(i in 1:length(quant_outcome_variables)){
    
    var <- quant_outcome_variables[i]
    
    for(j in seq(0,12,3)){
      var_j <- paste0(var,'_',j)
      
      ig_num <- sum(!is.na(df_ig[[var_j]]))
      ig_mean <- mean(df_ig[[var_j]], na.rm = TRUE)
      ig_sd <- sd(df_ig[[var_j]], na.rm = TRUE)
      
      cg_num <- sum(!is.na(df_cg[[var_j]]))
      cg_mean <- mean(df_cg[[var_j]], na.rm = TRUE)
      cg_sd <- sd(df_cg[[var_j]], na.rm = TRUE)
      
      if(all(!is.na(df_ig[[var_j]])) && all(!is.na(df_ig[[var_j]]))){
        test <- wilcox.test(df_ig[[var_j]], df_cg[[var_j]])
        p_val <- test$p.value
      }
      else{
        p_val <- 1.0
      }
        
      
      output_table <- output_table %>%
        add_row(
          variable = var_j, 
          ig_num = ig_num,
          ig_mean = round(ig_mean, 2),
          ig_sd = round(ig_sd, 2),
          cg_num = cg_num,
          cg_mean = round(cg_mean, 2),
          cg_sd = round(cg_sd, 2),
          p_val = round(p_val, 4)
        )
      
    }
  }
  
  return(output_table)
}

asthma_quant_outcomes <- generate_quant_outcomes(df_asthma)
COPD_quant_outcomes <- generate_quant_outcomes(df_COPD)
total_quant_outcomes <- generate_quant_outcomes(df_complete_remake)

## CATEGORICAL OUTCOMES

df_complete_remake <- df_complete_remake %>%
  mutate(
    total_controlled_0 = ifelse((D1.3_0 == 1 & ACT_controlled_0 == 1) | (D1.3_0 == 2 & CCQ_controlled_0 == 1), 1, 0),
    total_controlled_3 = ifelse((D1.3_0 == 1 & ACT_controlled_3 == 1) | (D1.3_0 == 2 & CCQ_controlled_3 == 1), 1, 0),
    total_controlled_6 = ifelse((D1.3_0 == 1 & ACT_controlled_6 == 1) | (D1.3_0 == 2 & CCQ_controlled_6 == 1), 1, 0),
    total_controlled_9 = ifelse((D1.3_0 == 1 & ACT_controlled_9 == 1) | (D1.3_0 == 2 & CCQ_controlled_9 == 1), 1, 0),
    total_controlled_12 = ifelse((D1.3_0 == 1 & ACT_controlled_12 == 1) | (D1.3_0 == 2 & CCQ_controlled_12 == 1), 1, 0)
  )

df_complete_remake_BOFE <- df_complete_remake[df_complete_remake$D1.4_0 == "ig (intervention group)", ]
df_complete_remake_UC <- df_complete_remake[df_complete_remake$D1.4_0 == "cg (control group)", ]

df_complete_remake <- df_complete_remake %>%
  mutate(
    total_controlled_0 = total_controlled_0 + 1,
    total_controlled_3 = total_controlled_3 + 1,
    total_controlled_6 = total_controlled_6 + 1,
    total_controlled_9 = total_controlled_9 + 1,
    total_controlled_12 = total_controlled_12 + 1
  )


cat_outcome_variables <- c(
  'ACT_controlled', 'CCQ_controlled', 'total_controlled','D5.9', 'D5.10'
)

cat_var_levels <- c(
  'ACT_controlled' = 2,
  'CCQ_controlled' = 2,
  'total_controlled' = 2,
  'D5.5' = 3,
  'D5.6' = 3, 
  'D5.7' = 4,
  'D5.8' = 3,
  'D5.9' = 3,
  'D5.10' = 5
)

generate_cat_outcomes <- function(df){
  
  output_table <- data.frame(
    variable = character(),
    level = numeric(),
    ig_num = numeric(),
    ig_prop = numeric(),
    cg_num = numeric(),
    cg_prop = numeric(),
    p_val = numeric()
  )
  
  df_ig <- df[df$D1.4_0 == "ig (intervention group)", ]
  df_cg <- df[df$D1.4_0 == "cg (control group)", ]
  
  for(i in 1:length(cat_outcome_variables)){
    var <- cat_outcome_variables[i]
    
    for(j in seq(0,12,3)){
      
      var_j <- paste0(var,'_',j)
      
      df_ig[, var_j][df_ig[, var_j] == 0] <- NA
      df_cg[, var_j][df_cg[, var_j] == 0] <- NA
      
      ig_num <- sum(!is.na(df_ig[[var_j]]))
      cg_num <- sum(!is.na(df_cg[[var_j]]))
      
      if(ig_num == 0 || cg_num == 0){
        next
      }
      
      ig_table <- table(df_ig[[var_j]])
      ig_prop_table <- prop.table(ig_table)
      ig_names <- dimnames(ig_prop_table)[[1]]
      
      cg_table <- table(df_cg[[var_j]])
      cg_prop_table <- prop.table(cg_table)
      cg_names <- dimnames(cg_prop_table)[[1]]
      
      for(k in 1:cat_var_levels[cat_outcome_variables[i]]){
        ig_index <- match(as.character(k), ig_names)
        if(!is.na(ig_index)){
          ig_count <- ig_table[ig_index]
          ig_prop <- round(ig_prop_table[ig_index] * 100, 3)
        }
        else{
          ig_count <- 0
          ig_prop <- 0.000
        }
        
        cg_index <- match(as.character(k), ig_names)
        if(!is.na(cg_index)){
          cg_count <- cg_table[cg_index]
          cg_prop <- round(cg_prop_table[cg_index] * 100, 3)
        }
        else{
          cg_count <- 0
          cg_prop <- 0.000
        }
        
        test <- prop.test(x = c(ig_count, cg_count), n = c(ig_num, cg_num))
        p_val <- test$p.value
        
        output_table <- output_table %>% 
          add_row(
            variable = var_j,
            level = k,
            ig_num = ig_num,
            ig_prop = ig_prop,
            cg_num = cg_num,
            cg_prop = cg_prop,
            p_val = p_val
          )
        
        
        
      }
    }
  }
  
  return(output_table)
  
}

asthma_cat_outcomes <- generate_cat_outcomes(df_asthma)
COPD_cat_outcomes <- generate_cat_outcomes(df_COPD)
total_cat_outcomes <- generate_cat_outcomes(df_complete_remake)





# DIFFERENCES

df_asthma_diff <- df_asthma %>%
  mutate(
    ACT.SCORE_diff = ACT.SCORE_12 - ACT.SCORE_0,
    ACT.1_diff = ACT.1_12 - ACT.1_0,
    ACT.2_diff = ACT.2_12 - ACT.2_0,
    ACT.3_diff = ACT.3_12 - ACT.3_0,
    ACT.4_diff = ACT.4_12 - ACT.4_0,
    ACT.5_diff = ACT.5_12 - ACT.5_0,
    EQindex_diff = EQindex_12 - EQindex_0
  )

asthma_diff_vars <- c(
  'ACT.SCORE_diff', 'ACT.1_diff', 'ACT.2_diff',
  'ACT.3_diff', 'ACT.4_diff', 'ACT.5_diff', 'EQindex_diff'
)

df_COPD_diff <- df_COPD %>%
  mutate(
    CCQ.SCORE_diff = CCQ.SCORE_12 - CCQ.SCORE_0,
    CCQ_function_diff = CCQ_function_12 - CCQ_function_0,
    CCQ_symptom_diff = CCQ_symptom_12 - CCQ_symptom_0,
    CCQ_mental_diff = CCQ_mental_12 - CCQ_mental_0,
    EQindex_diff = EQindex_12 - EQindex_0
  )

COPD_diff_vars <- c(
  'CCQ.SCORE_diff', 'CCQ_symptom_diff', 'CCQ_function_diff',
  'CCQ_mental_diff', 'EQindex_diff'
)

df_complete_diff <- df_complete %>%
  mutate(
    EQindex_diff = EQindex_12 - EQindex_0
  )

total_diff_vars <- c(
  'EQindex_diff'
)



generate_quant_outcomes_diff <- function(df, vars){
  
  output_table <- data.frame(
    variable = character(),
    ig_num = numeric(),
    ig_mean = numeric(),
    ig_sd = numeric(),
    cg_num = numeric(),
    cg_mean = numeric(),
    cg_sd = numeric(),
    p_val = numeric()
  )
  
  df_ig <- df[df$D1.4_0 == "ig (intervention group)", ]
  df_cg <- df[df$D1.4_0 == "cg (control group)", ]
  
  for(i in 1:length(vars)){
    
    var <- vars[i]
      
    ig_num <- sum(!is.na(df_ig[[var]]))
    ig_mean <- mean(df_ig[[var]], na.rm = TRUE)
    ig_sd <- sd(df_ig[[var]], na.rm = TRUE)
    
    cg_num <- sum(!is.na(df_cg[[var]]))
    cg_mean <- mean(df_cg[[var]], na.rm = TRUE)
    cg_sd <- sd(df_cg[[var]], na.rm = TRUE)
    
    if(all(!is.na(df_ig[[var]])) && all(!is.na(df_ig[[var]]))){
      test <- wilcox.test(df_ig[[var]], df_cg[[var]])
      p_val <- test$p.value
    }
    else{
      p_val <- 0.0 
    }
    
    
    output_table <- output_table %>%
      add_row(
        variable = as.character(var), 
        ig_num = ig_num,
        ig_mean = round(ig_mean, 2),
        ig_sd = round(ig_sd, 2),
        cg_num = cg_num,
        cg_mean = round(cg_mean, 2),
        cg_sd = round(cg_sd, 2),
        p_val = round(p_val, 4)
      )
  }
  
  return(output_table)
}

asthma_diff_outcomes <- generate_quant_outcomes_diff(df_asthma_diff, asthma_diff_vars)
COPD_diff_outcomes <- generate_quant_outcomes_diff(df_COPD_diff, COPD_diff_vars)
total_diff_outcomes <- generate_quant_outcomes_diff(df_complete_diff, total_diff_vars)


controlled_diff_test <- function(n1, n2, y1, y2){
  p1 <- y1/n1
  p2 <- y2/n2
  
  combined_prop = (y1 + y2) / (n1 + n2)
  
  z = (p1 - p2) / sqrt(combined_prop*(1-combined_prop)*((1/n1) + (1/n2)))
  
  p_val = pnorm(z, mean = 0, sd = 1, lower.tail = FALSE) * 2
  
  return(p_val)
}


ACT_controlled_diff_BOFE <- sum(df_asthma_BOFE$ACT_controlled_12) - sum(df_asthma_BOFE$ACT_controlled_0)
ACT_controlled_diff_BOFE_prop <- ACT_controlled_diff_BOFE / sum(!is.na(df_asthma_BOFE$ACT_controlled_12))

ACT_controlled_diff_UC <- sum(df_asthma_UC$ACT_controlled_12) - sum(df_asthma_UC$ACT_controlled_0)
ACT_controlled_diff_UC_prop <- ACT_controlled_diff_UC / sum(!is.na(df_asthma_UC$ACT_controlled_12))

ACT_controlled_diff_BOFE_test <- prop.test(x = c(sum(df_asthma_BOFE$ACT_controlled_12), sum(df_asthma_BOFE$ACT_controlled_0)), n = c(sum(!is.na(df_asthma_BOFE$ACT_controlled_12)), sum(!is.na(df_asthma_BOFE$ACT_controlled_0))))
ACT_controlled_diff_UC_test <- prop.test(x = c(sum(df_asthma_UC$ACT_controlled_12), sum(df_asthma_UC$ACT_controlled_0)), n = c(sum(!is.na(df_asthma_UC$ACT_controlled_12)), sum(!is.na(df_asthma_UC$ACT_controlled_0))))

ACT_controlled_diff_test <- controlled_diff_test(sum(!is.na(df_asthma_BOFE$ACT_controlled_12)), sum(!is.na(df_asthma_UC$ACT_controlled_12)), ACT_controlled_diff_BOFE, ACT_controlled_diff_UC)




CCQ_controlled_diff_BOFE <- sum(df_COPD_BOFE$CCQ_controlled_12) - sum(df_COPD_BOFE$CCQ_controlled_0)
CCQ_controlled_diff_BOFE_prop <- CCQ_controlled_diff_BOFE / sum(!is.na(df_COPD_BOFE$CCQ_controlled_12))

CCQ_controlled_diff_UC <- sum(df_COPD_UC$CCQ_controlled_12) - sum(df_COPD_UC$CCQ_controlled_0)
CCQ_controlled_diff_UC_prop <- CCQ_controlled_diff_UC / sum(!is.na(df_COPD_UC$CCQ_controlled_12))

CCQ_controlled_diff_BOFE_test <- prop.test(x = c(sum(df_COPD_BOFE$CCQ_controlled_12), sum(df_COPD_BOFE$CCQ_controlled_0)), n = c(sum(!is.na(df_COPD_BOFE$CCQ_controlled_12)), sum(!is.na(df_COPD_BOFE$CCQ_controlled_0))))
CCQ_controlled_diff_UC_test <- prop.test(x = c(sum(df_COPD_UC$CCQ_controlled_12), sum(df_COPD_UC$CCQ_controlled_0)), n = c(sum(!is.na(df_COPD_UC$CCQ_controlled_12)), sum(!is.na(df_COPD_UC$CCQ_controlled_0))))

CCQ_controlled_diff_test <- controlled_diff_test(sum(!is.na(df_COPD_BOFE$CCQ_controlled_12)), sum(!is.na(df_COPD_UC$CCQ_controlled_12)), CCQ_controlled_diff_BOFE, CCQ_controlled_diff_UC)




total_controlled_diff_BOFE <- sum(df_complete_remake_BOFE$total_controlled_12) - sum(df_complete_remake_BOFE$total_controlled_0)
total_controlled_diff_BOFE_prop <- total_controlled_diff_BOFE / sum(!is.na(df_complete_remake_BOFE$total_controlled_12))

total_controlled_diff_UC <- sum(df_complete_remake_UC$total_controlled_12) - sum(df_complete_remake_UC$total_controlled_0)
total_controlled_diff_UC_prop <- total_controlled_diff_UC / sum(!is.na(df_complete_remake_UC$total_controlled_12))

total_controlled_diff_BOFE_test <- prop.test(x = c(sum(df_complete_remake_BOFE$total_controlled_12), sum(df_complete_remake_BOFE$total_controlled_0)), n = c(sum(!is.na(df_complete_remake_BOFE$total_controlled_12)), sum(!is.na(df_complete_remake_BOFE$total_controlled_0))))
total_controlled_diff_UC_test <- prop.test(x = c(sum(df_complete_remake_UC$total_controlled_12), sum(df_complete_remake_UC$total_controlled_0)), n = c(sum(!is.na(df_complete_remake_UC$total_controlled_12)), sum(!is.na(df_complete_remake_UC$total_controlled_0))))

total_controlled_diff_test <- controlled_diff_test(n1 = sum(!is.na(df_complete_remake_BOFE$total_controlled_12)),
                                                   n2 = sum(!is.na(df_complete_remake_UC$total_controlled_12)),
                                                   y1 = total_controlled_diff_BOFE,
                                                   y2 = total_controlled_diff_UC)



missed_dose_diff <- function(df_ig, df_cg){
  yes_diff_ig <- sum(df_ig$D5.9_12 == 1) - sum(df_ig$D5.9_0 == 1)
  yes_diff_ig_prop <- yes_diff_ig / sum(!is.na(df_ig$D5.9_12))
  yes_diff_ig_test <- prop.test(x = c(sum(df_ig$D5.9_12 == 1), sum(df_ig$D5.9_0 == 1)), n = c(sum(!is.na(df_ig$D5.9_12 == 1)), sum(!is.na(df_ig$D5.9_0 == 1))))
  yes_diff_ig_pval <- yes_diff_ig_test$p.value
  
  yes_diff_cg <- sum(df_cg$D5.9_12 == 1) - sum(df_cg$D5.9_0 == 1)
  yes_diff_cg_prop <- yes_diff_cg / sum(!is.na(df_cg$D5.9_12))
  yes_diff_cg_test <- prop.test(x = c(sum(df_cg$D5.9_12 == 1), sum(df_cg$D5.9_0 == 1)), n = c(sum(!is.na(df_cg$D5.9_12 == 1)), sum(!is.na(df_cg$D5.9_0 == 1))))
  yes_diff_cg_pval <- yes_diff_cg_test$p.value
  
  no
  
  dk
  
  
  
  return(data.frame(
    yes_prop = c(yes_diff_ig_prop, yes_diff_cg_prop),
    yes_pval = c(yes_diff_ig_pval, yes_diff_cg_pval),
    no_prop = ,
    no_pval = ,
    dk_prop = ,
    dk_pval = ,
  ))
}

# RESOURCE USE

resource_vars <- c(
  'D3.10_1', 'D3.10_2', 'D3.10_3', 'D3.10_4',
  'D3.10_5', 'D3.10_6', 'D3.10_7', 
  'D3.11_1', 'D3.11_2'
)

resource_var_names <- c(
  'D3.10_1' = 'GP Visits', 
  'D3.10_2' = 'Nurse Visits', 
  'D3.10_3' = 'Therapist Visits', 
  'D3.10_4' = 'A&E Visits',
  'D3.10_5' = 'Outpatient Visits', 
  'D3.10_6' = 'Inpatient Visits', 
  'D3.10_7' = 'Inpatient Days', 
  'D3.11_1' = 'Social Worker Visits', 
  'D3.11_2' = 'Day Centre Visits (per week)'
)

generate_resource_outcomes <- function(df){
  
  df_ig <- df[df$D1.4_0 == "ig (intervention group)", ]
  df_cg <- df[df$D1.4_0 == "cg (control group)", ]
  
  
  output_table <- data.frame(
    variable = character(),
    ig_num = numeric(),
    ig_mean = numeric(),
    ig_sd = numeric(),
    cg_num = numeric(),
    cg_mean = numeric(),
    cg_sd = numeric(),
    p_val = numeric()
  )
  
  for(i in 1:length(resource_vars)){
    
    for(j in seq(0,12,6)){
      var_j <- paste0(resource_vars[i], '_', j)
      
      ig_num <- sum(!is.na(df_ig[[var_j]]))
      ig_mean <- mean(df_ig[[var_j]], na.rm = TRUE)
      ig_sd <- sd(df_ig[[var_j]], na.rm = TRUE)
      
      cg_num <- sum(!is.na(df_cg[[var_j]]))
      cg_mean <- mean(df_cg[[var_j]], na.rm = TRUE)
      cg_sd <- sd(df_cg[[var_j]], na.rm = TRUE)
      
      test <- wilcox.test(df_ig[!is.na(df_ig[[var_j]]), ][[var_j]], df_cg[!is.na(df_cg[[var_j]]), ][[var_j]])
      p_val <- test$p.value
      
      output_table <- output_table %>% add_row(
        variable = paste0(resource_var_names[i], ' ', j),
        ig_num = ig_num, 
        ig_mean = ig_mean,
        ig_sd = ig_sd, 
        cg_num = cg_num, 
        cg_mean = cg_mean, 
        cg_sd = cg_sd,
        p_val = p_val
      )
      
      
    }
  }
  return(output_table)
}


asthma_resource_use <- generate_resource_outcomes(df_asthma)
COPD_resource_use <- generate_resource_outcomes(df_COPD)
total_resource_use <- generate_resource_outcomes(df_complete_remake)


#Resource Differences

df_asthma_diff <- df_asthma_diff %>%
  mutate(
    D3.10_1_diff = D3.10_1_12 - D3.10_1_0,
    D3.10_2_diff = D3.10_2_12 - D3.10_2_0,
    D3.10_3_diff = D3.10_3_12 - D3.10_3_0,
    D3.10_4_diff = D3.10_4_12 - D3.10_4_0,
    D3.10_5_diff = D3.10_5_12 - D3.10_5_0,
    D3.10_6_diff = D3.10_6_12 - D3.10_6_0,
    D3.10_7_diff = D3.10_7_12 - D3.10_7_0,
    D3.11_1_diff = D3.11_1_12 - D3.11_1_0,
    D3.11_2_diff = D3.11_2_12 - D3.11_2_0
  )

df_COPD_diff <- df_COPD_diff %>%
  mutate(
    D3.10_1_diff = D3.10_1_12 - D3.10_1_0,
    D3.10_2_diff = D3.10_2_12 - D3.10_2_0,
    D3.10_3_diff = D3.10_3_12 - D3.10_3_0,
    D3.10_4_diff = D3.10_4_12 - D3.10_4_0,
    D3.10_5_diff = D3.10_5_12 - D3.10_5_0,
    D3.10_6_diff = D3.10_6_12 - D3.10_6_0,
    D3.10_7_diff = D3.10_7_12 - D3.10_7_0,
    D3.11_1_diff = D3.11_1_12 - D3.11_1_0,
    D3.11_2_diff = D3.11_2_12 - D3.11_2_0
  )

df_complete_diff <- df_complete_diff %>%
  mutate(
    D3.10_1_diff = D3.10_1_12 - D3.10_1_0,
    D3.10_2_diff = D3.10_2_12 - D3.10_2_0,
    D3.10_3_diff = D3.10_3_12 - D3.10_3_0,
    D3.10_4_diff = D3.10_4_12 - D3.10_4_0,
    D3.10_5_diff = D3.10_5_12 - D3.10_5_0,
    D3.10_6_diff = D3.10_6_12 - D3.10_6_0,
    D3.10_7_diff = D3.10_7_12 - D3.10_7_0,
    D3.11_1_diff = D3.11_1_12 - D3.11_1_0,
    D3.11_2_diff = D3.11_2_12 - D3.11_2_0
  )

resource_diff_vars <- c(
  'D3.10_1_diff', 'D3.10_2_diff', 'D3.10_3_diff', 'D3.10_4_diff',
  'D3.10_5_diff', 'D3.10_6_diff', 'D3.10_7_diff',
  'D3.11_1_diff', 'D3.11_2_diff'
)

asthma_resource_diff <- generate_quant_outcomes_diff(df_asthma_diff, resource_diff_vars)
COPD_resource_diff <- generate_quant_outcomes_diff(df_COPD_diff, resource_diff_vars)
total_resource_diff <- generate_quant_outcomes_diff(df_complete_diff, resource_diff_vars)

# COSTS

df_complete <- df_complete_remake

cost_vars_premutate <- c('cost_C6', 'cost_C12', 'cost_M6', 'cost_M12', 'cost_H6', 'cost_H12', 'cost_F6', 'cost_F12', 'cost_O6', 'cost_O12')

df_complete[cost_vars_premutate] <- df_complete[cost_vars_premutate] %>%
  mutate_all(~replace(., is.na(.), 0.0)) 

df_complete <- df_complete %>%
  mutate(cost_C_total = cost_C6 + cost_C12,
         cost_M_total = cost_M6 + cost_M12,
         cost_H_total = cost_H6 + cost_H12,
         cost_F_total = cost_F6 + cost_F12,
         cost_O_total = cost_O6 + cost_O12,
         cost_total_6 = cost_C6 + cost_M6 + cost_H6 + cost_F6 + cost_O6,
         cost_total_total = cost_C_total + cost_M_total + cost_H_total + cost_F_total + cost_O_total)

df_asthma <- df_complete[df_complete$D1.3_0 == 1, ]
df_COPD <- df_complete[df_complete$D1.3_0 == 2, ]


cost_vars <- c(
  'cost_M6', 'cost_M_total', 'cost_C6', 'cost_C_total', 'cost_F6', 'cost_F_total',
  'cost_H6', 'cost_H_total', 'cost_O6', 'cost_O_total', 'cost_total_6', 'cost_total_total'
)

cost_vars_names <- c(
  'cost_M6' = 'Outpatient costs (6mo)',
  'cost_M_total' = 'Outpatient costs (12mo)',
  'cost_C6' = 'Lab costs (6mo)',
  'cost_C_total' = 'Lab costs (12mo)',
  'cost_F6' = 'Medication costs (6mo)',
  'cost_F_total' = 'Medication costs (12mo)',
  'cost_H6' = 'Med Delivery costs (6mo)',
  'cost_H_total' = 'Med Delivery costs (12mo)',
  'cost_O6' = 'Hospital costs (6mo)',
  'cost_O_total' = 'Hospital costs (12mo)',
  'cost_total_6' = 'Total NHS Costs (6mo)',
  'cost_total_total' = 'Total NHS Costs (12mo)'
)

generate_quant_outcomes_general <- function(df, outcome_vars, outcome_var_names){
  
  output_table <- data.frame(
    variable = character(),
    ig_num = numeric(),
    ig_mean = numeric(),
    ig_sd = numeric(),
    cg_num = numeric(),
    cg_mean = numeric(),
    cg_sd = numeric(),
    p_val = numeric()
  )
  
  df_ig <- df[df$D1.4_0 == "ig (intervention group)", ]
  df_cg <- df[df$D1.4_0 == "cg (control group)", ]
  
  for(i in 1:length(outcome_vars)){
    
    var <- outcome_vars[i]
    
    ig_num <- sum(!is.na(df_ig[[var]]))
    ig_mean <- mean(df_ig[[var]], na.rm = TRUE)
    ig_sd <- sd(df_ig[[var]], na.rm = TRUE)
    
    cg_num <- sum(!is.na(df_cg[[var]]))
    cg_mean <- mean(df_cg[[var]], na.rm = TRUE)
    cg_sd <- sd(df_cg[[var]], na.rm = TRUE)
    
    if(all(!is.na(df_ig[[var]])) && all(!is.na(df_ig[[var]]))){
      test <- wilcox.test(df_ig[[var]], df_cg[[var]])
      p_val <- test$p.value
    }
    else{
      p_val <- 1.0
    }
    
    
    output_table <- output_table %>%
      add_row(
        variable = outcome_var_names[var], 
        ig_num = ig_num,
        ig_mean = round(ig_mean, 2),
        ig_sd = round(ig_sd, 2),
        cg_num = cg_num,
        cg_mean = round(cg_mean, 2),
        cg_sd = round(cg_sd, 2),
        p_val = round(p_val, 4)
      )
      
  }
  return(output_table)
}

asthma_cost_outcomes <- generate_quant_outcomes_general(df_asthma, cost_vars, cost_vars_names)
COPD_cost_outcomes <- generate_quant_outcomes_general(df_COPD, cost_vars, cost_vars_names)
total_cost_outcomes <- generate_quant_outcomes_general(df_complete, cost_vars, cost_vars_names)




################################ Regressions ######################################

group_factor_levels = c('cg (control group)', 'ig (intervention group)')

## Regression 1: GLM (logit) un-adjusted for 12mo outcome

df_complete$controlled_12 <- factor(df_complete$controlled_12)
df_complete$D1.4_0 <- factor(df_complete$D1.4_0, levels = group_factor_levels)
reg1_total <- glm(formula = controlled_12 ~ D1.4_0, family = "binomial", data = df_complete)

## Regression 2: GLM (logit) for 12mo outcome, adjusted for baseline outcome, age, and sex

df_complete$controlled_0 <- factor(df_complete$controlled_0)
df_complete$D2.2_0 <- factor(df_complete$D2.2_0)
df_complete$D2.3_0 <- factor(df_complete$D2.3_0)
reg2_total <- glm(formula = controlled_12 ~ D1.4_0 + controlled_0 + D2.2_0 + D2.3_0, family = "binomial", data = df_complete)

## Regression 3+: GLM (logit) for primary outcome @ t where t e {3, 6, 9, 12}
df_complete$controlled_3 <- factor(df_complete$controlled_3)
df_complete$controlled_6 <- factor(df_complete$controlled_6)
df_complete$controlled_9 <- factor(df_complete$controlled_9)
reg3_total_3mo <- glm(formula = controlled_3 ~ D1.4_0, family = "binomial", data = df_complete)
reg3_total_6mo <- glm(formula = controlled_6 ~ D1.4_0, family = "binomial", data = df_complete)
reg3_total_9mo <- glm(formula = controlled_9 ~ D1.4_0, family = "binomial", data = df_complete)

## Regression 4+: 









#################################################################################
####################### Below is deprecated code from Lydia #####################
####################### - Ethan 24/06/24 ########################################

################# INDIVIDUAL TESTS ###############################################
# BINARY VARIABLES: PROPORTION TEST 

# Asthma  
live_alone <-c(Table1$BOFE_Asthma_m[Table1$Values=="Live alone (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Live alone (yes) (%)"])
flu_vacc <-c(Table1$BOFE_Asthma_m[Table1$Values=="Flue vaccination (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Flue vaccination (yes) (%)"])
pneu_vacc <-c(Table1$BOFE_Asthma_m[Table1$Values=="Pneumococcal vaccination"],Table1$UC_Asthma_m[Table1$Values=="Pneumococcal vaccination"])
covid <-c(Table1$BOFE_Asthma_m[Table1$Values=="COVID-19 vaccination"],Table1$UC_Asthma_m[Table1$Values=="COVID-19 vaccination"])
diabetes <- c(Table1$BOFE_Asthma_m[Table1$Values=="Diabetes (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Diabetes (yes) (%)"])
heart_d <- c(Table1$BOFE_Asthma_m[Table1$Values=="Heart disease (yes)(%)"],Table1$UC_Asthma_m[Table1$Values=="Heart disease (yes)(%)"])
other <-c(Table1$BOFE_Asthma_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Other disease (yes) (%)"])
other <-c(Table1$BOFE_Asthma_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Other disease (yes) (%)"])
covid_test <- c(Table1$BOFE_Asthma_m[Table1$Values=="Covid-19 test (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Covid-19 test (yes) (%)"])
covid_test_postive <-  c(Table1$BOFE_Asthma_m[Table1$Values=="Covid-19 positive (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Covid-19 positive (yes) (%)"])
employed <-  c(Table1$BOFE_Asthma_m[Table1$Values=="Employed (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Employed (yes) (%)"])

n_asthma <- c(247,140)
prop.test(live_alone,n_asthma)
prop.test(flu_vacc,n_asthma)
prop.test(pneu_vacc,n_asthma)
prop.test(covid,n_asthma)
prop.test(diabetes,n_asthma)
prop.test(heart_d,n_asthma)
prop.test(other,n_asthma)
prop.test(covid_test,n_asthma)
prop.test(covid_test_postive, n_asthma)
prop.test(employed, n_asthma)


asthma_controlled <- c(sum(complete_cases$ACT_controlled_0[complete_cases$D1.3_0==1 & complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$ACT_controlled_0[complete_cases$D1.3_0==1 & complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(asthma_controlled, n_asthma)

#COPD
live_alone <-c(Table1$BOFE_COPD_m[Table1$Values=="Live alone (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Live alone (yes) (%)"])
flu_vacc <-c(Table1$BOFE_COPD_m[Table1$Values=="Flue vaccination (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Flue vaccination (yes) (%)"])
pneu_vacc <-c(Table1$BOFE_COPD_m[Table1$Values=="Pneumococcal vaccination"],Table1$UC_COPD_m[Table1$Values=="Pneumococcal vaccination"])
covid <-c(Table1$BOFE_COPD_m[Table1$Values=="COVID-19 vaccination"],Table1$UC_COPD_m[Table1$Values=="COVID-19 vaccination"])
diabetes <- c(Table1$BOFE_COPD_m[Table1$Values=="Diabetes (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Diabetes (yes) (%)"])
heart_d <- c(Table1$BOFE_COPD_m[Table1$Values=="Heart disease (yes)(%)"],Table1$UC_COPD_m[Table1$Values=="Heart disease (yes)(%)"])
other <-c(Table1$BOFE_COPD_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Other disease (yes) (%)"])
other <-c(Table1$BOFE_COPD_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Other disease (yes) (%)"])
covid_test <- c(Table1$BOFE_COPD_m[Table1$Values=="Covid-19 test (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Covid-19 test (yes) (%)"])
covid_test_postive <-  c(Table1$BOFE_COPD_m[Table1$Values=="Covid-19 positive (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Covid-19 positive (yes) (%)"])
employed <-  c(Table1$BOFE_COPD_m[Table1$Values=="Employed (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Employed (yes) (%)"])


n_copd <- c(249,121)
prop.test(live_alone,n_copd)
prop.test(flu_vacc,n_copd)
prop.test(pneu_vacc,n_copd)
prop.test(covid,n_copd)
prop.test(diabetes,n_copd)
prop.test(heart_d,n_copd)
prop.test(other,n_copd)
prop.test(covid_test,n_copd)
prop.test(covid_test_postive, n_copd)
prop.test(employed, n_copd)

table(complete_cases$D1.3_0, complete_cases$D1.4_0)

copd_controlled <- c(sum(complete_cases$CCQ_controlled_0[complete_cases$D1.3_0==2 & complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$CCQ_controlled_0[complete_cases$D1.3_0==2 & complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(copd_controlled, n_copd)

#Whole sample proportion test
n_total<- c(sum(complete_cases$D1.4_0=="ig (intervention group)"),sum(complete_cases$D1.4_0=="cg (control group)"))
live_alone <-c(sum(complete_cases$D2.7_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D2.7_0[complete_cases$D1.4_0=="cg (control group)"]==1))
flu_vacc <- c(sum(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="cg (control group)"]==1))
pneu_vacc <- c(sum(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid <- c(sum(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="cg (control group)"]==1))
diabetes <-c(sum(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="cg (control group)"]==1))
heart_d <- c(sum(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="cg (control group)"]==1))
other <-c(sum(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid_test <-c(sum(complete_cases$D3.8_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.8_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid_test_postive <- c(sum(complete_cases$D3.9_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.9_0[complete_cases$D1.4_0=="cg (control group)"]==1)) 
employed <- c(sum(complete_cases$D3.12_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.12_0[complete_cases$D1.4_0=="cg (control group)"]==1)) 

prop.test(live_alone,n_total)
prop.test(flu_vacc,n_total)
prop.test(pneu_vacc,n_total)
prop.test(covid,n_total)
prop.test(diabetes,n_total)
prop.test(heart_d,n_total)
prop.test(other,n_total)
prop.test(covid_test,n_total)
prop.test(covid_test_postive, n_total)
prop.test(employed, n_total)

all_controlled <- c(sum(complete_cases$controlled_0[complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$controlled_0[complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(all_controlled, n_total)


# Num observations per group
table(complete_cases$D1.4_0, complete_cases$D1.3_0)

# Age
# Treatment and Asthma
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.3_0[complete_cases$D1.4_0=="cg (control group)"])



# Sex
# Treatment and Asthma
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.2_0[complete_cases$D1.4_0=="cg (control group)"])


# Where does the patient live? D2.1_0
# Treatment and Asthma
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.1_0[complete_cases$D1.4_0=="cg (control group)"])



# Ethnic group: D2.4_0
# Treatment and Asthma
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.4_0[complete_cases$D1.4_0=="cg (control group)"])



# What is the patient's education level? D2.5_0
# Treatment and Asthma
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.5_0[complete_cases$D1.4_0=="cg (control group)"])



# Who has selected the patient? D2.6_0
# Treatment and Asthma
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.6_0[complete_cases$D1.4_0=="cg (control group)"])




# Does the patient live alone? D2.7_0
# Treatment and Asthma
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D2.7_0[complete_cases$D1.4_0=="cg (control group)"])


#Forced vital capacity (FVC) OPTIONAL
#Forced expiratory volume in 1 second (FEV1) OPTIONAL

# Height D3.1_0
hist(complete_cases$D3.1_0)
table(complete_cases$D3.1_0)
#Recode values in cm to m: 152  156  158  159  160  162  163  165  168  170  174  175  180
complete_cases$D3.1_0 <- ifelse(complete_cases$D3.1_0>100, complete_cases$D3.1_0/100,complete_cases$D3.1_0)

# Treatment and Asthma
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)"])

# Weight D3.2_0

# Treatment and Asthma
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)"])

# BMI D3.3_0
sum(is.na(complete_cases$D3.3_0))
# Treatment and Asthma
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)"])



# Flue vaccination D3.5_1_0

# Treatment and Asthma
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="cg (control group)"])


# Pneumococcal vaccination D3.5_2_0

# Treatment and Asthma
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="cg (control group)"])

# COVID-19 vaccination D3.5_3_0

# Treatment and Asthma
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="cg (control group)"])


# Does the patient smoke? D3.6_0

# Treatment and Asthma
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.6_0[complete_cases$D1.4_0=="cg (control group)"])


# Diabetes
# Treatment and Asthma
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="cg (control group)"])



# Heart disease
# Treatment and Asthma
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="cg (control group)"])



# Other
# Treatment and Asthma
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="cg (control group)"])



# Covid test 
# Treatment and Asthma
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.8_0[complete_cases$D1.4_0=="cg (control group)"])


# Was the patient positive for covid? D3.9_0
# Treatment and Asthma
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.9_0[complete_cases$D1.4_0=="cg (control group)"])


# D3.10_1_0
# Treatment and Asthma
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)"])




# D3.10_2_0
# Treatment and Asthma
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)"])



# D3.10_3_0
# Treatment and Asthma
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)"])



# D3.10_4_0 
# Treatment and Asthma
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)"])


# D3.10_5_0  
# Treatment and Asthma
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)"])



# D3.10_6_0
# Treatment and Asthma
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)"])



# D3.10_7_0
# Treatment and Asthma
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)"])



#  D3.11_1_0
# Treatment and Asthma
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)"])


# D3.11_2_0
# Treatment and Asthma
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)" ])
#Usual care whole sample
mean(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)"])
sd(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)"])


# Are you currently employed? D3.12_0
# Treatment and Asthma
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D3.12_0[complete_cases$D1.4_0=="cg (control group)"])


# Absenteeism D3.13_0

# Treatment and Asthma
mean(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(na.omit(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$D3.13_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#Usual care whole sample
mean(na.omit(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)"]))
sd(na.omit(complete_cases$D3.13_0[complete_cases$D1.4_0=="cg (control group)"]))


# 	ACT.SCORE_0
# Treatment and Asthma
mean(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Usual care and Asthma
mean(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])

#Treatment whole sample
mean(na.omit(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#Usual care whole sample
mean(na.omit(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="cg (control group)"]))
sd(na.omit(complete_cases$ACT.SCORE_0[complete_cases$D1.4_0=="cg (control group)"]))



# CCQ SCORE
# Treatment and COPD
mean(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and COPD
mean(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(na.omit(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#Usual care whole sample
mean(na.omit(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="cg (control group)"]))
sd(na.omit(complete_cases$CCQ.SCORE_0[complete_cases$D1.4_0=="cg (control group)"]))


#CCQ - SYMPTOMS
#treatment group
mean(na.omit(complete_cases$CCQ.symptom_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$CCQ.symptom_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#control group
mean(na.omit(complete_cases$CCQ.symptom_0[complete_cases$D1.4_0=="cg (control group)" ]))
sd(na.omit(complete_cases$CCQ.symptom_0[complete_cases$D1.4_0=="cg (control group)" ]))

#CCQ - ACTIVITIES
#treatment group
mean(na.omit(complete_cases$CCQ.functional_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$CCQ.functional_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#control group
mean(na.omit(complete_cases$CCQ.functional_0[complete_cases$D1.4_0=="cg (control group)" ]))
sd(na.omit(complete_cases$CCQ.functional_0[complete_cases$D1.4_0=="cg (control group)" ]))

#CCQ - MENTAL
#treatment group
mean(na.omit(complete_cases$CCQ.mental_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$CCQ.mental_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#control group
mean(na.omit(complete_cases$CCQ.mental_0[complete_cases$D1.4_0=="cg (control group)" ]))
sd(na.omit(complete_cases$CCQ.mental_0[complete_cases$D1.4_0=="cg (control group)" ]))


# EQindex_0
table(complete_cases$EQindex)
# Treatment and Asthma
mean(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])

#Treatment whole sample
mean(na.omit(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#Usual care whole sample
mean(na.omit(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)"]))
sd(na.omit(complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)"]))


# Active ingredients D5.2_0

# Treatment and Asthma
mean(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
mean(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
mean(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
sd(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
mean(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])
sd(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
mean(na.omit(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
sd(na.omit(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)" ]))
#Usual care whole sample
mean(na.omit(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)"]))
sd(na.omit(complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)"]))


# D5.5_0
# Treatment and Asthma
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D5.5_0[complete_cases$D1.4_0=="cg (control group)"])




# D5.6_0 
# Treatment and Asthma
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==1])
# Treatment and COPD
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="ig (intervention group)" & complete_cases$D1.3_0==2])
# Usual care and Asthma
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==1])
# Usual care and COPD
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="cg (control group)" & complete_cases$D1.3_0==2])


#Treatment whole sample
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="ig (intervention group)"])
#Usual care whole sample
table(complete_cases$D5.6_0[complete_cases$D1.4_0=="cg (control group)"])



######################################

# T-TEST: CONTINUOUS VARIABLES

# ASTHMA D1.3_0==1

t.test(D3.1_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.2_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_1_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_2_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_3_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_4_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_5_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_6_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.10_7_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.11_1_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.11_2_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)

#Absenteeism
table(complete_cases$D3.12_0)
table(complete_cases$D3.13_0)
complete_cases$D3.13_0[complete_cases$D3.12_0==0] <- NA
complete_cases$D3.13_3[complete_cases$D3.12_3==0] <- NA
complete_cases$D3.13_6[complete_cases$D3.12_6==0] <- NA
complete_cases$D3.13_9[complete_cases$D3.12_9==0] <- NA
complete_cases$D3.13_12[complete_cases$D3.12_12==0] <- NA

t.test(D3.13_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
hist(complete_cases$D3.13_0)
#

t.test(ACT.SCORE_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(EQindex_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D5.2_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)
t.test(D3.3_0[D1.3_0==1] ~ D1.4_0[D1.3_0==1], data=complete_cases)

# COPD D1.3_0==2

t.test(D3.1_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.2_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_1_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_2_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_3_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_4_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_5_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_6_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.10_7_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.11_1_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.11_2_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)

#Absenteeism
table(complete_cases$D3.12_0)
table(complete_cases$D3.13_0)
complete_cases$D3.13_0[complete_cases$D3.12_0==0] <- NA
complete_cases$D3.13_3[complete_cases$D3.12_3==0] <- NA
complete_cases$D3.13_6[complete_cases$D3.12_6==0] <- NA
complete_cases$D3.13_9[complete_cases$D3.12_9==0] <- NA
complete_cases$D3.13_12[complete_cases$D3.12_12==0] <- NA


t.test(D3.13_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(CCQ.SCORE_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(EQindex_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D5.2_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(D3.3_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)

t.test(CCQ.symptom_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(CCQ.functional_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)
t.test(CCQ.mental_0[D1.3_0==2] ~ D1.4_0[D1.3_0==2], data=complete_cases)


#Whole sample

t.test(D3.1_0 ~ D1.4_0, data=complete_cases)
t.test(D3.2_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_1_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_2_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_3_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_4_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_5_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_6_0 ~ D1.4_0, data=complete_cases)
t.test(D3.10_7_0 ~ D1.4_0, data=complete_cases)
t.test(D3.11_1_0 ~ D1.4_0, data=complete_cases)
t.test(D3.11_2_0 ~ D1.4_0, data=complete_cases)
#t.test(ACT.SCORE_0 ~ D1.4_0, data=complete_cases)
t.test(CCQ.SCORE_0 ~ D1.4_0, data=complete_cases)
t.test(EQindex_0 ~ D1.4_0, data=complete_cases)
t.test(D5.2_0 ~ D1.4_0, data=complete_cases)
t.test(D3.3_0 ~ D1.4_0, data=complete_cases)

######################################

setwd('C:/Users/lydiap/OneDrive - Nexus365/BOFE Project/Tables and diagrams')

Table1  <- read.xlsx('Table1.xlsx', sheet="Test", startRow=1)

# BINARY VARIABLES: PROPORTION TEST 

# Asthma  
live_alone <-c(Table1$BOFE_Asthma_m[Table1$Values=="Live alone (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Live alone (yes) (%)"])
flu_vacc <-c(Table1$BOFE_Asthma_m[Table1$Values=="Flue vaccination (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Flue vaccination (yes) (%)"])
pneu_vacc <-c(Table1$BOFE_Asthma_m[Table1$Values=="Pneumococcal vaccination"],Table1$UC_Asthma_m[Table1$Values=="Pneumococcal vaccination"])
covid <-c(Table1$BOFE_Asthma_m[Table1$Values=="COVID-19 vaccination"],Table1$UC_Asthma_m[Table1$Values=="COVID-19 vaccination"])
diabetes <- c(Table1$BOFE_Asthma_m[Table1$Values=="Diabetes (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Diabetes (yes) (%)"])
heart_d <- c(Table1$BOFE_Asthma_m[Table1$Values=="Heart disease (yes)(%)"],Table1$UC_Asthma_m[Table1$Values=="Heart disease (yes)(%)"])
other <-c(Table1$BOFE_Asthma_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Other disease (yes) (%)"])
other <-c(Table1$BOFE_Asthma_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Other disease (yes) (%)"])
covid_test <- c(Table1$BOFE_Asthma_m[Table1$Values=="Covid-19 test (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Covid-19 test (yes) (%)"])
covid_test_postive <-  c(Table1$BOFE_Asthma_m[Table1$Values=="Covid-19 positive (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Covid-19 positive (yes) (%)"])
employed <-  c(Table1$BOFE_Asthma_m[Table1$Values=="Employed (yes) (%)"],Table1$UC_Asthma_m[Table1$Values=="Employed (yes) (%)"])

n_asthma <- c(247,140)
prop.test(live_alone,n_asthma)
prop.test(flu_vacc,n_asthma)
prop.test(pneu_vacc,n_asthma)
prop.test(covid,n_asthma)
prop.test(diabetes,n_asthma)
prop.test(heart_d,n_asthma)
prop.test(other,n_asthma)
prop.test(covid_test,n_asthma)
prop.test(covid_test_postive, n_asthma)
prop.test(employed, n_asthma)


asthma_controlled <- c(sum(complete_cases$ACT_controlled_0[complete_cases$D1.3_0==1 & complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$ACT_controlled_0[complete_cases$D1.3_0==1 & complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(asthma_controlled, n_asthma)

#COPD
live_alone <-c(Table1$BOFE_COPD_m[Table1$Values=="Live alone (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Live alone (yes) (%)"])
flu_vacc <-c(Table1$BOFE_COPD_m[Table1$Values=="Flue vaccination (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Flue vaccination (yes) (%)"])
pneu_vacc <-c(Table1$BOFE_COPD_m[Table1$Values=="Pneumococcal vaccination"],Table1$UC_COPD_m[Table1$Values=="Pneumococcal vaccination"])
covid <-c(Table1$BOFE_COPD_m[Table1$Values=="COVID-19 vaccination"],Table1$UC_COPD_m[Table1$Values=="COVID-19 vaccination"])
diabetes <- c(Table1$BOFE_COPD_m[Table1$Values=="Diabetes (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Diabetes (yes) (%)"])
heart_d <- c(Table1$BOFE_COPD_m[Table1$Values=="Heart disease (yes)(%)"],Table1$UC_COPD_m[Table1$Values=="Heart disease (yes)(%)"])
other <-c(Table1$BOFE_COPD_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Other disease (yes) (%)"])
other <-c(Table1$BOFE_COPD_m[Table1$Values=="Other disease (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Other disease (yes) (%)"])
covid_test <- c(Table1$BOFE_COPD_m[Table1$Values=="Covid-19 test (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Covid-19 test (yes) (%)"])
covid_test_postive <-  c(Table1$BOFE_COPD_m[Table1$Values=="Covid-19 positive (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Covid-19 positive (yes) (%)"])
employed <-  c(Table1$BOFE_COPD_m[Table1$Values=="Employed (yes) (%)"],Table1$UC_COPD_m[Table1$Values=="Employed (yes) (%)"])


n_copd <- c(249,121)
prop.test(live_alone,n_copd)
prop.test(flu_vacc,n_copd)
prop.test(pneu_vacc,n_copd)
prop.test(covid,n_copd)
prop.test(diabetes,n_copd)
prop.test(heart_d,n_copd)
prop.test(other,n_copd)
prop.test(covid_test,n_copd)
prop.test(covid_test_postive, n_copd)
prop.test(employed, n_copd)

table(complete_cases$D1.3_0, complete_cases$D1.4_0)

copd_controlled <- c(sum(complete_cases$CCQ_controlled_0[complete_cases$D1.3_0==2 & complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$CCQ_controlled_0[complete_cases$D1.3_0==2 & complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(copd_controlled, n_copd)

#Whole sample proportion test
n_total <- c(496,261)
live_alone <-c(sum(complete_cases$D2.7_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D2.7_0[complete_cases$D1.4_0=="cg (control group)"]==1))
flu_vacc <- c(sum(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_1_0[complete_cases$D1.4_0=="cg (control group)"]==1))
pneu_vacc <- c(sum(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_2_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid <- c(sum(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.5_3_0[complete_cases$D1.4_0=="cg (control group)"]==1))
diabetes <-c(sum(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_1_0[complete_cases$D1.4_0=="cg (control group)"]==1))
heart_d <- c(sum(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_2_0[complete_cases$D1.4_0=="cg (control group)"]==1))
other <-c(sum(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.7_3_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid_test <-c(sum(complete_cases$D3.8_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.8_0[complete_cases$D1.4_0=="cg (control group)"]==1))
covid_test_postive <- c(sum(complete_cases$D3.9_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.9_0[complete_cases$D1.4_0=="cg (control group)"]==1)) 
employed <- c(sum(complete_cases$D3.12_0[complete_cases$D1.4_0=="ig (intervention group)"]==1), sum(complete_cases$D3.12_0[complete_cases$D1.4_0=="cg (control group)"]==1)) 

prop.test(live_alone,n_total)
prop.test(flu_vacc,n_total)
prop.test(pneu_vacc,n_total)
prop.test(covid,n_total)
prop.test(diabetes,n_total)
prop.test(heart_d,n_total)
prop.test(other,n_total)
prop.test(covid_test,n_total)
prop.test(covid_test_postive, n_total)
prop.test(employed, n_total)

all_controlled <- c(sum(complete_cases$controlled_0[complete_cases$D1.4_0=="ig (intervention group)"]==1),sum(complete_cases$controlled_0[complete_cases$D1.4_0=="cg (control group)"]==1))
prop.test(all_controlled, n_total)


######################################

# Chi-square and Fisher tests:

#Asthma: D1.3_0	Pathology =1

#age D2.3_0
age_table<-table(complete_cases$D2.3_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_age <- chisq.test(age_table) #pvalue=0.8811
fisher_test_age <- fisher.test(age_table)  #pvalue= 0.8771
rm(fisher_test_age)
#sex D2.2_0
sex_table<-table(complete_cases$D2.2_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_sex <- chisq.test(sex_table) #pvalue= 0.2592
fisher_test_sex <- fisher.test(sex_table)  #pvalue= 0.2383

#region D2.1_0
region_table<-table(complete_cases$D2.1_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_region <- chisq.test(region_table, simulate.p.value=TRUE) #pvalue=  0.912
fisher_test_region <- fisher.test(region_table, simulate.p.value=TRUE)  #pvalue= 0.901

#ethnic group D2.4_0
ethnic_table<-table(complete_cases$D2.4_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_ethnic <- chisq.test(ethnic_table, simulate.p.value=TRUE) #pvalue=  0.8356
fisher_test_ethnic <- fisher.test(ethnic_table, simulate.p.value=TRUE)  #pvalue= 0.7246
rm(fisher_test_ethnic)
#education D2.5_0
educ_table<-table(complete_cases$D2.5_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_educ <- chisq.test(educ_table, simulate.p.value=TRUE) #pvalue=  0.8136
fisher_test_educ <- fisher.test(educ_table, simulate.p.value=TRUE)  #pvalue= 0.6777
rm(fisher_test_educ)
#selection process D2.6_0
selec_table<-table(complete_cases$D2.6_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_selec <- chisq.test(selec_table, simulate.p.value=TRUE) #pvalue=  0.5882
fisher_test_selec <- fisher.test(selec_table, simulate.p.value=TRUE)  #pvalue= 0.5984
rm(fisher_test_selec)
#smoking D3.6_0
smoking_table<-table(complete_cases$D3.6_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_smoking <- chisq.test(smoking_table, simulate.p.value=TRUE) #pvalue=  0.6472
fisher_test_smoking <- fisher.test(smoking_table, simulate.p.value=TRUE)  #pvalue= 0.6482
rm(fisher_test_smoking)

#problems with the medication D5.5_0
prob_table<-table(complete_cases$D5.5_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_prob <- chisq.test(prob_table, simulate.p.value=TRUE) #pvalue=  0.01299
fisher_test_prob <- fisher.test(prob_table, simulate.p.value=TRUE)  #pvalue= 0.006497
rm(fisher_test_prob)

#knowledge of the medication D5.6_0 
knowledge_table<-table(complete_cases$D5.6_0[complete_cases$D1.3_0==1],complete_cases$D1.4_0[complete_cases$D1.3_0==1])
chisq_test_know <- chisq.test(knowledge_table, simulate.p.value=TRUE) #pvalue=  0.6662
fisher_test_know <- fisher.test(knowledge_table, simulate.p.value=TRUE)  #pvalue= 0.6117
rm(fisher_test_know)


#COPD: D1.3_0	Pathology =2

#age D2.3_0
attr(age_df$D2.3_0, "labels") <- NULL # remove label 1 (no observations)
age_df <-complete_cases[complete_cases$D2.3_0!=1, ]
age_df <-complete_cases[complete_cases$D1.3_0==2, ]
age_df$D2.3_0 <- as.factor(age_df$D2.3_0)
age_table<-table(age_df$D2.3_0,age_df$D1.4_0)
chisq_test_age <- chisq.test(age_table, simulate.p.value=TRUE) #pvalue=0.5407
fisher_test_age <- fisher.test(age_table,  simulate.p.value=TRUE)  #pvalue= 0.5137

#sex D2.2_0
sex_table<-table(complete_cases$D2.2_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_sex <- chisq.test(sex_table) #pvalue= 0.9756
fisher_test_sex <- fisher.test(sex_table)  #pvalue= 0.9046

#region D2.1_0
region_table<-table(complete_cases$D2.1_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_region <- chisq.test(region_table, simulate.p.value=TRUE) #pvalue=  0.989
fisher_test_region <- fisher.test(region_table, simulate.p.value=TRUE)  #pvalue= 0.9785

#ethnic group D2.4_0
ethnic_table<-table(complete_cases$D2.4_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_ethnic <- chisq.test(ethnic_table, simulate.p.value=TRUE) #pvalue=  0.5647
fisher_test_ethnic <- fisher.test(ethnic_table, simulate.p.value=TRUE)  #pvalue= 0.5437

#education D2.5_0
educ_table<-table(complete_cases$D2.5_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_educ <- chisq.test(educ_table, simulate.p.value=TRUE) #pvalue=  0.4208
fisher_test_educ <- fisher.test(educ_table, simulate.p.value=TRUE)  #pvalue= 0.3748

#selection process D2.6_0
selec_table<-table(complete_cases$D2.6_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_selec <- chisq.test(selec_table, simulate.p.value=TRUE) #pvalue=  0.6332
fisher_test_selec <- fisher.test(selec_table, simulate.p.value=TRUE)  #pvalue= 0.6467

#smoking D3.6_0
smoking_table<-table(complete_cases$D3.6_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_smoking <- chisq.test(smoking_table) #pvalue=  0.5677
fisher_test_smoking <- fisher.test(smoking_table)  #pvalue= 0.5651

#problems with the medication D5.5_0
prob_table<-table(complete_cases$D5.5_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_prob <- chisq.test(prob_table, simulate.p.value=TRUE) #pvalue=  1
fisher_test_prob <- fisher.test(prob_table, simulate.p.value=TRUE)  #pvalue= 1

#knowledge of the medication D5.6_0 
knowledge_table<-table(complete_cases$D5.6_0[complete_cases$D1.3_0==2],complete_cases$D1.4_0[complete_cases$D1.3_0==2])
chisq_test_know <- chisq.test(knowledge_table, simulate.p.value=TRUE) #pvalue=  0.4358
fisher_test_know <- fisher.test(knowledge_table, simulate.p.value=TRUE)  #pvalue= 0.3778


#Total patients: 

#age D2.3_0
age_table<-table(complete_cases$D2.3_0,complete_cases$D1.4_0)
chisq_test_age <- chisq.test(age_table, simulate.p.value=TRUE) #pvalue=0.6342
fisher_test_age <- fisher.test(age_table, simulate.p.value=TRUE)  #pvalue=0.6302

#sex D2.2_0
sex_table<-table(complete_cases$D2.2_0,complete_cases$D1.4_0)
chisq_test_sex <- chisq.test(sex_table) #pvalue= 0.5347
fisher_test_sex <- fisher.test(sex_table)  #pvalue= 0.49

#region D2.1_0
region_table<-table(complete_cases$D2.1_0,complete_cases$D1.4_0)
chisq_test_region <- chisq.test(region_table, simulate.p.value=TRUE) #pvalue= 0.992 
fisher_test_region <- fisher.test(region_table, simulate.p.value=TRUE)  #pvalue= 0.9855

#ethnic group D2.4_0
ethnic_table<-table(complete_cases$D2.4_0,complete_cases$D1.4_0)
chisq_test_ethnic <- chisq.test(ethnic_table, simulate.p.value=TRUE) #pvalue=0.8286  
fisher_test_ethnic <- fisher.test(ethnic_table, simulate.p.value=TRUE)  #pvalue=0.7291 

#education D2.5_0
educ_table<-table(complete_cases$D2.5_0,complete_cases$D1.4_0)
chisq_test_educ <- chisq.test(educ_table, simulate.p.value=TRUE) #pvalue= 0.4583 
fisher_test_educ <- fisher.test(educ_table, simulate.p.value=TRUE)  #pvalue= 0.4268

#selection process D2.6_0
selec_table<-table(complete_cases$D2.6_0,complete_cases$D1.4_0)
chisq_test_selec <- chisq.test(selec_table, simulate.p.value=TRUE) #pvalue=1  
fisher_test_selec <- fisher.test(selec_table, simulate.p.value=TRUE)  #pvalue= 1

#smoking D3.6_0
smoking_table<-table(complete_cases$D3.6_0,complete_cases$D1.4_0)
chisq_test_smoking <- chisq.test(smoking_table, simulate.p.value=TRUE) #pvalue=  0.4483
fisher_test_smoking <- fisher.test(smoking_table, simulate.p.value=TRUE)  #pvalue=0.4323 

#problems with the medication D5.5_0
prob_table<-table(complete_cases$D5.5_0,complete_cases$D1.4_0)
chisq_test_prob <- chisq.test(prob_table, simulate.p.value=TRUE) #pvalue= 0.1749 
fisher_test_prob <- fisher.test(prob_table, simulate.p.value=TRUE)  #pvalue= 0.1389


#knowledge of the medication D5.6_0 
knowledge_table<-table(complete_cases$D5.6_0,complete_cases$D1.4_0)
chisq_test_know <- chisq.test(knowledge_table, simulate.p.value=TRUE) #pvalue=0.2899  
fisher_test_know <- fisher.test(knowledge_table, simulate.p.value=TRUE)  #pvalue= 0.2864





#####################################
# Apply the tests to the baseline charactersitics in Table 1 on BMJ. 
n_male <- c(280,305)
n_total <- c(554,532)
prop.test(n_male,n_total)


mean1 <- 1.5 
sd1 <- 1.2 
n1 <- 553 

mean2 <- 1.3 
sd2 <- 1.2 
n2 <- 531 

# Compute the standard errors of the means 
se1 <- sd1 / sqrt(n1) 
se2 <- sd2 / sqrt(n2) 
# Compute the pooled standard deviation 
sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)) 
# Compute the t-statistic 
t_stat <- (mean1 - mean2) / (sp * sqrt(1/n1 + 1/n2)) 
# Degrees of freedom 
df <- n1 + n2 - 2 
# Compute the p-value 
p_value <- 2 * pt(-abs(t_stat), df) 
# Output the results 
cat("t-statistic:", t_stat, "\n") 
cat("Degrees of freedom:", df, "\n") 
cat("p-value:", p_value, "\n")



################# Wilcoxon rank-sum test (Mann-whitney U test)

ordinal_continuous_vars <- c("D5.5_0", "D1.4_0", "D2.3_0", "D3.1_0", "D3.2_0", "D3.3_0", "D3.10_1_0", "D3.10_2_0", "D3.10_3_0", 
                     "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", "D3.10_7_0", "D3.11_1_0", 
                     "D3.11_2_0", "ACT.SCORE_0", "CCQ.SCORE_0", "CCQ.symptom_0", "CCQ.functional_0", "CCQ.mental_0", "EQindex_0", "D5.2_0")

test_df1 <- complete_cases[complete_cases$D1.3_0==1,ordinal_continuous_vars] #Asthma
test_df2 <- complete_cases[complete_cases$D1.3_0==2,ordinal_continuous_vars] #COPD

#wilcox.test(df1$variable1[intervention], df1$variable1[control])
# Asthma (pathology=1)
wilcox.test(test_df1$D2.3_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D2.3_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.1_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.1_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.2_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.2_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.3_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.3_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_1_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_1_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_2_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_2_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_3_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_3_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_4_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_4_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_5_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_5_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.10_6_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_6_0[test_df1$D1.4_0=="cg (control group)"])
##Significant at 90%ci level
wilcox.test(test_df1$D3.10_7_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.10_7_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.11_1_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.11_1_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D3.11_2_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D3.11_2_0[test_df1$D1.4_0=="cg (control group)"])
##Significant at 95%ci level
wilcox.test(test_df1$ACT.SCORE_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$ACT.SCORE_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$EQindex_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$EQindex_0[test_df1$D1.4_0=="cg (control group)"])
wilcox.test(test_df1$D5.2_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D5.2_0[test_df1$D1.4_0=="cg (control group)"])
#If I treat problems as ordinal var (ranked data)
wilcox.test(test_df1$D5.5_0[test_df1$D1.4_0=="ig (intervention group)"],test_df1$D5.5_0[test_df1$D1.4_0=="cg (control group)"])

# COPD (pathology=2)
wilcox.test(test_df2$D2.3_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D2.3_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.1_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.1_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.2_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.2_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.3_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.3_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_1_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_1_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_2_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_2_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_3_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_3_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_4_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_4_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_5_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_5_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_6_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_6_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.10_7_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.10_7_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.11_1_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.11_1_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D3.11_2_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D3.11_2_0[test_df2$D1.4_0=="cg (control group)"])
##Significant at 95%ci level
wilcox.test(test_df2$CCQ.SCORE_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$CCQ.SCORE_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$EQindex_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$EQindex_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D5.2_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D5.2_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$D5.5_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$D5.5_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$CCQ.symptom_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$CCQ.symptom_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$CCQ.functional_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$CCQ.functional_0[test_df2$D1.4_0=="cg (control group)"])
wilcox.test(test_df2$CCQ.mental_0[test_df2$D1.4_0=="ig (intervention group)"],test_df2$CCQ.mental_0[test_df2$D1.4_0=="cg (control group)"])


#normality in distributions (Therapists visits, CCQ score
hist(test_df2$CCQ.SCORE_0)
hist(test_df1$ACT.SCORE_0)
hist(test_df1$D3.10_3_0)
hist(test_df2$EQindex_0)

#Whole sample
wilcox.test(complete_cases$D2.3_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D2.3_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.1_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.1_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.2_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.2_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.3_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.3_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_1_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_1_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_2_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_2_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_3_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_3_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_4_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_4_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_5_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_5_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_6_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_6_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.10_7_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.10_7_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.11_1_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.11_1_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D3.11_2_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D3.11_2_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$EQindex_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$EQindex_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D5.2_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.2_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D5.5_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.5_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D5.7_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.7_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D5.8_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.8_0[complete_cases$D1.4_0=="cg (control group)"])
wilcox.test(complete_cases$D5.9_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.9_0[complete_cases$D1.4_0=="cg (control group)"])
`wilcox.test(complete_cases$D5.10_0[complete_cases$D1.4_0=="ig (intervention group)"],complete_cases$D5.10_0[complete_cases$D1.4_0=="cg (control group)"])
`

######PREVIOUS TESTS

complete_cases$asthma <-ifelse(complete_cases$D1.3_0==1, 1, 0)
chisq_test_age <- complete_cases %>%
                      group_by(D2.3_0) %>%
                      summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
                                p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chi_square_results2 <- complete_cases %>%  
  group_by(D2.3_0) %>%  
  summarise(    
    chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
    p_value = chisq.test(table(D1.4_0, asthma))$p.value,    
    intervention_asthma = sum(asthma[D1.4_0 == "ig (intervention group)"]),    
    control_asthma = sum(asthma[D1.4_0 == "cg (control group)"]),   
    intervention_total = sum(D1.4_0 == "ig (intervention group)"),    
    control_total = sum(D1.4_0 == "cg (control group)"))

chisq_test_sex <- complete_cases %>%
  group_by(D2.2_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_reg <- complete_cases %>%
  group_by(D2.1_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_eth <- complete_cases %>%
  group_by(D2.4_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_edu <- complete_cases %>%
  group_by(D2.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_edu <- complete_cases %>%
  group_by(D2.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_sel <- complete_cases %>%
  group_by(D2.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_smo <- complete_cases %>%
  group_by(D3.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_pro <- complete_cases %>%
  group_by(D5.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

chisq_test_kno <- complete_cases %>%
  group_by(D5.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, asthma))),
            p_value = chisq.test(table(D1.4_0, asthma))$p.value)

#COPD
complete_cases$copd <-ifelse(complete_cases$D1.3_0==2, 1, 0)


chisq_test_age <- complete_cases %>%
  group_by(D2.3_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_sex <- complete_cases %>%
  group_by(D2.2_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_reg <- complete_cases %>%
  group_by(D2.1_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_eth <- complete_cases %>%
  group_by(D2.4_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_edu <- complete_cases %>%
  group_by(D2.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_edu <- complete_cases %>%
  group_by(D2.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_sel <- complete_cases %>%
  group_by(D2.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_smo <- complete_cases %>%
  group_by(D3.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_pro <- complete_cases %>%
  group_by(D5.5_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

chisq_test_kno <- complete_cases %>%
  group_by(D5.6_0) %>%
  summarise(chi_square_statistic = list(chisq.test(table(D1.4_0, copd))),
            p_value = chisq.test(table(D1.4_0, copd))$p.value)

#Asthma
Table1$Var <- NA
Table1$Var <- ifelse(Table1$Values=='Age1'|Table1$Values=='Age2'|Table1$Values=='Age3'|Table1$Values=='Age4'|Table1$Values=='Age5'|Table1$Values=='Age6'|Table1$Values=='Age7', "Age", NA)
table_age_BOFE <- table(Table1$Var[Table1$Var=="Age"], Table1$BOFE_Asthma_m[Table1$Var=="Age"] & Table1$UC_Asthma_m[Table1$Var=="Age"])
table_age <- cbind()
table_region <- table(complete_cases$D2.1_0[complete_cases$D1.3_0==1], complete_cases$D2.3_0[complete_cases$D1.3_0==1])
table_ethnic <- table(complete_cases$D2.4_0[complete_cases$D1.3_0==1], complete_cases$D2.3_0[complete_cases$D1.3_0==1])


chisq.test(table_age)
fisher.test(table_age)
chisq.test(table_region)
chisq.test(table_ethnic)



########################################################
categorical_vars <- c("D2.1_0", "D2.2_0", "D2.4_0", "D2.5_0", "D2.6_0", "D3.6_0", 
                      "D5.5_0", "D5.6_0", "D5.7_0", "D5.8_0", "D5.9_0", "D5.10_0")

binary_vars <- c("D2.3_0", "D2.7_0", "D3.5_1_0", "D3.5_2_0", "D3.5_3_0", "D3.7_1_0", 
                 "D3.7_2_0", "D3.7_3_0", "D3.8_0", "D3.9_0", "D3.12_0", "D5.3_1_0", 
                 "D5.3_2_0", "D5.3_3_0", "D5.3_4_0", "D5.3_5_0", "D5.3_6_0", "D5.3_7_0", 
                 "D5.3_8_0", "D5.3_9_0", "D5.3_10_0", "D5.4_1_0", "D5.4_2_0", "D5.4_3_0", 
                 "D5.4_4_0", "D5.12_1_0", "D5.12_2_0", "D5.12_3_0", "D5.12_4_0", 
                 "D5.12_5_0", "D5.12_6_0", "D5.13_1_0", "D5.13_2_0", "D5.13_3_0", 
                 "D5.13_4_0", "D5.13_5_0", "D5.13_6_0", "D5.13_7_0", "D5.13_8_0", 
                 "D5.13_9_0", "D5.13_10_0", "D5.13_11_0", "D5.13_12_0", "D5.13_13_0", 
                 "D5.13_14_0", "D5.14_1_0", "D5.14_2_0", "D5.14_3_0", "D5.14_4_0", 
                 "D5.14_5_0", "D5.14_6_0", "D5.14_7_0", "D5.15_1_0", "D5.15_2_0", 
                 "D5.15_3_0", "D5.15_4_0", "D5.15_5_0", "D5.15_6_0", "D5.15_7_0", 
                 "D5.15_8_0", "D5.15_9_0", "D5.16_1_0", "D5.16_2_0", "D5.16_3_0", 
                 "D5.16_4_0", "D5.16_5_0", "D5.16_6_0", "D5.16_7_0", "D5.16_8_0", 
                 "D5.17_1_0", "D5.17_2_0", "D5.17_3_0", "D5.17_4_0", "D5.17_5_0", 
                 "D5.17_6_0", "D5.17_7_0", "D5.17_8_0")

continuous_vars <- c("D3.1_0", "D3.2_0", "D3.10_1_0", "D3.10_2_0", "D3.10_3_0", 
                     "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", "D3.10_7_0", "D3.11_1_0", 
                     "D3.11_2_0", "ACT.SCORE_0", "CCQ.SCORE_0", "EQindex_0", "D5.2_0")


# Define variable lists
continuous_vars <- c("D3.1_0", "D3.2_0", "D3.10_1_0", "D3.10_2_0", "D3.10_3_0", 
                     "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", "D3.10_7_0", "D3.11_1_0", 
                     "D3.11_2_0", "ACT.SCORE_0", "CCQ.SCORE_0", "EQindex_0", "D5.2_0")




# Categorical:
D2.1_0
D2.2_0
D2.4_0
D2.5_0
D2.6_0
D3.6_0
D5.5_0
D5.6_0
D5.7_0
D5.8_0
D5.9_0
D5.10_0




# Binary
D2.3_0
D2.7_0
D3.5_1_0
D3.5_2_0
D3.5_3_0
D3.7_1_0
D3.7_2_0
D3.7_3_0
D3.8_0
D3.9_0
D3.12_0
D5.3_1_0
D5.3_2_0
D5.3_3_0
D5.3_4_0
D5.3_5_0
D5.3_6_0
D5.3_7_0
D5.3_8_0
D5.3_9_0
D5.3_10_0
D5.4_1_0
D5.4_2_0
D5.4_3_0
D5.4_4_0
D5.12_1_0
D5.12_2_0
D5.12_3_0
D5.12_4_0
D5.12_5_0
D5.12_6_0
D5.13_1_0
D5.13_2_0
D5.13_3_0
D5.13_4_0
D5.13_5_0
D5.13_6_0
D5.13_7_0
D5.13_8_0
D5.13_9_0
D5.13_10_0
D5.13_11_0
D5.13_12_0
D5.13_13_0
D5.13_14_0
D5.14_1_0
D5.14_2_0
D5.14_3_0
D5.14_4_0
D5.14_5_0
D5.14_6_0
D5.14_7_0
D5.15_1_0
D5.15_2_0
D5.15_3_0
D5.15_4_0
D5.15_5_0
D5.15_6_0
D5.15_7_0
D5.15_8_0
D5.15_9_0
D5.16_1_0
D5.16_2_0
D5.16_3_0
D5.16_4_0
D5.16_5_0
D5.16_6_0
D5.16_7_0
D5.16_8_0
D5.17_1_0
D5.17_2_0
D5.17_3_0
D5.17_4_0
D5.17_5_0
D5.17_6_0
D5.17_7_0
D5.17_8_0

# Continuous
D3.1_0 #Problema amb la codificacio
D3.2_0
D3.10_1_0
D3.10_2_0
D3.10_3_0
D3.10_4_0
D3.10_5_0
D3.10_6_0
D3.10_7_0
D3.11_1_0
D3.11_2_0
ACT.SCORE_0
CCQ.SCORE_0
EQindex_0
D5.2_0



######################################################
#### FIGURE 1: NUMBER OF PATIENTS BY STUDY PERIOD ####
######################################################


table(T0$D1.4_0,T0$D1.3_0)
table(T3$D1.4_3)
table(T3$D1.4_3,T3$D1.3_3)
table(T6$D1.4_6)
table(T6$D1.4_6,T6$D1.3_6)
table(T9$D1.4_9)
table(T9$D1.4_9,T9$D1.3_9)
table(T12$D1.4_12)
table(T12$D1.4_12,T12$D1.3_12)
table(complete_cases$D1.4_0)
table(complete_cases$D1.4_0,complete_cases$D1.3_0)


######################################################
#### FIGURE 2: COSTS AND RESOURCES USED ##############
######################################################

