
######################################################
########## Descriptive statistics and tests ##########
######################################################

######################################################
# TABLE 1: BASELINE CHARACTERISTICS AND TESTS ########
######################################################

# By pathology: D1.3 -> 1= Asthma, 2= COPD
# D1.4_0 -> A = ??, B = ??

# Load data


# Create data frames by health condition
df_asthma <- complete_cases[complete_cases$D1.3_0==1,] #Asthma
df_COPD <- complete_cases[complete_cases$D1.3_0==2,] #COPD

# Type of variables: 

ordinal_vars <- c("D2.3_0")

continuous_vars <- c("D3.1_0","D3.2_0", "D3.3_0","D3.10_1_0", "D3.10_2_0", 
                     "D3.10_3_0", "D3.10_4_0", "D3.10_5_0", "D3.10_6_0", 
                     "D3.10_7_0","D3.11_1_0", "D3.11_2_0", "ACT.SCORE_0", 
                     "CCQ.SCORE_0", "CCQ.symptom_0", "CCQ.functional_0", 
                     "CCQ.mental_0", "EQindex_0", "D5.2_0")

proportion_vars <- c("D2.7_0","D3.5_1_0","D3.5_2_0","D3.5_3_0", "D3.7_1_0",
                     "D3.7_2_0","D3.7_3_0", "D3.8_0", "D3.9_0","D3.12_0")
                     
                     
categoric_vars <- c("D2.1_0", "D2.2_0","D2.4_0", "D2.5_0","D2.6_0","D3.6_0",
                    "D5.5_0","D5.6_0")


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
  print(paste("Number of groups:", unique(df$D1.4_0)))

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
          add_row(variable_name = var,
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
        add_row(variable_name = var,
                intervention_mean = as.character(paste0(round(intervention_stats[2] * 100, 2), "%")),
                intervention_sd = NA,
                intervention_pvalue = prop_test$p.value,
                control_mean = as.character(paste0(round(control_stats[2] * 100, 2), "%")),
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
        
        Table1 <- Table1 %>% 
          add_row(variable_name = var,
                  intervention_mean = as.character(paste0(round(intervention_stats[2] * 100, 2), "%")),
                  intervention_sd = NA,
                  intervention_pvalue = wilcox_test$p.value,
                  control_mean = as.character(paste0(round(control_stats[2] * 100, 2), "%")),
                  control_sd = NA,
                  control_pvalue = wilcox_test$p.value)
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
      
      Table1 <- Table1 %>% 
        add_row(variable_name = var,
                intervention_mean = as.character(paste0(round(intervention_stats[2] * 100, 2), "%")),
                intervention_sd = NA,
                intervention_pvalue = chisq_test$p.value,
                control_mean = as.character(paste0(round(control_stats[2] * 100, 2), "%")),
                control_sd = NA,
                control_pvalue = chisq_test$p.value)
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

# Print the resulting Table1 dataframe for asthma test case
print(Table1_asthma)


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
table(df$D1.4_0, df$D1.3_0)

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

