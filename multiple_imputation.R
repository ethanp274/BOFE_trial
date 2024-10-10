#################### Multiple Imputation of Missing Data     ##################
###############################################################################
# Ethan Phillips (Univ of Oxford) - 03/09/2024


# Load libraries
library(tidyverse)
library(mice)
library(lme4)
library(optimx)
library(broom.mixed)
library(haven)
library(labelled)


################### Missingness Analysis ############################
nrow(df) # 835

table(df$D1.3_0.x)
df$D1.4_0.x <- as.factor(df$D1.4_0.x)

# regress for explanation of completeness by variables of interest
missing_reg <- glm(formula = complete ~ D1.3_0.x + D1.4_0.x + D2.2_0 + D2.3_0 + controlled_0 + EQindex_0 + D1.4_0.x:EQindex_0, data = df, family = binomial(link = 
                                                                                                                                                              logit))

summary(missing_reg)

################### Function to longitudinalize data after imputation #############################
make_long_data <- function(frame){
  frame = as.data.frame(frame)
  
  pts = unique(frame$patient)
  rep_pts = rep(pts, each = 5)
  time = rep(seq(0, 12, 3), length(pts))
  long_data = data.frame(patient = rep_pts, time = time)
  
  # merge all vars (repeated at each time point)
  long_data <- merge(long_data, frame, by.x = 'patient', by.y = 'patient', all.x = TRUE, no.dups = FALSE)
  
  # fix primary outcome
  long_data <- long_data %>%
    mutate(controlled_t = case_when(
      time == 0 ~ controlled_0,
      time == 3 ~ controlled_3,
      time == 6 ~ controlled_6,
      time == 9 ~ controlled_9,
      time == 12 ~ controlled_12
    )) %>%
    select(-c(controlled_3, controlled_6, controlled_9, controlled_12))
  
  # fix EQ-5D
  long_data <- long_data %>%
    mutate(EQindex_t = case_when(
      time == 0 ~ EQindex_0,
      time == 3 ~ EQindex_3,
      time == 6 ~ EQindex_6,
      time == 9 ~ EQindex_9,
      time == 12 ~ EQindex_12
    )) %>%
    select(-c(EQindex_3, EQindex_6, EQindex_9, EQindex_12))
  
  # fix med adherence
  long_data <- long_data %>%
    mutate(med_adherence = case_when(
      time == 0 ~ med_adherence_0,
      time == 3 ~ med_adherence_3,
      time == 6 ~ med_adherence_6,
      time == 9 ~ med_adherence_9,
      time == 12 ~ med_adherence_12
    )) %>%
    select(-c(med_adherence_0, med_adherence_3, med_adherence_6, med_adherence_9, med_adherence_12))
  
  # fix resource use
  long_data <- long_data %>%
    mutate(gp = case_when(
        time == 0 ~ gp_0,
        time == 3 ~ 0,
        time == 6 ~ gp_6,
        time == 9 ~ 0,
        time == 12 ~ gp_12
      ), nurse = case_when(
        time == 0 ~ nurse_0,
        time == 3 ~ 0,
        time == 6 ~ nurse_6,
        time == 9 ~ 0,
        time == 12 ~ nurse_12
      ), therapist = case_when(
        time == 0 ~ therapist_0,
        time == 3 ~ 0,
        time == 6 ~ therapist_6,
        time == 9 ~ 0,
        time == 12 ~ therapist_12
      ), ae = case_when(
        time == 0 ~ ae_0,
        time == 3 ~ 0,
        time == 6 ~ ae_6,
        time == 9 ~ 0,
        time == 12 ~ ae_12
      ), outpatient = case_when(
        time == 0 ~ outpatient_0,
        time == 3 ~ 0,
        time == 6 ~ outpatient_6,
        time == 9 ~ 0,
        time == 12 ~ outpatient_12
      ), inpatient = case_when(
        time == 0 ~ inpatient_0,
        time == 3 ~ 0,
        time == 6 ~ inpatient_6,
        time == 9 ~ 0,
        time == 12 ~ inpatient_12
      ), inpatient_days = case_when(
        time == 0 ~ inpatient_days_0,
        time == 3 ~ 0,
        time == 6 ~ inpatient_days_6,
        time == 9 ~ 0,
        time == 12 ~ inpatient_days_12
      ), sw = case_when(
        time == 0 ~ sw_0,
        time == 3 ~ 0,
        time == 6 ~ sw_6,
        time == 9 ~ 0,
        time == 12 ~ sw_12
      ), daycare = case_when(
        time == 0 ~ daycare_0,
        time == 3 ~ 0,
        time == 6 ~ daycare_6,
        time == 9 ~ 0,
        time == 12 ~ daycare_12
      )
    ) %>%
    select(-c(gp_0, gp_6, gp_12,
             nurse_0, nurse_6, nurse_12,
             therapist_0, therapist_6, therapist_12,
             ae_0, ae_6, ae_12,
             outpatient_0, outpatient_6, outpatient_12,
             inpatient_0, inpatient_6, inpatient_12,
             inpatient_days_0, inpatient_days_6, inpatient_days_12,
             sw_0, sw_6, sw_12,
             daycare_0, daycare_6, daycare_12
             )
          )
  
  # Fix cost data
  long_data <- long_data %>%
    mutate(cost_M = case_when(
          time == 0 ~ 0,
          time == 3 ~ 0,
          time == 6 ~ cost_M6,
          time == 9 ~ 0,
          time == 12 ~ cost_M12
          ), cost_C = case_when(
          time == 0 ~ 0,
          time == 3 ~ 0,
          time == 6 ~ cost_C6,
          time == 9 ~ 0,
          time == 12 ~ cost_C12
          ), cost_O = case_when(
          time == 0 ~ 0,
          time == 3 ~ 0,
          time == 6 ~ cost_O6,
          time == 9 ~ 0,
          time == 12 ~ cost_O12
          ), cost_H = case_when(
          time == 0 ~ 0,
          time == 3 ~ 0,
          time == 6 ~ cost_H6,
          time == 9 ~ 0,
          time == 12 ~ cost_H12
          ), cost_F = case_when(
          time == 0 ~ 0,
          time == 3 ~ 0,
          time == 6 ~ cost_F6,
          time == 9 ~ 0,
          time == 12 ~ cost_F12
          )
    ) %>%
    select(-c(cost_C6, cost_C12, 
              cost_M6, cost_M12, 
              cost_O6, cost_O12, 
              cost_H6, cost_H12, 
              cost_F6, cost_F12)
           )
  
  long_data[, c('cost_M', 'cost_C', 'cost_H', 'cost_F', 'cost_O')][is.na(long_data[, c('cost_M', 'cost_C', 'cost_H', 'cost_F', 'cost_O')])] <- 0
  
  long_data$time <- as.factor(long_data$time)
  
  return(long_data)
}

################### MICE ############################
# make copy for imputation
df_impute <- data.frame(df) 

# Remove empty cols
df_impute <- df_impute[, colSums(is.na(df_impute)) < nrow(df_impute)] 

# Rename cols and cut down to relevant features
df_impute <- df_impute %>% 
  rename(c(patient = D1.2, condition = D1.3_0.x, group = D1.4_0.x, 
           
           # Static variables (at baseline)
           location = D2.1_0, gender = D2.2_0, age = D2.3_0, 
           ethnicity = D2.4_0, education = D2.5_0, 
           selection = D2.6_0, live_alone = D2.7_0,
           FVC_0 = D2.8_0, FEV1_0 = D2.9_0, height = D3.1_0, 
           weight = D3.2_0, BMI = D3.3_0, BMI_range = D3.4_0, 
           smoking = D3.6_0, diabetes = D3.7_1_0, ihd = D3.7_2_0,
           employed = D3.12_0, num_meds = D5.2_0,
           
           # Resource use (at baseline, 6mo, and 12mo)
           gp_0 = D3.10_1_0, nurse_0 = D3.10_2_0,
           therapist_0 = D3.10_3_0, ae_0 = D3.10_4_0,
           outpatient_0 = D3.10_5_0, inpatient_0 = D3.10_6_0,
           inpatient_days_0 = D3.10_7_0, sw_0 = D3.11_1_0,
           daycare_0 = D3.11_2_0, 
           
           gp_6 = D3.10_1_6, nurse_6 = D3.10_2_6,
           therapist_6 = D3.10_3_6, ae_6 = D3.10_4_6,
           outpatient_6 = D3.10_5_6, inpatient_6 = D3.10_6_6,
           inpatient_days_6 = D3.10_7_6, sw_6 = D3.11_1_6,
           daycare_6 = D3.11_2_6,
           
           gp_12 = D3.10_1_12, nurse_12 = D3.10_2_12,
           therapist_12 = D3.10_3_12, ae_12 = D3.10_4_12,
           outpatient_12 = D3.10_5_12, inpatient_12 = D3.10_6_12,
           inpatient_days_12 = D3.10_7_12, sw_12 = D3.11_1_12,
           daycare_12 = D3.11_2_12,
           
           # Med adherence (at baseline, 3mo, 6mo, 9mo, 12mo)
           med_adherence_0 = D5.9_0, last_missed_dose_0 = D5.10_0,
           med_adherence_3 = D5.9_3, last_missed_dose_3 = D5.10_3,
           med_adherence_6 = D5.9_6, last_missed_dose_6 = D5.10_6,
           med_adherence_9 = D5.9_9, last_missed_dose_9 = D5.10_9,
           med_adherence_12 = D5.9_12, last_missed_dose_12 = D5.10_12)) %>%
  select(c( # Static variables
            patient, condition, group, location, gender, ethnicity, age, 
            education, selection, live_alone, FVC_0, FEV1_0, height,
            weight, BMI, BMI_range, smoking, diabetes, ihd, employed, num_meds,
            
            # Primary outcomes
            controlled_0, controlled_3, controlled_6, controlled_9, controlled_12,
            
            EQindex_0, EQindex_3, EQindex_6, EQindex_9, EQindex_12,
            
            # Resource use
            gp_0, nurse_0, therapist_0, ae_0, outpatient_0, inpatient_0, inpatient_days_0, sw_0, daycare_0,
            gp_6, nurse_6, therapist_6, ae_6, outpatient_6, inpatient_6, inpatient_days_6, sw_6, daycare_6,
            gp_12, nurse_12, therapist_12, ae_12, outpatient_12, inpatient_12, inpatient_days_12, sw_12, daycare_12,
            
            
            # Med adherence
            med_adherence_0, last_missed_dose_0, 
            med_adherence_3, last_missed_dose_3,
            med_adherence_6, last_missed_dose_6, 
            med_adherence_9, last_missed_dose_9,
            med_adherence_12, last_missed_dose_12,
            
            # Cost data
            cost_M6, cost_M12,
            cost_C6, cost_C12,
            cost_H6, cost_H12,
            cost_F6, cost_F12,
            cost_O6, cost_O12
  ))

df_impute$age <- as.factor(df_impute$age)
df_impute$education <- as.factor(df_impute$education)
df_impute$selection <- as.factor(df_impute$selection)
df_impute$live_alone <- as.factor(df_impute$live_alone)
df_impute$gender <- as.factor(df_impute$gender)
df_impute$ethnicity <- as.factor(df_impute$ethnicity)
df_impute$BMI_range <- as.factor(df_impute$BMI_range)
df_impute$smoking <- as.factor(df_impute$smoking)
df_impute$diabetes <- as.factor(df_impute$diabetes)
df_impute$ihd <- as.factor(df_impute$ihd)
df_impute$employed <- as.factor(df_impute$employed)
df_impute$controlled_0 <- as.factor(df_impute$controlled_0)
df_impute$controlled_3 <- as.factor(df_impute$controlled_3)
df_impute$controlled_6 <- as.factor(df_impute$controlled_6)
df_impute$controlled_9 <- as.factor(df_impute$controlled_9)
df_impute$controlled_12 <- as.factor(df_impute$controlled_12)

View(df_impute)

# replace NA with 0 for cost columns only
df_impute[, c('cost_M6', 'cost_M12', 'cost_C6', 'cost_C12', 'cost_H6', 'cost_H12', 'cost_F6', 'cost_F12', 'cost_O6', 'cost_O12')][is.na(df_impute[, c('cost_M6', 'cost_M12', 'cost_C6', 'cost_C12', 'cost_H6', 'cost_H12', 'cost_F6', 'cost_F12', 'cost_O6', 'cost_O12')])] <- 0

# Create prediction matrix
pred_matrix <- rep(rep(0, ncol(df_impute)), ncol(df_impute))
pred_matrix <- matrix(pred_matrix, nrow = ncol(df_impute), ncol = ncol(df_impute))

pred_matrix[, grep('^gender$', colnames(df_impute))] <- 1
pred_matrix[, grep('^age$', colnames(df_impute))] <- 1
pred_matrix[, grep('^controlled_0$', colnames(df_impute))] <- 1
pred_matrix[, grep('^EQindex_0$', colnames(df_impute))] <- 1
pred_matrix[grep('^controlled_6$', colnames(df_impute)), grep('^controlled_3$', colnames(df_impute))] <- 1
pred_matrix[grep('^controlled_9$', colnames(df_impute)), grep('^controlled_6$', colnames(df_impute))] <- 1
pred_matrix[grep('^controlled_12$', colnames(df_impute)), grep('^controlled_9$', colnames(df_impute))] <- 1

diag(pred_matrix) <- 0

View(pred_matrix)

# Split by treatment arm
df_impute_UC <- df_impute %>% filter(group == 'cg (control group)')
df_impute_BOFE <- df_impute %>% filter(group == 'ig (intervention group)')

# Split by disease
df_impute_UC_asthma <- df_impute_UC %>% filter(condition == 1) 
df_impute_UC_COPD <- df_impute_UC %>% filter(condition == 2)

df_impute_BOFE_asthma <- df_impute_BOFE %>% filter(condition == 1)
df_impute_BOFE_COPD <- df_impute_BOFE %>% filter(condition == 2)

# labels dont play well with MICE package
df_impute_UC_asthma <- remove_val_labels(df_impute_UC_asthma)
df_impute_UC_COPD <- remove_val_labels(df_impute_UC_COPD)
df_impute_BOFE_asthma <- remove_val_labels(df_impute_BOFE_asthma)
df_impute_BOFE_COPD <- remove_val_labels(df_impute_BOFE_COPD)

# Impute missing vals for both cohorts using MICE algo with PMM (m = 5, n = 5)
df_impute_UC_asthma <- mice(df_impute_UC_asthma, m = 10, method = 'pmm', predictorMatrix = pred_matrix, seed = 123, print = F)
df_impute_UC_COPD <- mice(df_impute_UC_COPD, m = 10, method = 'pmm', predictorMatrix = pred_matrix, seed = 123, print = F)

df_impute_BOFE_asthma <- mice(df_impute_BOFE_asthma, m = 10, method = 'pmm', predictorMatrix = pred_matrix, seed = 123, print = F)
df_impute_BOFE_COPD <- mice(df_impute_BOFE_COPD, m = 10, method = 'pmm', predictorMatrix = pred_matrix, seed = 123, print = F)

# Combine imputed datasets
mids_UC <- mice::rbind(df_impute_UC_asthma, df_impute_UC_COPD)
mids_BOFE <- mice::rbind(df_impute_BOFE_asthma, df_impute_BOFE_COPD)

mids_total <- mice::rbind(mids_UC, mids_BOFE)

# Transform mids into longitudinal format
mids_data <- mice::complete(mids_total, action = "long", include = TRUE)
mids_data_og <- mids_data %>% filter(.imp == 0) %>% select(-c(.imp, .id))
mids_data_1 <- mids_data %>% filter(.imp == 1) %>% select(-c(.imp, .id))
mids_data_2 <- mids_data %>% filter(.imp == 2) %>% select(-c(.imp, .id))
mids_data_3 <- mids_data %>% filter(.imp == 3) %>% select(-c(.imp, .id))
mids_data_4 <- mids_data %>% filter(.imp == 4) %>% select(-c(.imp, .id))
mids_data_5 <- mids_data %>% filter(.imp == 5) %>% select(-c(.imp, .id))
mids_data_6 <- mids_data %>% filter(.imp == 6) %>% select(-c(.imp, .id))
mids_data_7 <- mids_data %>% filter(.imp == 7) %>% select(-c(.imp, .id))
mids_data_8 <- mids_data %>% filter(.imp == 8) %>% select(-c(.imp, .id))
mids_data_9 <- mids_data %>% filter(.imp == 9) %>% select(-c(.imp, .id))

mids_data_og <- make_long_data(mids_data_og)
mids_data_og$time <- as.factor(mids_data_og$time)
levels(mids_data_og$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_og$time <- as.character(mids_data_og$time)
mids_data_og$group <- relevel(mids_data_og$group, ref = 2)
mids_data_og$age <- as.factor(mids_data_og$age)
mids_data_og['.imp'] <- 0

mids_data_1_long <- make_long_data(mids_data_1)
mids_data_1_long$time <- as.factor(mids_data_1_long$time)
levels(mids_data_1_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_1_long$time <- as.character(mids_data_1_long$time)
mids_data_1_long$group <- relevel(mids_data_1_long$group, ref = 2)
mids_data_1_long$age <- as.factor(mids_data_1_long$age)
mids_data_1_long['.imp'] <- 1

mids_data_2_long <- make_long_data(mids_data_2)
mids_data_2_long$time <- as.factor(mids_data_2_long$time)
levels(mids_data_2_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_2_long$time <- as.character(mids_data_2_long$time)
mids_data_2_long$group <- relevel(mids_data_2_long$group, ref = 2)
mids_data_2_long$age <- as.factor(mids_data_2_long$age)
mids_data_2_long['.imp'] <- 2

mids_data_3_long <- make_long_data(mids_data_3)
mids_data_3_long$time <- as.factor(mids_data_3_long$time)
levels(mids_data_3_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_3_long$time <- as.character(mids_data_3_long$time)
mids_data_3_long$group <- relevel(mids_data_3_long$group, ref = 2)
mids_data_3_long$age <- as.factor(mids_data_3_long$age)
mids_data_3_long['.imp'] <- 3

mids_data_4_long <- make_long_data(mids_data_4)
mids_data_4_long$time <- as.factor(mids_data_4_long$time)
levels(mids_data_4_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_4_long$time <- as.character(mids_data_4_long$time)
mids_data_4_long$group <- relevel(mids_data_4_long$group, ref = 2)
mids_data_4_long$age <- as.factor(mids_data_4_long$age)
mids_data_4_long['.imp'] <- 4

mids_data_5_long <- make_long_data(mids_data_5)
mids_data_5_long$time <- as.factor(mids_data_5_long$time)
levels(mids_data_5_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_5_long$time <- as.character(mids_data_5_long$time)
mids_data_5_long$group <- relevel(mids_data_5_long$group, ref = 2)
mids_data_5_long$age <- as.factor(mids_data_5_long$age)
mids_data_5_long['.imp'] <- 5

mids_data_6_long <- make_long_data(mids_data_6)
mids_data_6_long$time <- as.factor(mids_data_6_long$time)
levels(mids_data_6_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_6_long$time <- as.character(mids_data_6_long$time)
mids_data_6_long$group <- relevel(mids_data_6_long$group, ref = 2)
mids_data_6_long$age <- as.factor(mids_data_6_long$age)
mids_data_6_long['.imp'] <- 6

mids_data_7_long <- make_long_data(mids_data_7)
mids_data_7_long$time <- as.factor(mids_data_7_long$time)
levels(mids_data_7_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_7_long$time <- as.character(mids_data_7_long$time)
mids_data_7_long$group <- relevel(mids_data_7_long$group, ref = 2)
mids_data_7_long$age <- as.factor(mids_data_7_long$age)
mids_data_7_long['.imp'] <- 7

mids_data_8_long <- make_long_data(mids_data_8)
mids_data_8_long$time <- as.factor(mids_data_8_long$time)
levels(mids_data_8_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_8_long$time <- as.character(mids_data_8_long$time)
mids_data_8_long$group <- relevel(mids_data_8_long$group, ref = 2)
mids_data_8_long$age <- as.factor(mids_data_8_long$age)
mids_data_8_long['.imp'] <- 8

mids_data_9_long <- make_long_data(mids_data_9)
mids_data_9_long$time <- as.factor(mids_data_9_long$time)
levels(mids_data_9_long$time) <- c('0mo', '3mo' , '6mo', '9mo', '12mo')
mids_data_9_long$time <- as.character(mids_data_9_long$time)
mids_data_9_long$group <- relevel(mids_data_9_long$group, ref = 2)
mids_data_9_long$age <- as.factor(mids_data_9_long$age)
mids_data_9_long['.imp'] <- 9

mids_data_long <- mice::rbind(mids_data_og, mids_data_1_long, mids_data_2_long, mids_data_3_long, mids_data_4_long, mids_data_5_long, mids_data_6_long, mids_data_7_long, mids_data_8_long, mids_data_9_long)
mids_data_long <- as.mids(mids_data_long)

# perform GLMM regression on total MIDS
mira_glmm <- with(mids_data_long, 
                  glmer(formula = controlled_t ~ controlled_0 + age + gender + group*time + (1|patient),
                        family = "binomial", 
                        control = glmerControl(
                          optimizer ='optimx', optCtrl=list(method='L-BFGS-B')
                          )
                        )
                  )

mira_glmm <- as.mira(mira_glmm)

mipo_glmm <- pool(mira_glmm)

summary(mipo_glmm)



