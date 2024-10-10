#################### Power Calculation & Regression Analysis ##################
###############################################################################
# Ethan Phillips (Univ of Oxford) - 01/08/2024


# Install packages
#install.packages('pwr')
#install.packages('tidyverse')

# Load packages
library(tidyverse)
library(pwr)
library(lme4)
library(optimx)


############# Calculate power for complete cases & condition sub groups ################

h <- 2 * asin(sqrt(0.683)) - 2*asin(sqrt(0.570)) # proportions taken from previous I-MUR asthma trial (Manfrin, 2017)
n_bofe <- sum(df_complete$D1.4_0 == 'ig (intervention group)') # complete samples in BOFE group
n_uc <- sum(df_complete$D1.4_0 == 'cg (control group)') # complete samples in usual care group

complete_pwr <- pwr.2p2n.test(h = h,
              n1 = n_bofe, 
              n2 = n_uc,
              sig.level = 0.05,
              alternative = "greater")$power

# power = 0.92, therefore sufficiently powered for analysis of all complete patients

n_asthma_bofe <- sum(df_asthma$D1.4_0 == 'ig (intervention group)')
n_asthma_uc <- sum(df_asthma$D1.4_0 == 'cg (control group)')
n_COPD_bofe <- sum(df_asthma$D1.4_0 == 'ig (intervention group)')
n_COPD_uc <- sum(df_COPD$D1.4_0 == 'cg (control group)')

asthma_pwr <- pwr.2p2n.test(h = h,
                           n1 = n_asthma_bofe,
                           n2 = n_asthma_uc,
                           sig.level = 0.05,
                           alternative = "greater")$power

copd_pwr <- pwr.2p2n.test(h = h,
                         n1 = n_COPD_bofe,
                         n2 = n_COPD_uc,
                         sig.level = 0.05,
                         alternative = "greater")$power

# asthma power = 0.705, copd power = 0.677, therefore not sufficiently powered to do sub-group analysis


#################### Calculate power for cost data ###########################
econ_data_no_missing <- economic_data[rowSums(is.na(select(economic_data, -c(D1.2, D1.3_0, D1.4_0)))) != ncol(select(economic_data, -c(D1.2, D1.3_0, D1.4_0))), ]

cost_complete_pts <- Reduce(intersect, list(unique(df_complete_long$patient), unique(econ_data_no_missing$D1.2)))

cost_complete_df <- df_complete %>% filter(D1.2 %in% cost_complete_pts)

n_cost_complete_pts_bofe <- nrow(cost_complete_df %>% filter(D1.4_0 == 'ig (intervention group)'))
n_cost_complete_pts_uc <- nrow(cost_complete_df %>% filter(D1.4_0 == 'cg (control group)'))

cea_pwr = pwr.t2n.test(n1 = n_cost_complete_pts_bofe, n2 = n_cost_complete_pts_uc, power = 0.800, alternative = 'greater')

# smallest effect size we are powered for (at 0.8) is 20%, unless we do imputation on cost data



######################### Construct longitudinalized data ##########################

complete_pts <- unique(df_complete$D1.2)
obs_time <- rep(c(0, 3, 6, 9, 12), times = length(complete_pts))
pts_repeated <- sort(rep(complete_pts, times = 5))

df_complete_long <- data.frame(pts_repeated, obs_time) %>% rename(patient = pts_repeated, time = obs_time)

time_independent_cols <- df_complete %>% select(c(D1.2, D1.3_0, D1.4_0, D2.2_0, D2.3_0, controlled_0, controlled_3, controlled_6, controlled_9, controlled_12))

df_complete_long <- merge(df_complete_long, time_independent_cols, by.x = c('patient'), by.y = c('D1.2'), all.x = TRUE, no.dups = FALSE)

# make controlled_0 and controlled_12 into ints from factors
df_complete_long$controlled_0 <- as.numeric(levels(df_complete_long$controlled_0))[df_complete_long$controlled_0]
df_complete_long$controlled_12 <- as.numeric(levels(df_complete_long$controlled_12))[df_complete_long$controlled_12]


df_complete_long <- df_complete_long %>%
  rename(condition = D1.3_0, group = D1.4_0, gender = D2.2_0, age = D2.3_0) %>%
  mutate(controlled_t = case_when(time == 0 ~ controlled_0, 
                                  time == 3 ~ controlled_3,
                                  time == 6 ~ controlled_6,
                                  time == 9 ~ controlled_9,
                                  time == 12 ~ controlled_12),
         patient = as.factor(patient)) %>%
  select(-c(controlled_3, controlled_6, controlled_9, controlled_12))

group_factor_levels = c('cg (control group)', 'ig (intervention group)')
df_complete_long$group = factor(df_complete_long$group, levels = group_factor_levels)

# QALY Data
quality_life_cols <- df_complete %>%
  select(D1.2, EQindex_0, EQindex_3, EQindex_6, EQindex_9, EQindex_12)

df_complete_long <- merge(df_complete_long, quality_life_cols, by.x = c('patient'), by.y = ('D1.2'), all.x = TRUE, no.dups = TRUE)

df_complete_long <- df_complete_long %>%
  mutate(qalys = case_when(time == 0 ~ 0,
                           time == 3 ~ 0.25 * (EQindex_0 + EQindex_3)/2,
                           time == 6 ~ 0.25 * (EQindex_3 + EQindex_6)/2,
                           time == 9 ~ 0.25 * (EQindex_6 + EQindex_9)/2,
                           time == 12 ~ 0.25 * (EQindex_9 + EQindex_12)/2
                           )
         ) %>%
  select(-c(EQindex_0, EQindex_3, EQindex_6, EQindex_9, EQindex_12))

# Resource Data

resource_use_cols <- df_complete %>% 
  select(c(D1.2, 
           D3.10_1_0, D3.10_1_3, D3.10_1_6, D3.10_1_9, D3.10_1_12,
           D3.10_2_0, D3.10_2_3, D3.10_2_6, D3.10_2_9, D3.10_2_12,
           D3.10_3_0, D3.10_3_3, D3.10_3_6, D3.10_3_9, D3.10_3_12,
           D3.10_4_0, D3.10_4_3, D3.10_4_6, D3.10_4_9, D3.10_4_12,
           D3.10_5_0, D3.10_5_3, D3.10_5_6, D3.10_5_9, D3.10_5_12,
           D3.10_6_0, D3.10_6_3, D3.10_6_6, D3.10_6_9, D3.10_6_12,
           D3.10_7_0, D3.10_7_3, D3.10_7_6, D3.10_7_9, D3.10_7_12,
           D3.11_1_0, D3.11_1_3, D3.11_1_6, D3.11_1_9, D3.11_1_12,
           D3.11_2_0, D3.11_2_3, D3.11_2_6, D3.11_2_9, D3.11_2_12
           ))

df_complete_long <- merge(df_complete_long, resource_use_cols, by.x = c('patient'), by.y = c('D1.2'), all.x = TRUE, no.dups = TRUE)

df_complete_long <- df_complete_long %>%
  mutate(GP_visits = case_when(time == 0 ~ D3.10_1_0,
                                 time == 3 ~ D3.10_1_3,
                                 time == 6 ~ D3.10_1_6,
                                 time == 9 ~ D3.10_1_9,
                                 time == 12 ~ D3.10_1_12
                                 ),
         nurse_visits = case_when(time == 0 ~ D3.10_2_0,
                                    time == 3 ~ D3.10_2_3,
                                    time == 6 ~ D3.10_2_6,
                                    time == 9 ~ D3.10_2_9,
                                    time == 12 ~ D3.10_2_12
                                    ), 
         therapist_visits = case_when(time == 0 ~ D3.10_3_0,
                                        time == 3 ~ D3.10_3_3,
                                        time == 6 ~ D3.10_3_6,
                                        time == 9 ~ D3.10_3_9,
                                        time == 12 ~ D3.10_3_12
                                        ),
         AE_visits = case_when(time == 0 ~ D3.10_4_0,
                                 time == 3 ~ D3.10_4_3,
                                 time == 6 ~ D3.10_4_6,
                                 time == 9 ~ D3.10_4_9,
                                 time == 12 ~ D3.10_4_12
                                 ),
         outpatient_visits = case_when(time == 0 ~ D3.10_5_0,
                                         time == 3 ~ D3.10_5_3,
                                         time == 6 ~ D3.10_5_6,
                                         time == 9 ~ D3.10_5_9,
                                         time == 12 ~ D3.10_5_12
                                         ),
         inpatient_visits = case_when(time == 0 ~ D3.10_6_0,
                                        time == 3 ~ D3.10_6_3,
                                        time == 6 ~ D3.10_6_6,
                                        time == 9 ~ D3.10_6_9,
                                        time == 12 ~ D3.10_6_12
                                        ),
         inpatient_days = case_when(time == 0 ~ D3.10_7_0,
                                      time == 3 ~ D3.10_7_3,
                                      time == 6 ~ D3.10_7_6,
                                      time == 9 ~ D3.10_7_9,
                                      time == 12 ~ D3.10_7_12
                                      ),
         sw_visits = case_when(time == 0 ~ D3.11_1_0,
                                 time == 3 ~ D3.11_1_3,
                                 time == 6 ~ D3.11_1_6,
                                 time == 9 ~ D3.11_1_9,
                                 time == 12 ~ D3.11_1_12
                                 ),
         carecentre_visits_pw = case_when(time == 0 ~ D3.11_2_0,
                                            time == 3 ~ D3.11_2_3,
                                            time == 6 ~ D3.11_2_6,
                                            time == 9 ~ D3.11_2_9,
                                            time == 12 ~ D3.11_2_12
                                            )
         ) %>%
  select(-c(D3.10_1_0, D3.10_1_3, D3.10_1_6, D3.10_1_9, D3.10_1_12,
            D3.10_2_0, D3.10_2_3, D3.10_2_6, D3.10_2_9, D3.10_2_12,
            D3.10_3_0, D3.10_3_3, D3.10_3_6, D3.10_3_9, D3.10_3_12,
            D3.10_4_0, D3.10_4_3, D3.10_4_6, D3.10_4_9, D3.10_4_12,
            D3.10_5_0, D3.10_5_3, D3.10_5_6, D3.10_5_9, D3.10_5_12,
            D3.10_6_0, D3.10_6_3, D3.10_6_6, D3.10_6_9, D3.10_6_12,
            D3.10_7_0, D3.10_7_3, D3.10_7_6, D3.10_7_9, D3.10_7_12,
            D3.11_1_0, D3.11_1_3, D3.11_1_6, D3.11_1_9, D3.11_1_12,
            D3.11_2_0, D3.11_2_3, D3.11_2_6, D3.11_2_9, D3.11_2_12))

# cost data

cost_cols <- df_complete %>% 
  select(c(D1.2, 
           cost_M6, cost_M12,
           cost_C6, cost_C12,
           cost_F6, cost_F12,
           cost_H6, cost_H12,
           cost_O6, cost_O12)) 

df_complete_long <- merge(df_complete_long, cost_cols, by.x = c('patient'), by.y = c('D1.2'), all.x = TRUE, no.dups = TRUE)

df_complete_long <- df_complete_long %>%
  mutate(interv_cost = case_when(time == 0 ~ case_when(group == 'ig (intervention group)' ~ 40, group == 'cg (control group)' ~ 0),
                                 time == 3 ~ 0, 
                                 time == 6 ~ case_when(group == 'ig (intervention group)' ~ 40, group == 'cg (control group)' ~ 0),
                                 time == 9 ~ 0,
                                 time == 12 ~ 0
                                 )
         ) %>%
  mutate(outpatient_cost = case_when(time == 6 ~ cost_M6,
                                     time == 12 ~ cost_M12
                                     ),
         lab_cost = case_when(time == 6 ~ cost_C6,
                              time == 12 ~ cost_C12
                              ),
         med_cost = case_when(time == 6 ~ cost_F6,
                              time == 12 ~ cost_F12
                              ),
         delivery_cost = case_when(time == 6 ~ cost_H6,
                                   time == 12 ~ cost_H12
                                   ), 
         inpatient_cost = case_when(time == 6 ~ cost_O6,
                                    time == 12 ~ cost_O12
                                    )
         ) %>%
  select(-c(cost_M6, cost_M12,
            cost_C6, cost_C12,
            cost_F6, cost_F12,
            cost_H6, cost_H12,
            cost_O6, cost_O12
            )
         )



####################### Regress for Primary Outcome (Longitudinal) ################################

df_complete_long$time <- as.factor(df_complete_long$time)
df_complete_long$group <- as.factor(df_complete_long$group)
df_complete_long$controlled_0 <- as.factor(df_complete_long$controlled_0)
df_complete_long$controlled_t <- as.factor(df_complete_long$controlled_t)
df_complete_long$gender <- as.factor(df_complete_long$gender)
df_complete_long$age <- as.factor(df_complete_long$age)

# USE GLMM 1 (main effect of both group and time included)
glmm_1 <- glmer(formula = controlled_t ~ controlled_0 + age + gender + group*time + (1 | patient),
                data = df_complete_long,
                family = "binomial", 
                control = glmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')
                ))

summary(glmm_1)




# DONT USE THESE MODELS 
# glmm_2 <- glmer(formula = controlled_t ~ controlled_0 + age + gender + group + group:time + (1|patient),
#                 data = df_complete_long,
#                 family = "binomial", 
#                 control = glmerControl(
#                   optimizer ='optimx', optCtrl=list(method='L-BFGS-B')
#                 ))
# 
# summary(glmm_2)
# 
# glmm_3 <- glmer(formula = controlled_t ~ controlled_0 + age + gender + group:time + (1 | patient),
#                 data = df_complete_long,
#                 family = "binomial",
#                 control = glmerControl(
#                   optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# 
# summary(glmm_3)
# 
# glmm_4 <- glmer(formula = controlled_t ~ controlled_0 + age + gender + group + time + (1 | patient),
#                 data = df_complete_long,
#                 family = "binomial",
#                 control = glmerControl(
#                   optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# 
# summary(glmm_4)


###################### Visualize Odds Ratios ######################
odds_data <- data.frame(
  'time' = c(0, 3, 6, 9, 12),
  'bofe_odds' = c(0.980, 1.898, 1.908, 2.092, 1.833),
  'bofe_lower' = c(0.602, 1.042, 1.047, 1.148, 1.006),
  'bofe_upper' = c(1.597, 3.459, 3.476, 3.811, 3.340)
)

library(extrafont)
loadfonts(device = "win")

odds_plot <- ggplot(data = odds_data, mapping = aes(x = time)) + 
  geom_errorbar(aes(ymin = bofe_lower, ymax = bofe_upper, color = 'lightblue'), width = 0.2) +
  geom_point(aes(y = bofe_odds, color = 'BOFE'), size = 2) +
  geom_line(aes(y = 1, color = 'Reference'), linetype = 'dashed') +
  labs(x = 'Time (months)',
       y = 'Odds Ratio') +
  theme_bw() + 
  theme(legend.position = 'none', text = element_text(family = "serif", size = 12)) +
  scale_x_continuous(limits = c(-0.1, 12.1), breaks = seq(0, 12, 3)) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_color_manual(name = 'Treatment Group', values = c('BOFE' = 'blue', 'Reference (Usual Care)' = 'black')) 

odds_plot




##################### Regress for QALYs and Costs ############################


# Remove patients who are missing cost data and reduce long-form data

df_complete_CEA <- df_complete_long %>%
  filter(patient %in% cost_complete_pts) %>%
  group_by(patient) %>%
  mutate(patient = first(patient),
         group = first(group),
         condition = first(condition),
         age = first(age),
         gender = first(gender),
         QALY = sum(qalys, na.rm = TRUE), 
         interv_cost = sum(interv_cost, na.rm = TRUE),
         outpatient_cost = sum(outpatient_cost, na.rm = TRUE), 
         lab_cost = sum(lab_cost, na.rm = TRUE),
         med_cost = sum(med_cost, na.rm = TRUE),
         delivery_cost = sum(delivery_cost, na.rm = TRUE),
         inpatient_cost = sum(inpatient_cost, na.rm = TRUE)
         ) %>%
  filter(row_number(group) == 1) %>%
  mutate(total_cost = sum(interv_cost + outpatient_cost + lab_cost + med_cost + delivery_cost + inpatient_cost))
  #select(c(patient, condition, gender, group, age, controlled_0, QALY, total_cost))

# Look at data shape for CEA analysis
hist(df_complete_CEA$total_cost)
hist(df_complete_CEA$total_cost[df_complete_CEA$total_cost < 10000])
hist(df_complete_CEA$QALY)
hist(df_complete_CEA$QALY[df_complete_CEA$QALY >0])



# Correct for vals < 0

df_complete_CEA$total_cost <- df_complete_CEA$total_cost + 0.001 # fix 0 values for gamma distribution
df_complete_CEA <- df_complete_CEA %>%
  mutate(QALY = case_when(QALY > 0 ~ QALY,
                          QALY <= 0 ~ 0.0001)) # fix 0 values for gamma distribution

df_complete_CEA <- df_complete_CEA %>%
  mutate(age = as.factor(age), gender = as.factor(gender))

hist(df_complete_CEA$QALY)



# Bootstrap and regress

num_iter <- 5000

cost_results <- data.frame(intercept = rep(NA, num_iter), intervention = rep(NA, num_iter))
cost_restuls_adj <- data.frame(intercept = rep(NA, num_iter), interveention = rep(NA, num_iter))

effect_results <- data.frame(intercept = rep(NA, num_iter), intervention = rep(NA, num_iter))
effect_results_adj <- data.frame(intercept = rep(NA, num_iter), intervention = rep(NA, num_iter))

df_complete_CEA_bofe <- df_complete_CEA %>% filter(group == 'ig (intervention group)')
n_bofe <- nrow(df_complete_CEA_bofe)

df_complete_CEA_uc <- df_complete_CEA %>% filter(group == 'cg (control group)')
n_uc <- nrow(df_complete_CEA_uc)

pb <- txtProgressBar(min = 0, max = num_iter, width = 50, initial = 0)


for(i in 1:num_iter){
  
  set.seed(seed = i * 37)
  sample_ig <- df_complete_CEA_bofe[sample(n_bofe, n_bofe, replace = TRUE), ]
  sample_cg <- df_complete_CEA_uc[sample(n_uc, n_uc, replace = TRUE), ]
  
  df_sample <- rbind(sample_ig, sample_cg)
    
  glm_cost <- glm(formula = total_cost ~ group + age + gender, data = df_sample, family="Gamma"(link='log'))
  glm_qaly <- glm(formula = QALY ~ group + age + gender, data = df_sample, family="gaussian"(link='identity'))
  
  cost_results$intercept[i] <- glm_cost$coefficients[1]
  cost_results$intervention[i] <- glm_cost$coefficients[2]
  
  effect_results$intercept[i] <- glm_qaly$coefficients[1]
  effect_results$intervention[i] <- glm_qaly$coefficients[2]
  
  setTxtProgressBar(pb, i)
  
  rm(sample_ig)
  rm(sample_cg)
  rm(df_sample)
  rm(glm_cost)
  rm(glm_qaly)
}
close(pb)

mean(cost_results$intercept)
sd(cost_results$intercept)

mean(cost_results$intervention)
sd(cost_results$intervention)

mean(effect_results$intercept)
sd(effect_results$intercept)

mean(effect_results$intervention)
sd(effect_results$intervention)

# using inverse gaussian, intervention effect is 0.0546 increase in QALY over the year
# using gamma, intervention effect is 0.023 increase in QALY over the year



boot_results = data.frame(
  intercept_mean = c(mean(cost_results$intercept), mean(effect_results$intercept)),
  intercept_min = c(mean(cost_results$intercept) - 1.96*sd(cost_results$intercept), mean(effect_results$intercept) - 1.96*sd(effect_results$intercept)),
  intercept_max = c(mean(cost_results$intercept) + 1.96*sd(cost_results$intercept), mean(effect_results$intercept) + 1.96*sd(effect_results$intercept)),
  intervention_mean = c(mean(cost_results$intervention), mean(effect_results$intervention)),
  intervention_min = c(mean(cost_results$intervention) - 1.96*sd(cost_results$intervention), mean(effect_results$intervention) - 1.96*sd(effect_results$intervention)),
  intervention_max = c(mean(cost_results$intervention) + 1.96*sd(cost_results$intervention), mean(effect_results$intervention) + 1.96*sd(effect_results$intervention))
)

boot_results <- exp(boot_results)
boot_results <- boot_results

cost_results <- cost_results %>%
  mutate(baseline_cost = exp(intercept), cost_ratio = exp(intervention)) %>%
  mutate(intervention_cost = baseline_cost * cost_ratio) %>%
  mutate(incremental_cost = intervention_cost - baseline_cost)

effect_results <- effect_results %>%
  mutate(baseline_qaly = intercept, incremental_qaly = intervention)

cea_results <- data.frame(
  incremental_cost = cost_results$incremental_cost,
  incremental_qaly = effect_results$incremental_qaly
)

mean(cea_results$incremental_cost)
mean(cea_results$incremental_cost) - 1.96 * sd(cea_results$incremental_cost)
mean(cea_results$incremental_cost) + 1.96 * sd(cea_results$incremental_cost)

mean(cea_results$incremental_qaly)
mean(cea_results$incremental_qaly) - 1.96 * sd(cea_results$incremental_qaly)
mean(cea_results$incremental_qaly) + 1.96 * sd(cea_results$incremental_qaly)

mean(cea_results$incremental_qaly)
sd(cea_results$incremental_qaly)

boot_results$incremental_val <- c(mean(cost_results$incremental_cost), mean(effect_results$incremental_qaly))
boot_results$incremental_min <- c(mean(cost_results$incremental_cost) - 1.96 * sd(cost_results$incremental_cost), mean(effect_results$incremental_qaly) - 1.96 * sd(effect_results$incremental_qaly))
boot_results$incremental_max <- c(mean(cost_results$incremental_cost) + 1.96 * sd(cost_results$incremental_cost), mean(effect_results$incremental_qaly) + 1.96 * sd(effect_results$incremental_qaly))

ICER_scores <- cost_results$incremental_cost / effect_results$incremental_qaly
mean(ICER_scores)
sd(ICER_scores)

###################### Calculate probability of acceptance ######################
THRESH <- 25000 # EUR/QALY (taken from NICE guidelines due to lack of figure for ITALY)

thresh_values <- seq(0, 40000, 10)
acceptance_probs <- rep(0, length(thresh_values))

for(i in 1:length(thresh_values)){
  acceptance_probs[i] <- sum((cea_results$incremental_qaly * thresh_values[i] - cea_results$incremental_cost) > 0) / nrow(cea_results)
}

acceptance_probs_df <- data.frame(thresh_values, acceptance_probs)




###################### PLOT CEA Results ######################

cea_plane <- ggplot(data = cea_results, aes(x = incremental_qaly, y = incremental_cost)) + 
  geom_point(color = 'lightblue', size = 1, alpha = 0.5) + 
  geom_point(aes(x = mean(incremental_qaly), y = mean(incremental_cost)), color = 'blue', size = 3) +
  geom_linerange(aes(x = mean(incremental_qaly), ymin = mean(incremental_cost) - 1.96 * sd(incremental_cost), ymax = mean(incremental_cost) + 1.96 * sd(incremental_cost)), color = 'blue') +
  geom_linerange(aes(y = mean(incremental_cost), xmin = mean(incremental_qaly) - 1.96 * sd(incremental_qaly), xmax = mean(incremental_qaly) + 1.96 * sd(incremental_qaly)), color = 'blue') +
  geom_abline(intercept = 0, slope = THRESH, linetype = 'dashed', color = 'black') + # ICER threshold taken from NICE guidelines (lack data for Italy)
  xlim(-0.1, 0.1) + 
  ylim(-750,750) + 
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(xintercept = 0, color = 'black') +
  theme_bw() + 
  geom_text(aes(x = -0.026, y = -525, label = '€25,000 per QALY'), color = 'black', size = 3, angle = 67.5) + 
  scale_x_continuous(limits = c(-0.1, 0.1), breaks = seq(-0.1, 0.1, 0.05), expand = c(0,0)) +
  scale_y_continuous(limits = c(-750, 750), breaks = seq(-750, 750, 250), expand = c(0,0)) + 
  theme(legend.position = 'none', text = element_text(family = "serif", size = 12)) + 
  labs( x = 'Incremental QALY', y = 'Incremental Cost (€)') 

cea_plane


acceptance_curve <- ggplot(data = acceptance_probs_df, (aes(x = thresh_values, y = acceptance_probs))) +
  geom_line(color = 'blue') +
  geom_point(aes(x = THRESH, y = acceptance_probs[thresh_values - THRESH > -5 & thresh_values - THRESH < 5]), color = 'blue', size = 3) +
  geom_vline(xintercept = THRESH, linetype = 'dashed', color = 'black') +
  scale_x_continuous(limits = c(0, 35000), breaks = seq(0, 35000, 5000), expand = c(0,0)) +
  #scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0,0)) +
  scale_y_continuous(limits = c(0.7, 1), breaks = c(0.7, 0.8, 0.9, 1), expand = c(0,0)) +
  labs( x = 'Willingness-to-Pay (€/QALY)', y = 'Probability of Acceptance') + 
  theme_bw() + 
  theme(legend.position = 'none', text = element_text(family = "serif", size = 12))

acceptance_curve

