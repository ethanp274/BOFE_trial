
#############################################
########### BOFE R PLOTS: 15-05 #############
#############################################

# Author: Lydia Prieto
# Date: 10-05-2024 


#Graph 
#Evolution of patients with negative values of EQindex in T0
#EQINDEX IN T0 = -0.36 -0.356 -0.218  -0.21 -0.166 -0.153

severer_patients <- complete_cases %>% 
  filter(EQindex_0==-0.36)   %>% 
  select(D1.2) %>%
  pull(D1.2)

patients <- c("TM8A","EJ1B","MX0B","DX1A","IM6B","HA9B")
patients[patients==FALSE] <- NA
patients <- na.omit(patients)
table(patients)
patients_pathology <- complete_cases[complete_cases$D1.2 %in% patients,]
patients_pathology <- patients_pathology$D1.3_0

patients_subset_t0 <- complete_cases[complete_cases$D1.2 %in% patients,c("D1.2", "CCQ.SCORE_0", "EQindex_0")]
patients_subset_t0$studyperiod <- 0
patients_subset_t0 <- patients_subset_t0 %>%
  rename(
    CCQ.SCORE=CCQ.SCORE_0,
    EQindex= EQindex_0
  )

patients_subset_t3 <- complete_cases[complete_cases$D1.2 %in% patients,c("D1.2", "CCQ.SCORE_3", "EQindex_3")]
patients_subset_t3$studyperiod <- 3
patients_subset_t3 <- patients_subset_t3 %>%
  rename(
    CCQ.SCORE=CCQ.SCORE_3,
    EQindex= EQindex_3
  )

patients_subset_t6 <- complete_cases[complete_cases$D1.2 %in% patients,c("D1.2", "CCQ.SCORE_6", "EQindex_6")]
patients_subset_t6$studyperiod <- 6
patients_subset_t6 <- patients_subset_t6 %>%
  rename(
    CCQ.SCORE=CCQ.SCORE_6,
    EQindex= EQindex_6
  )


patients_subset_t9 <- complete_cases[complete_cases$D1.2 %in% patients,c("D1.2", "CCQ.SCORE_9", "EQindex_9")]
patients_subset_t9$studyperiod <- 9
patients_subset_t9 <- patients_subset_t9 %>%
  rename(
    CCQ.SCORE=CCQ.SCORE_9,
    EQindex= EQindex_9
  )

patients_subset_t12 <- complete_cases[complete_cases$D1.2 %in% patients,c("D1.2", "CCQ.SCORE_12", "EQindex_12")]
patients_subset_t12$studyperiod <- 12
patients_subset_t12 <- patients_subset_t12 %>%
  rename(
    CCQ.SCORE=CCQ.SCORE_12,
    EQindex= EQindex_12
  )


patients_subset <-bind_rows(
  patients_subset_t0,
  patients_subset_t3,
  patients_subset_t6,
  patients_subset_t9,
  patients_subset_t12,
)

ggplot(patients_subset, aes(x=studyperiod, y=CCQ.SCORE, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title=" CCQ Score over time for serverer patients", x="Study periods", y="CCQ score", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=EQindex, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title=" EQindex over time for serverer patients", x="Study periods", y="EQ index", color="Patient id")+
  theme_minimal()


# Severest cases inconsistency:  "DX1A"
complete_cases[complete_cases$D1.2=="DX1A",]
pharmacist <- complete_cases %>% 
  filter(complete_cases$D1.2=="DX1A") %>%
  select(D1.1) %>%
  pull(D1.1)


# "15434" at T6
# patients that correspond to pharmacy code 15434
patients2 <- complete_cases %>%
  filter(complete_cases$D1.1=="15434") %>%
  select(D1.2) %>%
  pull(D1.2)
# "DX1A" "FG5B" "FP4B" "HV4A" "KT1A" "PA6A" "WQ2B"

patients2_subset_t0 <- complete_cases[complete_cases$D1.2 %in% patients2,c("D1.2", "EQindex_0")]
patients2_subset_t0$studyperiod <- 0
patients2_subset_t0 <- patients2_subset_t0 %>%
  rename(
    EQindex= EQindex_0
  )

patients2_subset_t3 <- complete_cases[complete_cases$D1.2 %in% patients2,c("D1.2", "EQindex_3")]
patients2_subset_t3$studyperiod <- 3
patients2_subset_t3 <- patients2_subset_t3 %>%
  rename(
    EQindex= EQindex_3
  )

patients2_subset_t6 <- complete_cases[complete_cases$D1.2 %in% patients2,c("D1.2", "EQindex_6")]
patients2_subset_t6$studyperiod <- 6
patients2_subset_t6 <- patients2_subset_t6 %>%
  rename(
    EQindex= EQindex_6
  )


patients2_subset_t9 <- complete_cases[complete_cases$D1.2 %in% patients2,c("D1.2", "EQindex_9")]
patients2_subset_t9$studyperiod <- 9
patients2_subset_t9 <- patients2_subset_t9 %>%
  rename(
    EQindex= EQindex_9
  )

patients2_subset_t12 <- complete_cases[complete_cases$D1.2 %in% patients2,c("D1.2", "EQindex_12")]
patients2_subset_t12$studyperiod <- 12
patients2_subset_t12 <- patients2_subset_t12 %>%
  rename(
    EQindex= EQindex_12
  )


patients2_subset <-bind_rows(
  patients2_subset_t0,
  patients2_subset_t3,
  patients2_subset_t6,
  patients2_subset_t9,
  patients2_subset_t12,
)

ggplot(patients2_subset, aes(x=studyperiod, y=EQindex, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title=" EQindex Score over time for patients in pharmacy 15434", x="Study periods", y="EQindex", color="Patient id")+
  theme_minimal()

complete_cases$EQindex_3[complete_cases$D1.2=="FG5B"]
complete_cases[complete_cases$D1.2=="FG5B",]



rm(ACTcontrolled_table0, ACTcontrolled_table3)
rm(ACTcontrolled_table6, ACTcontrolled_table9, ACTcontrolled_table12)
rm(CCQcontrolled_table0, CCQcontrolled_table3,CCQcontrolled_table6,
   CCQcontrolled_table9, CCQcontrolled_table12)

# Histogram plots of ACT/CCQ Scores

hist(merged_data$ACT.SCORE_0, xlab="ACT Score in T0")
hist(merged_data$ACT.SCORE_3, xlab="ACT Score in T3")
hist(merged_data$ACT.SCORE_6, xlab="ACT Score in T6")
hist(merged_data$ACT.SCORE_9, xlab="ACT Score in T9")
hist(merged_data$ACT.SCORE_12, xlab="ACT Score in T12")

hist(merged_data$CCQ.SCORE_0, xlab="CCQ Score in T0")
hist(merged_data$CCQ.SCORE_3, xlab="CCQ Score in T3")
hist(merged_data$CCQ.SCORE_6, xlab="CCQ Score in T6")
hist(merged_data$CCQ.SCORE_9, xlab="CCQ Score in T9")
hist(merged_data$CCQ.SCORE_12, xlab="CCQ Score in T12")


# Plot of controlled vs not controlled

# ACT (Asthma)

ACTcontrolled_table0 <- table(merged_data$ACT_controlled_0, merged_data$D1.4_0)
barplot(ACTcontrolled_table0, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled Asthma patients by group in T0")

ACTcontrolled_table3 <- table(merged_data$ACT_controlled_3, merged_data$D1.4_3[merged_data$ACT_controlled_3>=0])
barplot(ACTcontrolled_table3, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled Asthma patients by group in T3")

ACTcontrolled_table6 <- table(merged_data$ACT_controlled_6, merged_data$D1.4_6[merged_data$ACT_controlled_6>=0])
barplot(ACTcontrolled_table6, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled Asthma patients by group in T6")


ACTcontrolled_table9 <- table(merged_data$ACT_controlled_9, merged_data$D1.4_9[merged_data$ACT_controlled_9>=0])
barplot(ACTcontrolled_table9, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled Asthma patients by group in T9")

ACTcontrolled_table12 <- table(merged_data$ACT_controlled_12, merged_data$D1.4_12[merged_data$ACT_controlled_12>=0])
barplot(ACTcontrolled_table12, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled Asthma patients by group in T12")


# CCQ (COPD)

CCQcontrolled_table0 <- table(merged_data$CCQ_controlled_0, merged_data$D1.4_0)
barplot(CCQcontrolled_table0, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled COPD patients by group in T0")

CCQcontrolled_table3 <- table(merged_data$CCQ_controlled_3, merged_data$D1.4_3[merged_data$CCQ_controlled_3>=0])
barplot(CCQcontrolled_table3, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled COPD patients by group in T3")

CCQcontrolled_table6 <- table(merged_data$CCQ_controlled_6, merged_data$D1.4_6[merged_data$CCQ_controlled_6>=0])
barplot(CCQcontrolled_table6, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled COPD patients by group in T6")

CCQcontrolled_table9 <- table(merged_data$CCQ_controlled_9, merged_data$D1.4_9[merged_data$CCQ_controlled_9>=0])
barplot(CCQcontrolled_table9, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled COPD patients by group in T9")

CCQcontrolled_table12 <- table(merged_data$CCQ_controlled_12, merged_data$D1.4_12[merged_data$CCQ_controlled_12>=0])
barplot(CCQcontrolled_table12, beside=TRUE, legend=TRUE, 
        names.arg=c("IG", "CG"),
        xlab="Intervention and Control Group", ylab="Frequency", 
        main="Controlled COPD patients by group in T12")




#########################################################################################################################
### Missing economic data for patients in complete cases:

missing_pat_costs <- c("YZ1B","FA1B","BG9A","AM5A","FR4B","DG5A","JV7A","MY0A","HV4A","SS5A") 

patients_subset_t0 <- complete_cases[complete_cases$D1.2 %in% missing_pat_costs,
                                     c("D1.2", "D3.10_1_0","D3.10_2_0","D3.10_3_0","D3.10_4_0","D3.10_5_0",
                                       "D3.10_6_0", "D3.10_7_0")]
patients_subset_t0$studyperiod <- 0
patients_subset_t0 <- patients_subset_t0 %>%
  rename(
    D3.10_1=D3.10_1_0,
    D3.10_2=D3.10_2_0,
    D3.10_3=D3.10_3_0,
    D3.10_4=D3.10_4_0,
    D3.10_5=D3.10_5_0,
    D3.10_6=D3.10_6_0,
    D3.10_7=D3.10_7_0
  )

patients_subset_t6 <- complete_cases[complete_cases$D1.2 %in% missing_pat_costs,
                                     c("D1.2", "D3.10_1_6", "D3.10_2_6", "D3.10_3_6","D3.10_4_6","D3.10_5_6",
                                       "D3.10_6_6","D3.10_7_6")]
patients_subset_t6$studyperiod <- 6
patients_subset_t6 <- patients_subset_t6 %>%
  rename(
    D3.10_1=D3.10_1_6,
    D3.10_2=D3.10_2_6,
    D3.10_3=D3.10_3_6,
    D3.10_4=D3.10_4_6,
    D3.10_5=D3.10_5_6,
    D3.10_6=D3.10_6_6,
    D3.10_7=D3.10_7_6
  )



patients_subset_t12 <- complete_cases[complete_cases$D1.2 %in% missing_pat_costs,
                                      c("D1.2", "D3.10_1_12", "D3.10_2_12","D3.10_3_12","D3.10_4_12","D3.10_5_12",
                                        "D3.10_6_12","D3.10_7_12")]
patients_subset_t12$studyperiod <- 12
patients_subset_t12 <- patients_subset_t12 %>%
  rename(
    D3.10_1=D3.10_1_12,
    D3.10_2=D3.10_2_12,
    D3.10_3=D3.10_3_12,
    D3.10_4=D3.10_4_12,
    D3.10_5=D3.10_5_12,
    D3.10_6=D3.10_6_12,
    D3.10_7=D3.10_7_12
  )


patients_subset <-bind_rows(
  patients_subset_t0,
  patients_subset_t6,
  patients_subset_t12,
)

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_1, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of GP visits in the last 6 months (D3.10_1)", x="Study periods", y="GP visits", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_2, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of Nurse visits in the last 6 months (D3.10_2)", x="Study periods", y="Nurse visits", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_3, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of Therapists visits in the last 6 months (D3.10_3)", x="Study periods", y="Therapists visits", color="Patient id")+
  theme_minimal

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_4, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of Hospital A&E visits in the last 6 months (D3.10_4)", x="Study periods", y=" A&E  visits", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_5, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of Hospital outpatient visits in the last 6 months (D3.10_5)", x="Study periods", y=" Outpatient  visits", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_6, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of hospitalisations in the last 6 months (D3.10_6)", x="Study periods", y="Hospitalisations", color="Patient id")+
  theme_minimal()

ggplot(patients_subset, aes(x=studyperiod, y=D3.10_7, color=as.factor(D1.2), group = D1.2)) +
  geom_line() +
  geom_point() +
  labs(title="Number of days in the hospital in the last 6 months (D3.10_7)", x="Study periods", y="Days in hospital", color="Patient id")+
  theme_minimal()
