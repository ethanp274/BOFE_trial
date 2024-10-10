
#############################################
########### BOFE: R MAIN SCRIPT #############
#############################################

# Author: Lydia Prieto
# Date: 01-05-2024

#############################################


# Install packages

install.packages('haven')
install.packages('labelled')
install.packages('tidyverse')
install.packages('sjPlot')
install.packages('summarytools')
install.packages('survey')
install.packages('openxlsx')
install.packages('comparedf')

# Load the packages

library(survey)
library(summarytools)
library(haven)
library(labelled)
library(tidyverse)
library(sjPlot)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)

# Set the working directory

setwd('C:/Users/lydiap/OneDrive - Nexus365/BOFE Project/Master data sets 18.02.2024/Master data sets 18.02.2024')



#############################################
########### QUESTIONNAIRE DATASET ###########
#############################################


# Open and save data sets

T0  <- read_sav('T0.sav')
T3  <- read_sav('T3.sav')
T6  <- read_sav('T6.sav')
T9  <- read_sav('T9.sav')
T12 <- read_sav('T12.sav')


# Examine the values

# view_df(T0)
# view_df(T3)
# view_df(T6)
# view_df(T9)
# view_df(T12)


# Descriptive statistics: 

# Rewrite variable from chr("A","B") to factor

T0$D1.4 <- as_factor(T0$D1.4)
T3$D1.4 <- as_factor(T3$D1.4)
T6$D1.4 <- as_factor(T6$D1.4)
T9$D1.4 <- as_factor(T9$D1.4)
T12$D1.4 <- as_factor(T12$D1.4)



# Turn empty variables of sections not asked in the questionnaire at "t" time period to NA (specified in "C:\Users\lydiap\OneDrive - Nexus365\BOFE Project\Master data sets 18.02.2024\Variables\Empty values.xlsx")

# Empty variables in T0

  T0$D5.18_1 <- na_if(T0$D5.18_1, 0)
  T0$D5.18_2 <- na_if(T0$D5.18_2, 0)
  T0$D5.18_3 <- na_if(T0$D5.18_3, 0)
  T0$D5.18_4 <- na_if(T0$D5.18_4, 0)
  T0$D5.18_5 <- na_if(T0$D5.18_5, 0)

  
# Empty variables in T3
  
  T3$D2.1 <- na_if(T3$D2.1, 0)
  T3$D2.2 <- na_if(T3$D2.2, 0)
  T3$D2.3 <- na_if(T3$D2.3, 0)
  T3$D2.4 <- na_if(T3$D2.4, 0)
  T3$D2.5 <- na_if(T3$D2.5, 0)
  T3$D2.6 <- na_if(T3$D2.6, 0)
  T3$D2.7 <- na_if(T3$D2.7, 0)
  T3$D2.8 <- na_if(T3$D2.8, 0)
  T3$D2.9 <- na_if(T3$D2.9, 0)
  T3$D3.1 <- na_if(T3$D3.1, 0)
  T3$D3.2 <- na_if(T3$D3.2, 0)
  T3$D3.3 <- na_if(T3$D3.3, 0)
  T3$D3.4 <- na_if(T3$D3.4, 0)
  T3$D3.5_1 <- na_if(T3$D3.5_1, 0)
  T3$D3.5_2 <- na_if(T3$D3.5_2, 0) 
  T3$D3.5_3 <- na_if(T3$D3.5_3, 0)
  T3$D3.6 <- na_if(T3$D3.6, 0)
  T3$D3.7_1 <- na_if(T3$D3.7_1, 0)
  T3$D3.7_2 <- na_if(T3$D3.7_2, 0)
  T3$D3.7_3 <- na_if(T3$D3.7_3, 0)
  T3$D3.8 <- na_if(T3$D3.8, 0)
  T3$D3.9 <- na_if(T3$D3.9, 0)
  T3$D3.10_1 <- na_if(T3$D3.10_1, 0)
  T3$D3.10_2 <- na_if(T3$D3.10_2, 0)
  T3$D3.10_3 <- na_if(T3$D3.10_3, 0)
  T3$D3.10_4 <- na_if(T3$D3.10_4, 0)
  T3$D3.10_5 <- na_if(T3$D3.10_5, 0)
  T3$D3.10_6 <- na_if(T3$D3.10_6, 0)
  T3$D3.10_7 <- na_if(T3$D3.10_7, 0)
  T3$D3.11_1 <- na_if(T3$D3.11_1, 0)
  T3$D3.11_2 <- na_if(T3$D3.11_2, 0)
  T3$D3.12 <- na_if(T3$D3.12, 0)
  T3$D3.13 <- na_if(T3$D3.13, 0)
  T3$D5.3_1 <- na_if(T3$D5.3_1, 0)
  T3$D5.3_2 <- na_if(T3$D5.3_2, 0)
  T3$D5.3_3 <- na_if(T3$D5.3_3, 0)
  T3$D5.3_4 <- na_if(T3$D5.3_4, 0)
  T3$D5.3_5 <- na_if(T3$D5.3_5, 0)
  T3$D5.3_6 <- na_if(T3$D5.3_6, 0)
  T3$D5.3_7 <- na_if(T3$D5.3_7, 0)
  T3$D5.3_8 <- na_if(T3$D5.3_8, 0)
  T3$D5.3_9 <- na_if(T3$D5.3_9, 0)
  T3$D5.3_10 <- na_if(T3$D5.3_10, 0)
  T3$D5.4_1 <- na_if(T3$D5.4_1, 0)
  T3$D5.4_2 <- na_if(T3$D5.4_2, 0)
  T3$D5.4_3 <- na_if(T3$D5.4_3, 0)
  T3$D5.4_4 <- na_if(T3$D5.4_4, 0)
  T3$D5.5 <- na_if(T3$D5.5, 0)
  T3$D5.6 <- na_if(T3$D5.6, 0)
  T3$D5.7 <- na_if(T3$D5.7, 0)
  T3$D5.8 <- na_if(T3$D5.8, 0)
  T3$D5.11_1 <- na_if(T3$D5.11_1, 0)
  T3$D5.11_2 <- na_if(T3$D5.11_2, 0)
  T3$D5.11_3 <- na_if(T3$D5.11_3, 0)
  T3$D5.11_4 <- na_if(T3$D5.11_4, 0)
  T3$D5.11_5 <- na_if(T3$D5.11_5, 0)
  T3$D5.11_6 <- na_if(T3$D5.11_6, 0)
  T3$D5.11_7 <- na_if(T3$D5.11_7, 0)
  T3$D5.11_8 <- na_if(T3$D5.11_8, 0)
  T3$D5.11_9 <- na_if(T3$D5.11_9, 0)
  T3$D5.11_10 <- na_if(T3$D5.11_10, 0)
  T3$D5.11_11 <- na_if(T3$D5.11_11, 0)
  T3$D5.11_12 <- na_if(T3$D5.11_12, 0)
  T3$D5.12_1 <- na_if(T3$D5.12_1, 0)
  T3$D5.12_2 <- na_if(T3$D5.12_2, 0)
  T3$D5.12_3 <- na_if(T3$D5.12_3, 0)
  T3$D5.12_4 <- na_if(T3$D5.12_4, 0)
  T3$D5.12_5 <- na_if(T3$D5.12_5, 0)
  T3$D5.12_6 <- na_if(T3$D5.12_6, 0)
  T3$D5.14_1 <- na_if(T3$D5.14_1, 0)
  T3$D5.14_2 <- na_if(T3$D5.14_2, 0)
  T3$D5.14_3 <- na_if(T3$D5.14_3, 0)
  T3$D5.14_4 <- na_if(T3$D5.14_4, 0)
  T3$D5.14_5 <- na_if(T3$D5.14_5, 0)
  T3$D5.14_6 <- na_if(T3$D5.14_6, 0)
  T3$D5.14_7 <- na_if(T3$D5.14_7, 0)
  T3$D5.15_1 <- na_if(T3$D5.15_1, 0)
  T3$D5.15_2 <- na_if(T3$D5.15_2, 0)
  T3$D5.15_3 <- na_if(T3$D5.15_3, 0)
  T3$D5.15_4 <- na_if(T3$D5.15_4, 0)
  T3$D5.15_5 <- na_if(T3$D5.15_5, 0)
  T3$D5.15_6 <- na_if(T3$D5.15_6, 0)
  T3$D5.15_7 <- na_if(T3$D5.15_7, 0)
  T3$D5.15_8 <- na_if(T3$D5.15_8, 0)
  T3$D5.15_9 <- na_if(T3$D5.15_9, 0)
  T3$D5.16_1 <- na_if(T3$D5.16_1, 0)
  T3$D5.16_2 <- na_if(T3$D5.16_2, 0)
  T3$D5.16_3 <- na_if(T3$D5.16_3, 0)
  T3$D5.16_4 <- na_if(T3$D5.16_4, 0)
  T3$D5.16_5 <- na_if(T3$D5.16_5, 0)
  T3$D5.16_6 <- na_if(T3$D5.16_6, 0)
  T3$D5.16_7 <- na_if(T3$D5.16_7, 0)
  T3$D5.16_8 <- na_if(T3$D5.16_8, 0)
  T3$D5.17_1 <- na_if(T3$D5.17_1, 0)
  T3$D5.17_2 <- na_if(T3$D5.17_2, 0)
  T3$D5.17_3 <- na_if(T3$D5.17_3, 0)
  T3$D5.17_4 <- na_if(T3$D5.17_4, 0)
  T3$D5.17_5 <- na_if(T3$D5.17_5, 0)
  T3$D5.17_6 <- na_if(T3$D5.17_6, 0)
  T3$D5.17_7 <- na_if(T3$D5.17_7, 0)
  T3$D5.17_8 <- na_if(T3$D5.17_8, 0)
  T3$D5.18_1 <- na_if(T3$D5.18_1, 0)
  T3$D5.18_2 <- na_if(T3$D5.18_2, 0)
  T3$D5.18_3 <- na_if(T3$D5.18_3, 0)
  T3$D5.18_4 <- na_if(T3$D5.18_4, 0)
  T3$D5.18_5 <- na_if(T3$D5.18_5, 0)

# Empty variables in T6

  T6$D2.1 <- na_if(T6$D2.1, 0)
  T6$D2.2 <- na_if(T6$D2.2, 0)
  T6$D2.3 <- na_if(T6$D2.3, 0)
  T6$D2.4 <- na_if(T6$D2.4, 0)
  T6$D2.5 <- na_if(T6$D2.5, 0)
  T6$D2.6 <- na_if(T6$D2.6, 0)
  T6$D2.7 <- na_if(T6$D2.7, 0)
  T6$D2.8 <- na_if(T6$D2.8, 0)
  T6$D2.9 <- na_if(T6$D2.9, 0)
  T6$D3.1 <- na_if(T6$D3.1, 0)
  T6$D3.2 <- na_if(T6$D3.2, 0)
  T6$D3.3 <- na_if(T6$D3.3, 0)
  T6$D3.4 <- na_if(T6$D3.4, 0)
  T6$D3.5_1 <- na_if(T6$D3.5_1, 0)
  T6$D3.5_2 <- na_if(T6$D3.5_2, 0) 
  T6$D3.5_3 <- na_if(T6$D3.5_3, 0)
  T6$D3.6 <- na_if(T6$D3.6, 0)
  T6$D3.7_1 <- na_if(T6$D3.7_1, 0)
  T6$D3.7_2 <- na_if(T6$D3.7_2, 0)
  T6$D3.7_3 <- na_if(T6$D3.7_3, 0)
  T6$D3.8 <- na_if(T6$D3.8, 0)
  T6$D3.9 <- na_if(T6$D3.9, 0)
  T6$D5.5 <- na_if(T6$D5.5, 0)
  T6$D5.6 <- na_if(T6$D5.6, 0)
  T6$D5.7 <- na_if(T6$D5.7, 0)
  T6$D5.8 <- na_if(T6$D5.8, 0)
  T6$D5.18_1 <- na_if(T6$D5.18_1, 0)
  T6$D5.18_2 <- na_if(T6$D5.18_2, 0)
  T6$D5.18_3 <- na_if(T6$D5.18_3, 0)
  T6$D5.18_4 <- na_if(T6$D5.18_4, 0)
  T6$D5.18_5 <- na_if(T6$D5.18_5, 0)


# Empty variables in T9

  T9$D2.1 <- na_if(T9$D2.1, 0)
  T9$D2.2 <- na_if(T9$D2.2, 0)
  T9$D2.3 <- na_if(T9$D2.3, 0)
  T9$D2.4 <- na_if(T9$D2.4, 0)
  T9$D2.5 <- na_if(T9$D2.5, 0)
  T9$D2.6 <- na_if(T9$D2.6, 0)
  T9$D2.7 <- na_if(T9$D2.7, 0)
  T9$D2.8 <- na_if(T9$D2.8, 0)
  T9$D2.9 <- na_if(T9$D2.9, 0)
  T9$D3.1 <- na_if(T9$D3.1, 0)
  T9$D3.2 <- na_if(T9$D3.2, 0)
  T9$D3.3 <- na_if(T9$D3.3, 0)
  T9$D3.4 <- na_if(T9$D3.4, 0)
  T9$D3.5_1 <- na_if(T9$D3.5_1, 0)
  T9$D3.5_2 <- na_if(T9$D3.5_2, 0) 
  T9$D3.5_3 <- na_if(T9$D3.5_3, 0)
  T9$D3.6 <- na_if(T9$D3.6, 0)
  T9$D3.7_1 <- na_if(T9$D3.7_1, 0)
  T9$D3.7_2 <- na_if(T9$D3.7_2, 0)
  T9$D3.7_3 <- na_if(T9$D3.7_3, 0)
  T9$D3.8 <- na_if(T9$D3.8, 0)
  T9$D3.9 <- na_if(T9$D3.9, 0)
  T9$D3.10_1 <- na_if(T9$D3.10_1, 0)
  T9$D3.10_2 <- na_if(T9$D3.10_2, 0)
  T9$D3.10_3 <- na_if(T9$D3.10_3, 0)
  T9$D3.10_4 <- na_if(T9$D3.10_4, 0)
  T9$D3.10_5 <- na_if(T9$D3.10_5, 0)
  T9$D3.10_6 <- na_if(T9$D3.10_6, 0)
  T9$D3.10_7 <- na_if(T9$D3.10_7, 0)
  T9$D3.11_1 <- na_if(T9$D3.11_1, 0)
  T9$D3.11_2 <- na_if(T9$D3.11_2, 0)
  T9$D3.12 <- na_if(T9$D3.12, 0)
  T9$D3.13 <- na_if(T9$D3.13, 0)
  T9$D5.3_1 <- na_if(T9$D5.3_1, 0)
  T9$D5.3_2 <- na_if(T9$D5.3_2, 0)
  T9$D5.3_3 <- na_if(T9$D5.3_3, 0)
  T9$D5.3_4 <- na_if(T9$D5.3_4, 0)
  T9$D5.3_5 <- na_if(T9$D5.3_5, 0)
  T9$D5.3_6 <- na_if(T9$D5.3_6, 0)
  T9$D5.3_7 <- na_if(T9$D5.3_7, 0)
  T9$D5.3_8 <- na_if(T9$D5.3_8, 0)
  T9$D5.3_9 <- na_if(T9$D5.3_9, 0)
  T9$D5.3_10 <- na_if(T9$D5.3_10, 0)
  T9$D5.4_1 <- na_if(T9$D5.4_1, 0)
  T9$D5.4_2 <- na_if(T9$D5.4_2, 0)
  T9$D5.4_3 <- na_if(T9$D5.4_3, 0)
  T9$D5.4_4 <- na_if(T9$D5.4_4, 0)
  T9$D5.5 <- na_if(T9$D5.5, 0)
  T9$D5.6 <- na_if(T9$D5.6, 0)
  T9$D5.7 <- na_if(T9$D5.7, 0)
  T9$D5.8 <- na_if(T9$D5.8, 0)
  T9$D5.11_1 <- na_if(T9$D5.11_1, 0)
  T9$D5.11_2 <- na_if(T9$D5.11_2, 0)
  T9$D5.11_3 <- na_if(T9$D5.11_3, 0)
  T9$D5.11_4 <- na_if(T9$D5.11_4, 0)
  T9$D5.11_5 <- na_if(T9$D5.11_5, 0)
  T9$D5.11_6 <- na_if(T9$D5.11_6, 0)
  T9$D5.11_7 <- na_if(T9$D5.11_7, 0)
  T9$D5.11_8 <- na_if(T9$D5.11_8, 0)
  T9$D5.11_9 <- na_if(T9$D5.11_9, 0)
  T9$D5.11_10 <- na_if(T9$D5.11_10, 0)
  T9$D5.11_11 <- na_if(T9$D5.11_11, 0)
  T9$D5.11_12 <- na_if(T9$D5.11_12, 0)
  T9$D5.12_1 <- na_if(T9$D5.12_1, 0)
  T9$D5.12_2 <- na_if(T9$D5.12_2, 0)
  T9$D5.12_3 <- na_if(T9$D5.12_3, 0)
  T9$D5.12_4 <- na_if(T9$D5.12_4, 0)
  T9$D5.12_5 <- na_if(T9$D5.12_5, 0)
  T9$D5.12_6 <- na_if(T9$D5.12_6, 0)
  T9$D5.14_1 <- na_if(T9$D5.14_1, 0)
  T9$D5.14_2 <- na_if(T9$D5.14_2, 0)
  T9$D5.14_3 <- na_if(T9$D5.14_3, 0)
  T9$D5.14_4 <- na_if(T9$D5.14_4, 0)
  T9$D5.14_5 <- na_if(T9$D5.14_5, 0)
  T9$D5.14_6 <- na_if(T9$D5.14_6, 0)
  T9$D5.14_7 <- na_if(T9$D5.14_7, 0)
  T9$D5.15_1 <- na_if(T9$D5.15_1, 0)
  T9$D5.15_2 <- na_if(T9$D5.15_2, 0)
  T9$D5.15_3 <- na_if(T9$D5.15_3, 0)
  T9$D5.15_4 <- na_if(T9$D5.15_4, 0)
  T9$D5.15_5 <- na_if(T9$D5.15_5, 0)
  T9$D5.15_6 <- na_if(T9$D5.15_6, 0)
  T9$D5.15_7 <- na_if(T9$D5.15_7, 0)
  T9$D5.15_8 <- na_if(T9$D5.15_8, 0)
  T9$D5.15_9 <- na_if(T9$D5.15_9, 0)
  T9$D5.16_1 <- na_if(T9$D5.16_1, 0)
  T9$D5.16_2 <- na_if(T9$D5.16_2, 0)
  T9$D5.16_3 <- na_if(T9$D5.16_3, 0)
  T9$D5.16_4 <- na_if(T9$D5.16_4, 0)
  T9$D5.16_5 <- na_if(T9$D5.16_5, 0)
  T9$D5.16_6 <- na_if(T9$D5.16_6, 0)
  T9$D5.16_7 <- na_if(T9$D5.16_7, 0)
  T9$D5.16_8 <- na_if(T9$D5.16_8, 0)
  T9$D5.17_1 <- na_if(T9$D5.17_1, 0)
  T9$D5.17_2 <- na_if(T9$D5.17_2, 0)
  T9$D5.17_3 <- na_if(T9$D5.17_3, 0)
  T9$D5.17_4 <- na_if(T9$D5.17_4, 0)
  T9$D5.17_5 <- na_if(T9$D5.17_5, 0)
  T9$D5.17_6 <- na_if(T9$D5.17_6, 0)
  T9$D5.17_7 <- na_if(T9$D5.17_7, 0)
  T9$D5.17_8 <- na_if(T9$D5.17_8, 0)
  T9$D5.18_1 <- na_if(T9$D5.18_1, 0)
  T9$D5.18_2 <- na_if(T9$D5.18_2, 0)
  T9$D5.18_3 <- na_if(T9$D5.18_3, 0)
  T9$D5.18_4 <- na_if(T9$D5.18_4, 0)
  T9$D5.18_5 <- na_if(T9$D5.18_5, 0)

# Empty variables in T12

  T12$D2.1 <- na_if(T12$D2.1, 0)
  T12$D2.2 <- na_if(T12$D2.2, 0)
  T12$D2.3 <- na_if(T12$D2.3, 0)
  T12$D2.4 <- na_if(T12$D2.4, 0)
  T12$D2.5 <- na_if(T12$D2.5, 0)
  T12$D2.6 <- na_if(T12$D2.6, 0)
  T12$D2.7 <- na_if(T12$D2.7, 0)
  T12$D2.8 <- na_if(T12$D2.8, 0)
  T12$D2.9 <- na_if(T12$D2.9, 0)
  T12$D3.1 <- na_if(T12$D3.1, 0)
  T12$D3.2 <- na_if(T12$D3.2, 0)
  T12$D3.3 <- na_if(T12$D3.3, 0)
  T12$D3.4 <- na_if(T12$D3.4, 0)
  T12$D3.5_1 <- na_if(T12$D3.5_1, 0)
  T12$D3.5_2 <- na_if(T12$D3.5_2, 0) 
  T12$D3.5_3 <- na_if(T12$D3.5_3, 0)
  T12$D3.6 <- na_if(T12$D3.6, 0)
  T12$D3.7_1 <- na_if(T12$D3.7_1, 0)
  T12$D3.7_2 <- na_if(T12$D3.7_2, 0)
  T12$D3.7_3 <- na_if(T12$D3.7_3, 0)
  T12$D3.8 <- na_if(T12$D3.8, 0)
  T12$D3.9 <- na_if(T12$D3.9, 0)
  T12$D5.3_1 <- na_if(T12$D5.3_1, 0)
  T12$D5.3_2 <- na_if(T12$D5.3_2, 0)
  T12$D5.3_3 <- na_if(T12$D5.3_3, 0)
  T12$D5.3_4 <- na_if(T12$D5.3_4, 0)
  T12$D5.3_5 <- na_if(T12$D5.3_5, 0)
  T12$D5.3_6 <- na_if(T12$D5.3_6, 0)
  T12$D5.3_7 <- na_if(T12$D5.3_7, 0)
  T12$D5.3_8 <- na_if(T12$D5.3_8, 0)
  T12$D5.3_9 <- na_if(T12$D5.3_9, 0)
  T12$D5.3_10 <- na_if(T12$D5.3_10, 0)
  T12$D5.5 <- na_if(T12$D5.5, 0)
  T12$D5.6 <- na_if(T12$D5.6, 0)
  T12$D5.7 <- na_if(T12$D5.7, 0)
  T12$D5.8 <- na_if(T12$D5.8, 0)
  T12$D5.11_1 <- na_if(T12$D5.11_1, 0)
  T12$D5.11_2 <- na_if(T12$D5.11_2, 0)
  T12$D5.11_3 <- na_if(T12$D5.11_3, 0)
  T12$D5.11_4 <- na_if(T12$D5.11_4, 0)
  T12$D5.11_5 <- na_if(T12$D5.11_5, 0)
  T12$D5.11_6 <- na_if(T12$D5.11_6, 0)
  T12$D5.11_7 <- na_if(T12$D5.11_7, 0)
  T12$D5.11_8 <- na_if(T12$D5.11_8, 0)
  T12$D5.11_9 <- na_if(T12$D5.11_9, 0)
  T12$D5.11_10 <- na_if(T12$D5.11_10, 0)
  T12$D5.11_11 <- na_if(T12$D5.11_11, 0)
  T12$D5.11_12 <- na_if(T12$D5.11_12, 0)
  T12$D5.15_1 <- na_if(T12$D5.15_1, 0)
  T12$D5.15_2 <- na_if(T12$D5.15_2, 0)
  T12$D5.15_3 <- na_if(T12$D5.15_3, 0)
  T12$D5.15_4 <- na_if(T12$D5.15_4, 0)
  T12$D5.15_5 <- na_if(T12$D5.15_5, 0)
  T12$D5.15_6 <- na_if(T12$D5.15_6, 0)
  T12$D5.15_7 <- na_if(T12$D5.15_7, 0)
  T12$D5.15_8 <- na_if(T12$D5.15_8, 0)
  T12$D5.15_9 <- na_if(T12$D5.15_9, 0)
  T12$D5.16_1 <- na_if(T12$D5.16_1, 0)
  T12$D5.16_2 <- na_if(T12$D5.16_2, 0)
  T12$D5.16_3 <- na_if(T12$D5.16_3, 0)
  T12$D5.16_4 <- na_if(T12$D5.16_4, 0)
  T12$D5.16_5 <- na_if(T12$D5.16_5, 0)
  T12$D5.16_6 <- na_if(T12$D5.16_6, 0)
  T12$D5.16_7 <- na_if(T12$D5.16_7, 0)
  T12$D5.16_8 <- na_if(T12$D5.16_8, 0)
  T12$D5.17_1 <- na_if(T12$D5.17_1, 0)
  T12$D5.17_2 <- na_if(T12$D5.17_2, 0)
  T12$D5.17_3 <- na_if(T12$D5.17_3, 0)
  T12$D5.17_4 <- na_if(T12$D5.17_4, 0)
  T12$D5.17_5 <- na_if(T12$D5.17_5, 0)
  T12$D5.17_6 <- na_if(T12$D5.17_6, 0)
  T12$D5.17_7 <- na_if(T12$D5.17_7, 0)
  T12$D5.17_8 <- na_if(T12$D5.17_8, 0)



# Summary of descriptive statistics
  
  # dfSummary(T0)
  # dfSummary(T3)
  # dfSummary(T6)
  # dfSummary(T9)
  # dfSummary(T12)
  

# Merge data sets: 
# Rename variable names for each time period using "t" as a suffix
  
datasets<-list(T0, T3, T6, T9, T12)

rename_vars <- function(data, suffix){
  vars_to_rename <- setdiff(names(data), c("D1.1", "D1.2"))
  new_names <- paste(vars_to_rename, sep="_", suffix)
  names(data)[names(data) %in% vars_to_rename] <- new_names
  return(data)
}

T0 <- rename_vars(T0,0)
T3 <- rename_vars(T3,3)
T6 <- rename_vars(T6,6)
T9 <- rename_vars(T9,9)
T12 <- rename_vars(T12,12)


####Initial merges questionnaire: 

# Merge 1) complete cases: 757 observations

complete_cases <- merge(T0, T3, by = c("D1.1", "D1.2"))
complete_cases <- merge(complete_cases, T6, by = c("D1.1", "D1.2"))
complete_cases <- merge(complete_cases, T9, by = c("D1.1", "D1.2"))
complete_cases <- merge(complete_cases, T12, by = c("D1.1", "D1.2"))

# Merge 2) all cases: 

all_cases <- merge(T0, T3, by = c("D1.1", "D1.2"), all=TRUE)
all_cases <- merge(all_cases,  T6, by = c("D1.1", "D1.2"), all=TRUE)
all_cases <- merge(all_cases,  T9, by = c("D1.1", "D1.2"), all=TRUE)
all_cases <- merge(all_cases, T12, by = c("D1.1", "D1.2"), all=TRUE)


# Summary of the data

complete_cases_no_lab <- remove_labels(complete_cases)
str(complete_cases_no_lab)
descr(complete_cases_no_lab)


# Data inconsistencies and drop outs:

# Missing patient in T0: Difference in T0$D1.2 and all_cases$D1.2
miss_patient <- anti_join(all_cases, T0) %>% select(D1.2) %>% pull(D1.2) 
miss_patient #"PR2B" only in the study in T12
miss_patient <- all_cases[all_cases$D1.2=="PR2B",]

# "PR2B"
table(all_cases$D1.3_0 [all_cases$D1.2=="PR2B"])
table(all_cases$D1.3_3 [all_cases$D1.2=="PR2B"])
table(all_cases$D1.3_6 [all_cases$D1.2=="PR2B"])
table(all_cases$D1.3_9 [all_cases$D1.2=="PR2B"])
table(all_cases$D1.3_12[all_cases$D1.2=="PR2B"]) #Patient PR2B only in T12

# Drop out table: Patient missingness in all cases

missing_patients <- all_cases %>% 
  filter(is.na(D1.3_0)==TRUE|is.na(D1.3_3)==TRUE|is.na(D1.3_6)==TRUE|is.na(D1.3_9)==TRUE|is.na(D1.3_12)==TRUE) %>%
  select(D1.2) %>%
  pull(D1.2)
missing_patients <-as.list(missing_patients)



#Table with drop out patients by time period and total number of missing periods

drop_outs <- all_cases[all_cases$D1.2 %in% missing_patients, c("D1.2","D1.3_0","D1.3_3","D1.3_6","D1.3_9","D1.3_12")]
  drop_outs <- drop_outs %>%
    rowwise() %>%
    mutate(missed_periods = sum(is.na(c_across(D1.3_0:D1.3_12)))) %>%
    ungroup()
  
  # Total incomplete cases = 79

  # Total cases - incomplete cases = complete cases (836-79=757)
  
#Patient inconsistencies

table(complete_cases$D1.3_0, complete_cases$D1.4_0)
table(complete_cases$D1.3_3, complete_cases$D1.4_0)
table(complete_cases$D1.3_6, complete_cases$D1.4_0) #1 COPD patient becomes asthma patient
table(complete_cases$D1.3_9, complete_cases$D1.4_0)
table(complete_cases$D1.3_12,complete_cases$D1.4_0)

patient_inconsistency <- complete_cases %>% 
                          filter(D1.3_0==2, D1.3_3==2,D1.3_9==2,D1.3_12==2,D1.3_6==1) %>%
                          select(D1.2) %>%
                          pull(D1.2)

print(patient_inconsistency) # "OH5A"
complete_cases$D1.3_6[complete_cases$D1.2=="OH5A"] <- 2
complete_cases[complete_cases$D1.2=="OH5A", c("D1.3_0","D1.3_3","D1.3_6","D1.3_9","D1.3_12")]

# Score values of patient "OH5A"

# Check if values are imputed to the right test
complete_cases$ACT.SCORE_0[complete_cases$D1.2=="OH5A"] #0
complete_cases$ACT.SCORE_3[complete_cases$D1.2=="OH5A"] #0
complete_cases$ACT.SCORE_6[complete_cases$D1.2=="OH5A"] #0
complete_cases$ACT.SCORE_9[complete_cases$D1.2=="OH5A"] #0
complete_cases$ACT.SCORE_12[complete_cases$D1.2=="OH5A"] #0

complete_cases$CCQ.SCORE_0[complete_cases$D1.2=="OH5A"] #16
complete_cases$CCQ.SCORE_3[complete_cases$D1.2=="OH5A"] #14
complete_cases$CCQ.SCORE_6[complete_cases$D1.2=="OH5A"] #18
complete_cases$CCQ.SCORE_9[complete_cases$D1.2=="OH5A"] #8 
complete_cases$CCQ.SCORE_12[complete_cases$D1.2=="OH5A"] #17


# Cross-overs in complete_cases: 756 (1 cross-over)
cross_overs <- sum(complete_cases$D1.4_0==complete_cases$D1.4_3 & complete_cases$D1.4_0==complete_cases$D1.4_6 & complete_cases$D1.4_0==complete_cases$D1.4_9 & complete_cases$D1.4_0==complete_cases$D1.4_12)

#Inconsistencies in ig/cg: cross-overs

table(complete_cases$D1.4_0,complete_cases$D1.3_0)
table(complete_cases$D1.4_3,complete_cases$D1.3_3)
table(complete_cases$D1.4_6,complete_cases$D1.3_6) #1 COPD patient in IG becomes CG
table(complete_cases$D1.4_9,complete_cases$D1.3_9)
table(complete_cases$D1.4_12,complete_cases$D1.3_12)

patient_inconsistency2 <- complete_cases %>% 
  filter(D1.4_0=="ig (intervention group)", D1.4_3=="ig (intervention group)",D1.4_9=="ig (intervention group)",D1.4_12=="ig (intervention group)",D1.4_6=="cg (control group)") %>%
  select(D1.2) %>%
  pull(D1.2)

print(patient_inconsistency2) # "OH5A"

# Re-code variables: 

# Height D3.1_0 (only in T0) : meters 
min(complete_cases$D3.1_0)
max(complete_cases$D3.1_0)
table(complete_cases$D3.1_0)

# Re-code values in cm to m: 152  156  158  159  160  162  163  165  168  170  174  175  180
complete_cases$D3.1_0 <- ifelse(complete_cases$D3.1_0>100, complete_cases$D3.1_0/100,complete_cases$D3.1_0)

#Recalculate BMI D3.3_0
complete_cases$D3.3_0 <- complete_cases$D3.2_0/(complete_cases$D3.1_0)^2



# Primary outcome

# Turn NA ACT/CCQ SCORES not corresponding to the patient

# ACT SCORES 
complete_cases[complete_cases$D1.3_0 == 2, c("ACT.1_0", "ACT.2_0", "ACT.3_0", "ACT.4_0", "ACT.5_0")] <- NA
complete_cases[complete_cases$D1.3_3 == 2, c("ACT.1_3", "ACT.2_3", "ACT.3_3", "ACT.4_3", "ACT.5_3")] <- NA
complete_cases[complete_cases$D1.3_6 == 2, c("ACT.1_6", "ACT.2_6", "ACT.3_6", "ACT.4_6", "ACT.5_6")] <- NA
complete_cases[complete_cases$D1.3_9 == 2, c("ACT.1_9", "ACT.2_9", "ACT.3_9", "ACT.4_9", "ACT.5_9")] <- NA
complete_cases[complete_cases$D1.3_9 == 2, c("ACT.1_12", "ACT.2_12", "ACT.3_12", "ACT.4_12", "ACT.5_12")] <- NA

complete_cases$ACT.SCORE_0 [complete_cases$D1.3_0==2] <- NA
complete_cases$ACT.SCORE_3 [complete_cases$D1.3_0==2] <- NA
complete_cases$ACT.SCORE_6 [complete_cases$D1.3_0==2] <- NA
complete_cases$ACT.SCORE_9 [complete_cases$D1.3_0==2] <- NA
complete_cases$ACT.SCORE_12[complete_cases$D1.3_0==2] <- NA

# CCQ SCORES

complete_cases[complete_cases$D1.3_0 == 1, c("CCQ.1_0", "CCQ.2_0", "CCQ.3_0", "CCQ.4_0", "CCQ.5_0", "CCQ.6_0", "CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0")] <- NA
complete_cases[complete_cases$D1.3_3 == 1, c("CCQ.1_3", "CCQ.2_3", "CCQ.3_3", "CCQ.4_3", "CCQ.5_3", "CCQ.6_3", "CCQ.7_3", "CCQ.8_3", "CCQ.9_3", "CCQ.10_3")] <- NA
complete_cases[complete_cases$D1.3_6 == 1, c("CCQ.1_6", "CCQ.2_6", "CCQ.3_6", "CCQ.4_6", "CCQ.5_6", "CCQ.6_6", "CCQ.7_6", "CCQ.8_6", "CCQ.9_6", "CCQ.10_6")] <- NA
complete_cases[complete_cases$D1.3_9 == 1, c("CCQ.1_9", "CCQ.2_9", "CCQ.3_9", "CCQ.4_9", "CCQ.5_9", "CCQ.6_9", "CCQ.7_9", "CCQ.8_9", "CCQ.9_9", "CCQ.10_9")] <- NA
complete_cases[complete_cases$D1.3_12 == 1, c("CCQ.1_12", "CCQ.2_12", "CCQ.3_12", "CCQ.4_12", "CCQ.5_12", "CCQ.6_12", "CCQ.7_12", "CCQ.8_12", "CCQ.9_12", "CCQ.10_12")] <- NA

complete_cases$CCQ.SCORE_0 [complete_cases$D1.3_0==1] <- NA
complete_cases$CCQ.SCORE_3 [complete_cases$D1.3_0==1] <- NA
complete_cases$CCQ.SCORE_6 [complete_cases$D1.3_0==1] <- NA
complete_cases$CCQ.SCORE_9 [complete_cases$D1.3_0==1] <- NA
complete_cases$CCQ.SCORE_12[complete_cases$D1.3_0==1] <- NA


# For all cases (TOTAL SCORES): 

all_cases$ACT.SCORE_0 [all_cases$D1.3_0==2] <- NA
all_cases$ACT.SCORE_3 [all_cases$D1.3_0==2] <- NA
all_cases$ACT.SCORE_6 [all_cases$D1.3_0==2] <- NA
all_cases$ACT.SCORE_9 [all_cases$D1.3_0==2] <- NA
all_cases$ACT.SCORE_12[all_cases$D1.3_0==2] <- NA

all_cases$CCQ.SCORE_0 [all_cases$D1.3_0==1] <- NA
all_cases$CCQ.SCORE_3 [all_cases$D1.3_0==1] <- NA
all_cases$CCQ.SCORE_6 [all_cases$D1.3_0==1] <- NA
all_cases$CCQ.SCORE_9 [all_cases$D1.3_0==1] <- NA
all_cases$CCQ.SCORE_12[all_cases$D1.3_0==1] <- NA


# Re-code CCQ score: sum of items/10

complete_cases$CCQ.SCORE_0  <- complete_cases$CCQ.SCORE_0/10
complete_cases$CCQ.SCORE_3  <- complete_cases$CCQ.SCORE_3/10
complete_cases$CCQ.SCORE_6  <- complete_cases$CCQ.SCORE_6/10
complete_cases$CCQ.SCORE_9  <- complete_cases$CCQ.SCORE_9/10
complete_cases$CCQ.SCORE_12 <- complete_cases$CCQ.SCORE_12/10

# Symptom = (item 1 + 2 + 5 + 6)/4;
# Functional state = (item 7 + 8 + 9 + 10)/4; 
# Mental state = (item 3 + 4)/2.27

#CCQ scores for symptom, functional state and mental state  (only for T0)

complete_cases$CCQ.1_0 <-as.numeric(complete_cases$CCQ.1_0)
complete_cases$CCQ.2_0 <-as.numeric(complete_cases$CCQ.2_0)
complete_cases$CCQ.3_0 <-as.numeric(complete_cases$CCQ.3_0)
complete_cases$CCQ.4_0 <-as.numeric(complete_cases$CCQ.4_0)
complete_cases$CCQ.5_0 <-as.numeric(complete_cases$CCQ.5_0)
complete_cases$CCQ.6_0 <-as.numeric(complete_cases$CCQ.6_0)
complete_cases$CCQ.7_0 <-as.numeric(complete_cases$CCQ.7_0)
complete_cases$CCQ.8_0 <-as.numeric(complete_cases$CCQ.8_0)
complete_cases$CCQ.9_0 <-as.numeric(complete_cases$CCQ.9_0)
complete_cases$CCQ.10_0 <-as.numeric(complete_cases$CCQ.10_0)


#CCQ SCORE FOR SYMPTOMS (only for T0)

complete_cases$CCQ.symptom_0 <- NA
complete_cases$CCQ.symptom_0[complete_cases$D1.3_0==2] <- rowSums(complete_cases[complete_cases$D1.3_0==2, c("CCQ.1_0","CCQ.2_0", "CCQ.5_0", "CCQ.6_0")]/4, na.rm=TRUE)
complete_cases$CCQ.symptom_0[complete_cases$D1.3_0==1]
complete_cases$CCQ.symptom_0[complete_cases$D1.3_0==2]

#ACTIVITIES  (only for T0)
 
complete_cases$CCQ.functional_0 <- NA
complete_cases$CCQ.functional_0[complete_cases$D1.3_0==2] <- rowSums(complete_cases[complete_cases$D1.3_0==2, c("CCQ.7_0","CCQ.8_0", "CCQ.9_0", "CCQ.10_0")]/4, na.rm=TRUE)
complete_cases$CCQ.functional_0[complete_cases$D1.3_0==1]
complete_cases$CCQ.functional_0[complete_cases$D1.3_0==2]


# MENTAL (only for T0)
complete_cases$CCQ.mental_0 <- NA
complete_cases$CCQ.mental_0[complete_cases$D1.3_0==2] <- rowSums(complete_cases[complete_cases$D1.3_0==2, c("CCQ.3_0","CCQ.4_0")]/2.27, na.rm=TRUE)
complete_cases$CCQ.mental_0[complete_cases$D1.3_0==1]
complete_cases$CCQ.mental_0[complete_cases$D1.3_0==2]



# Dichotomize ACT and CCQ using the thresholds in the study protocol
# Controlled: ACT≥20 and CCQ<2; Not controlled: ACT<20 and CCQ≥2 

complete_cases$ACT_controlled_0  <- ifelse(complete_cases$ACT.SCORE_0>=20, 1,0)
complete_cases$ACT_controlled_3  <- ifelse(complete_cases$ACT.SCORE_3>=20, 1,0)
complete_cases$ACT_controlled_6  <- ifelse(complete_cases$ACT.SCORE_6>=20, 1,0)
complete_cases$ACT_controlled_9  <- ifelse(complete_cases$ACT.SCORE_9>=20, 1,0)
complete_cases$ACT_controlled_12 <- ifelse(complete_cases$ACT.SCORE_12>=20, 1,0)

complete_cases$CCQ_controlled_0  <- ifelse(complete_cases$CCQ.SCORE_0<2, 1,0)
complete_cases$CCQ_controlled_3  <- ifelse(complete_cases$CCQ.SCORE_3<2, 1,0)
complete_cases$CCQ_controlled_6  <- ifelse(complete_cases$CCQ.SCORE_6<2, 1,0)
complete_cases$CCQ_controlled_9  <- ifelse(complete_cases$CCQ.SCORE_9<2, 1,0)
complete_cases$CCQ_controlled_12 <- ifelse(complete_cases$CCQ.SCORE_12<2, 1,0)


# Create new variable for controlled for all sample (indistinctively of their condition)

complete_cases$controlled_0  <- NA 
complete_cases$controlled_3  <- NA 
complete_cases$controlled_6  <- NA
complete_cases$controlled_9  <- NA 
complete_cases$controlled_12 <- NA

# Controlled patients in T0

complete_cases$controlled_0[complete_cases$ACT_controlled_0==1] <- 1
complete_cases$controlled_0[complete_cases$ACT_controlled_0==0] <- 0
complete_cases$controlled_0[complete_cases$CCQ_controlled_0==1] <- 1
complete_cases$controlled_0[complete_cases$CCQ_controlled_0==0] <- 0
sum(is.na(complete_cases$controlled_0))
table(complete_cases$controlled_0)

# Controlled patients in T3

complete_cases$controlled_3[complete_cases$ACT_controlled_3==1] <- 1
complete_cases$controlled_3[complete_cases$ACT_controlled_3==0] <- 0
complete_cases$controlled_3[complete_cases$CCQ_controlled_3==1] <- 1
complete_cases$controlled_3[complete_cases$CCQ_controlled_3==0] <- 0
sum(is.na(complete_cases$controlled_3))
table(complete_cases$controlled_3)

# Controlled patients in T6

complete_cases$controlled_6[complete_cases$ACT_controlled_6==1] <- 1
complete_cases$controlled_6[complete_cases$ACT_controlled_6==0] <- 0
complete_cases$controlled_6[complete_cases$CCQ_controlled_6==1] <- 1
complete_cases$controlled_6[complete_cases$CCQ_controlled_6==0] <- 0
sum(is.na(complete_cases$controlled_6))
table(complete_cases$controlled_6)

# Controlled patients in T9

complete_cases$controlled_9[complete_cases$ACT_controlled_9==1] <- 1
complete_cases$controlled_9[complete_cases$ACT_controlled_9==0] <- 0
complete_cases$controlled_9[complete_cases$CCQ_controlled_9==1] <- 1
complete_cases$controlled_9[complete_cases$CCQ_controlled_9==0] <- 0
sum(is.na(complete_cases$controlled_9))
table(complete_cases$controlled_9)


# Controlled patients in T12

complete_cases$controlled_12[complete_cases$ACT_controlled_12==1] <- 1
complete_cases$controlled_12[complete_cases$ACT_controlled_12==0] <- 0
complete_cases$controlled_12[complete_cases$CCQ_controlled_12==1] <- 1
complete_cases$controlled_12[complete_cases$CCQ_controlled_12==0] <- 0
sum(is.na(complete_cases$controlled_12))
table(complete_cases$controlled_12)

# EQ5D5L - UTILITY SCORES : 

#Rename the variables:
#Mobility

names(complete_cases)[names(complete_cases)=="EQ5D5L.1_0"] <- "mobility_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.1_3"] <- "mobility_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.1_6"] <- "mobility_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.1_9"] <- "mobility_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.1_12"] <- "mobility_12"

#Selfcare

names(complete_cases)[names(complete_cases)=="EQ5D5L.2_0"] <- "selfcare_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.2_3"] <- "selfcare_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.2_6"] <- "selfcare_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.2_9"] <- "selfcare_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.2_12"] <- "selfcare_12"

#Activity

names(complete_cases)[names(complete_cases)=="EQ5D5L.3_0"] <- "activity_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.3_3"] <- "activity_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.3_6"] <- "activity_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.3_9"] <- "activity_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.3_12"] <- "activity_12"

#Pain

names(complete_cases)[names(complete_cases)=="EQ5D5L.4_0"] <- "pain_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.4_3"] <- "pain_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.4_6"] <- "pain_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.4_9"] <- "pain_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.4_12"] <- "pain_12"

#Anxiety

names(complete_cases)[names(complete_cases)=="EQ5D5L.5_0"] <- "anxiety_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.5_3"] <- "anxiety_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.5_6"] <- "anxiety_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.5_9"] <- "anxiety_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.5_12"] <- "anxiety_12"


#EQ5D5L.SCORE to EQindex
names(complete_cases)[names(complete_cases)=="EQ5D5L.SCORE_0"] <- "EQindex_0"
names(complete_cases)[names(complete_cases)=="EQ5D5L.SCORE_3"] <- "EQindex_3"
names(complete_cases)[names(complete_cases)=="EQ5D5L.SCORE_6"] <- "EQindex_6"
names(complete_cases)[names(complete_cases)=="EQ5D5L.SCORE_9"] <- "EQindex_9"
names(complete_cases)[names(complete_cases)=="EQ5D5L.SCORE_12"] <- "EQindex_12"


#Generate new variable disut = disutility

#T0

#T0 disutility by dimensions
complete_cases$disut_mo_0 <- NA
complete_cases$disut_sf_0 <- NA
complete_cases$disut_ua_0 <- NA
complete_cases$disut_pd_0 <- NA
complete_cases$disut_ad_0 <- NA

#T0: disut mobility
complete_cases$disut_mo_0[is.na(complete_cases$disut_mo_0) & complete_cases$mobility_0==1]<-0
complete_cases$disut_mo_0[is.na(complete_cases$disut_mo_0) & complete_cases$mobility_0==2]<-0.051
complete_cases$disut_mo_0[is.na(complete_cases$disut_mo_0) & complete_cases$mobility_0==3]<-0.064
complete_cases$disut_mo_0[is.na(complete_cases$disut_mo_0) & complete_cases$mobility_0==4]<-0.244
complete_cases$disut_mo_0[is.na(complete_cases$disut_mo_0) & complete_cases$mobility_0==5]<-0.329

#T0: disut self-care
complete_cases$disut_sf_0[is.na(complete_cases$disut_sf_0) & complete_cases$selfcare_0==1]<-0
complete_cases$disut_sf_0[is.na(complete_cases$disut_sf_0) & complete_cases$selfcare_0==2]<-0.046
complete_cases$disut_sf_0[is.na(complete_cases$disut_sf_0) & complete_cases$selfcare_0==3]<-0.056
complete_cases$disut_sf_0[is.na(complete_cases$disut_sf_0) & complete_cases$selfcare_0==4]<-0.216
complete_cases$disut_sf_0[is.na(complete_cases$disut_sf_0) & complete_cases$selfcare_0==5]<-0.257

#T0: disut usual activities
complete_cases$disut_ua_0[is.na(complete_cases$disut_ua_0) & complete_cases$activity_0==1]<-0
complete_cases$disut_ua_0[is.na(complete_cases$disut_ua_0) & complete_cases$activity_0==2]<-0.050
complete_cases$disut_ua_0[is.na(complete_cases$disut_ua_0) & complete_cases$activity_0==3]<-0.064
complete_cases$disut_ua_0[is.na(complete_cases$disut_ua_0) & complete_cases$activity_0==4]<-0.225
complete_cases$disut_ua_0[is.na(complete_cases$disut_ua_0) & complete_cases$activity_0==5]<-0.255

#T0: disut pain and discomfort
complete_cases$disut_pd_0[is.na(complete_cases$disut_pd_0) & complete_cases$pain_0==1]<-0
complete_cases$disut_pd_0[is.na(complete_cases$disut_pd_0) & complete_cases$pain_0==2]<-0.047
complete_cases$disut_pd_0[is.na(complete_cases$disut_pd_0) & complete_cases$pain_0==3]<-0.088
complete_cases$disut_pd_0[is.na(complete_cases$disut_pd_0) & complete_cases$pain_0==4]<-0.353
complete_cases$disut_pd_0[is.na(complete_cases$disut_pd_0) & complete_cases$pain_0==5]<-0.408

#T0: disut anxiety and depression
complete_cases$disut_ad_0[is.na(complete_cases$disut_ad_0) & complete_cases$anxiety_0==1]<-0
complete_cases$disut_ad_0[is.na(complete_cases$disut_ad_0) & complete_cases$anxiety_0==2]<-0.044
complete_cases$disut_ad_0[is.na(complete_cases$disut_ad_0) & complete_cases$anxiety_0==3]<-0.109
complete_cases$disut_ad_0[is.na(complete_cases$disut_ad_0) & complete_cases$anxiety_0==4]<-0.318
complete_cases$disut_ad_0[is.na(complete_cases$disut_ad_0) & complete_cases$anxiety_0==5]<-0.322

#T3

#T3 disutility by dimensions
complete_cases$disut_mo_3 <- NA
complete_cases$disut_sf_3 <- NA
complete_cases$disut_ua_3 <- NA
complete_cases$disut_pd_3 <- NA
complete_cases$disut_ad_3 <- NA

#T3: disut mobility
complete_cases$disut_mo_3[is.na(complete_cases$disut_mo_3) & complete_cases$mobility_3==1]<-0
complete_cases$disut_mo_3[is.na(complete_cases$disut_mo_3) & complete_cases$mobility_3==2]<-0.051
complete_cases$disut_mo_3[is.na(complete_cases$disut_mo_3) & complete_cases$mobility_3==3]<-0.064
complete_cases$disut_mo_3[is.na(complete_cases$disut_mo_3) & complete_cases$mobility_3==4]<-0.244
complete_cases$disut_mo_3[is.na(complete_cases$disut_mo_3) & complete_cases$mobility_3==5]<-0.329

#T3: disut self-care
complete_cases$disut_sf_3[is.na(complete_cases$disut_sf_3) & complete_cases$selfcare_3==1]<-0
complete_cases$disut_sf_3[is.na(complete_cases$disut_sf_3) & complete_cases$selfcare_3==2]<-0.046
complete_cases$disut_sf_3[is.na(complete_cases$disut_sf_3) & complete_cases$selfcare_3==3]<-0.056
complete_cases$disut_sf_3[is.na(complete_cases$disut_sf_3) & complete_cases$selfcare_3==4]<-0.216
complete_cases$disut_sf_3[is.na(complete_cases$disut_sf_3) & complete_cases$selfcare_3==5]<-0.257

#T3: disut usual activities
complete_cases$disut_ua_3[is.na(complete_cases$disut_ua_3) & complete_cases$activity_3==1]<-0
complete_cases$disut_ua_3[is.na(complete_cases$disut_ua_3) & complete_cases$activity_3==2]<-0.050
complete_cases$disut_ua_3[is.na(complete_cases$disut_ua_3) & complete_cases$activity_3==3]<-0.064
complete_cases$disut_ua_3[is.na(complete_cases$disut_ua_3) & complete_cases$activity_3==4]<-0.225
complete_cases$disut_ua_3[is.na(complete_cases$disut_ua_3) & complete_cases$activity_3==5]<-0.255

#T3: disut pain and discomfort
complete_cases$disut_pd_3[is.na(complete_cases$disut_pd_3) & complete_cases$pain_3==1]<-0
complete_cases$disut_pd_3[is.na(complete_cases$disut_pd_3) & complete_cases$pain_3==2]<-0.047
complete_cases$disut_pd_3[is.na(complete_cases$disut_pd_3) & complete_cases$pain_3==3]<-0.088
complete_cases$disut_pd_3[is.na(complete_cases$disut_pd_3) & complete_cases$pain_3==4]<-0.353
complete_cases$disut_pd_3[is.na(complete_cases$disut_pd_3) & complete_cases$pain_3==5]<-0.408

#T3: disut anxiety and depression
complete_cases$disut_ad_3[is.na(complete_cases$disut_ad_3) & complete_cases$anxiety_3==1]<-0
complete_cases$disut_ad_3[is.na(complete_cases$disut_ad_3) & complete_cases$anxiety_3==2]<-0.044
complete_cases$disut_ad_3[is.na(complete_cases$disut_ad_3) & complete_cases$anxiety_3==3]<-0.109
complete_cases$disut_ad_3[is.na(complete_cases$disut_ad_3) & complete_cases$anxiety_3==4]<-0.318
complete_cases$disut_ad_3[is.na(complete_cases$disut_ad_3) & complete_cases$anxiety_3==5]<-0.322

#T6

#T6 disutility by dimensions
complete_cases$disut_mo_6 <- NA
complete_cases$disut_sf_6 <- NA
complete_cases$disut_ua_6 <- NA
complete_cases$disut_pd_6 <- NA
complete_cases$disut_ad_6 <- NA

#T6: disut mobility
complete_cases$disut_mo_6[is.na(complete_cases$disut_mo_6) & complete_cases$mobility_6==1]<-0
complete_cases$disut_mo_6[is.na(complete_cases$disut_mo_6) & complete_cases$mobility_6==2]<-0.051
complete_cases$disut_mo_6[is.na(complete_cases$disut_mo_6) & complete_cases$mobility_6==3]<-0.064
complete_cases$disut_mo_6[is.na(complete_cases$disut_mo_6) & complete_cases$mobility_6==4]<-0.244
complete_cases$disut_mo_6[is.na(complete_cases$disut_mo_6) & complete_cases$mobility_6==5]<-0.329

#T6: disut self-care
complete_cases$disut_sf_6[is.na(complete_cases$disut_sf_6) & complete_cases$selfcare_6==1]<-0
complete_cases$disut_sf_6[is.na(complete_cases$disut_sf_6) & complete_cases$selfcare_6==2]<-0.046
complete_cases$disut_sf_6[is.na(complete_cases$disut_sf_6) & complete_cases$selfcare_6==3]<-0.056
complete_cases$disut_sf_6[is.na(complete_cases$disut_sf_6) & complete_cases$selfcare_6==4]<-0.216
complete_cases$disut_sf_6[is.na(complete_cases$disut_sf_6) & complete_cases$selfcare_6==5]<-0.257

#T6: disut usual activities
complete_cases$disut_ua_6[is.na(complete_cases$disut_ua_6) & complete_cases$activity_6==1]<-0
complete_cases$disut_ua_6[is.na(complete_cases$disut_ua_6) & complete_cases$activity_6==2]<-0.050
complete_cases$disut_ua_6[is.na(complete_cases$disut_ua_6) & complete_cases$activity_6==3]<-0.064
complete_cases$disut_ua_6[is.na(complete_cases$disut_ua_6) & complete_cases$activity_6==4]<-0.225
complete_cases$disut_ua_6[is.na(complete_cases$disut_ua_6) & complete_cases$activity_6==5]<-0.255

#T6: disut pain and discomfort
complete_cases$disut_pd_6[is.na(complete_cases$disut_pd_6) & complete_cases$pain_6==1]<-0
complete_cases$disut_pd_6[is.na(complete_cases$disut_pd_6) & complete_cases$pain_6==2]<-0.047
complete_cases$disut_pd_6[is.na(complete_cases$disut_pd_6) & complete_cases$pain_6==3]<-0.088
complete_cases$disut_pd_6[is.na(complete_cases$disut_pd_6) & complete_cases$pain_6==4]<-0.353
complete_cases$disut_pd_6[is.na(complete_cases$disut_pd_6) & complete_cases$pain_6==5]<-0.408

#T6: disut anxiety and depression
complete_cases$disut_ad_6[is.na(complete_cases$disut_ad_6) & complete_cases$anxiety_6==1]<-0
complete_cases$disut_ad_6[is.na(complete_cases$disut_ad_6) & complete_cases$anxiety_6==2]<-0.044
complete_cases$disut_ad_6[is.na(complete_cases$disut_ad_6) & complete_cases$anxiety_6==3]<-0.109
complete_cases$disut_ad_6[is.na(complete_cases$disut_ad_6) & complete_cases$anxiety_6==4]<-0.318
complete_cases$disut_ad_6[is.na(complete_cases$disut_ad_6) & complete_cases$anxiety_6==5]<-0.322

#T9

#T9 disutility by dimensions
complete_cases$disut_mo_9 <- NA
complete_cases$disut_sf_9 <- NA
complete_cases$disut_ua_9 <- NA
complete_cases$disut_pd_9 <- NA
complete_cases$disut_ad_9 <- NA

#T9: disut mobility
complete_cases$disut_mo_9[is.na(complete_cases$disut_mo_9) & complete_cases$mobility_9==1]<-0
complete_cases$disut_mo_9[is.na(complete_cases$disut_mo_9) & complete_cases$mobility_9==2]<-0.051
complete_cases$disut_mo_9[is.na(complete_cases$disut_mo_9) & complete_cases$mobility_9==3]<-0.064
complete_cases$disut_mo_9[is.na(complete_cases$disut_mo_9) & complete_cases$mobility_9==4]<-0.244
complete_cases$disut_mo_9[is.na(complete_cases$disut_mo_9) & complete_cases$mobility_9==5]<-0.329

#T9: disut self-care
complete_cases$disut_sf_9[is.na(complete_cases$disut_sf_9) & complete_cases$selfcare_9==1]<-0
complete_cases$disut_sf_9[is.na(complete_cases$disut_sf_9) & complete_cases$selfcare_9==2]<-0.046
complete_cases$disut_sf_9[is.na(complete_cases$disut_sf_9) & complete_cases$selfcare_9==3]<-0.056
complete_cases$disut_sf_9[is.na(complete_cases$disut_sf_9) & complete_cases$selfcare_9==4]<-0.216
complete_cases$disut_sf_9[is.na(complete_cases$disut_sf_9) & complete_cases$selfcare_9==5]<-0.257

#T9: disut usual activities
complete_cases$disut_ua_9[is.na(complete_cases$disut_ua_9) & complete_cases$activity_9==1]<-0
complete_cases$disut_ua_9[is.na(complete_cases$disut_ua_9) & complete_cases$activity_9==2]<-0.050
complete_cases$disut_ua_9[is.na(complete_cases$disut_ua_9) & complete_cases$activity_9==3]<-0.064
complete_cases$disut_ua_9[is.na(complete_cases$disut_ua_9) & complete_cases$activity_9==4]<-0.225
complete_cases$disut_ua_9[is.na(complete_cases$disut_ua_9) & complete_cases$activity_9==5]<-0.255

#T9: disut pain and discomfort
complete_cases$disut_pd_9[is.na(complete_cases$disut_pd_9) & complete_cases$pain_9==1]<-0
complete_cases$disut_pd_9[is.na(complete_cases$disut_pd_9) & complete_cases$pain_9==2]<-0.047
complete_cases$disut_pd_9[is.na(complete_cases$disut_pd_9) & complete_cases$pain_9==3]<-0.088
complete_cases$disut_pd_9[is.na(complete_cases$disut_pd_9) & complete_cases$pain_9==4]<-0.353
complete_cases$disut_pd_9[is.na(complete_cases$disut_pd_9) & complete_cases$pain_9==5]<-0.408

#T9: disut anxiety and depression
complete_cases$disut_ad_9[is.na(complete_cases$disut_ad_9) & complete_cases$anxiety_9==1]<-0
complete_cases$disut_ad_9[is.na(complete_cases$disut_ad_9) & complete_cases$anxiety_9==2]<-0.044
complete_cases$disut_ad_9[is.na(complete_cases$disut_ad_9) & complete_cases$anxiety_9==3]<-0.109
complete_cases$disut_ad_9[is.na(complete_cases$disut_ad_9) & complete_cases$anxiety_9==4]<-0.318
complete_cases$disut_ad_9[is.na(complete_cases$disut_ad_9) & complete_cases$anxiety_9==5]<-0.322

#T12

#T12 disutility by dimensions
complete_cases$disut_mo_12 <- NA
complete_cases$disut_sf_12 <- NA
complete_cases$disut_ua_12 <- NA
complete_cases$disut_pd_12 <- NA
complete_cases$disut_ad_12 <- NA

#T12: disut mobility
complete_cases$disut_mo_12[is.na(complete_cases$disut_mo_12) & complete_cases$mobility_12==1]<-0
complete_cases$disut_mo_12[is.na(complete_cases$disut_mo_12) & complete_cases$mobility_12==2]<-0.051
complete_cases$disut_mo_12[is.na(complete_cases$disut_mo_12) & complete_cases$mobility_12==3]<-0.064
complete_cases$disut_mo_12[is.na(complete_cases$disut_mo_12) & complete_cases$mobility_12==4]<-0.244
complete_cases$disut_mo_12[is.na(complete_cases$disut_mo_12) & complete_cases$mobility_12==5]<-0.329

#T12: disut self-care
complete_cases$disut_sf_12[is.na(complete_cases$disut_sf_12) & complete_cases$selfcare_12==1]<-0
complete_cases$disut_sf_12[is.na(complete_cases$disut_sf_12) & complete_cases$selfcare_12==2]<-0.046
complete_cases$disut_sf_12[is.na(complete_cases$disut_sf_12) & complete_cases$selfcare_12==3]<-0.056
complete_cases$disut_sf_12[is.na(complete_cases$disut_sf_12) & complete_cases$selfcare_12==4]<-0.216
complete_cases$disut_sf_12[is.na(complete_cases$disut_sf_12) & complete_cases$selfcare_12==5]<-0.257

#T12: disut usual activities
complete_cases$disut_ua_12[is.na(complete_cases$disut_ua_12) & complete_cases$activity_12==1]<-0
complete_cases$disut_ua_12[is.na(complete_cases$disut_ua_12) & complete_cases$activity_12==2]<-0.050
complete_cases$disut_ua_12[is.na(complete_cases$disut_ua_12) & complete_cases$activity_12==3]<-0.064
complete_cases$disut_ua_12[is.na(complete_cases$disut_ua_12) & complete_cases$activity_12==4]<-0.225
complete_cases$disut_ua_12[is.na(complete_cases$disut_ua_12) & complete_cases$activity_12==5]<-0.255

#T12: disut pain and discomfort
complete_cases$disut_pd_12[is.na(complete_cases$disut_pd_12) & complete_cases$pain_12==1]<-0
complete_cases$disut_pd_12[is.na(complete_cases$disut_pd_12) & complete_cases$pain_12==2]<-0.047
complete_cases$disut_pd_12[is.na(complete_cases$disut_pd_12) & complete_cases$pain_12==3]<-0.088
complete_cases$disut_pd_12[is.na(complete_cases$disut_pd_12) & complete_cases$pain_12==4]<-0.353
complete_cases$disut_pd_12[is.na(complete_cases$disut_pd_12) & complete_cases$pain_12==5]<-0.408

#T12: disut anxiety and depression
complete_cases$disut_ad_12[is.na(complete_cases$disut_ad_12) & complete_cases$anxiety_12==1]<-0
complete_cases$disut_ad_12[is.na(complete_cases$disut_ad_12) & complete_cases$anxiety_12==2]<-0.044
complete_cases$disut_ad_12[is.na(complete_cases$disut_ad_12) & complete_cases$anxiety_12==3]<-0.109
complete_cases$disut_ad_12[is.na(complete_cases$disut_ad_12) & complete_cases$anxiety_12==4]<-0.318
complete_cases$disut_ad_12[is.na(complete_cases$disut_ad_12) & complete_cases$anxiety_12==5]<-0.322


#Total disutility by study period: SUM OF ALL DISUTILITY COMPONENTS 


complete_cases$total_disut_0 <- rowSums(complete_cases[,c('disut_mo_0','disut_sf_0','disut_ua_0','disut_pd_0','disut_ad_0')])
complete_cases$total_disut_3 <- rowSums(complete_cases[,c('disut_mo_3','disut_sf_3','disut_ua_3','disut_pd_3','disut_ad_3')])
complete_cases$total_disut_6 <- rowSums(complete_cases[,c('disut_mo_6','disut_sf_6','disut_ua_6','disut_pd_6','disut_ad_6')])
complete_cases$total_disut_9 <- rowSums(complete_cases[,c('disut_mo_9','disut_sf_9','disut_ua_9','disut_pd_9','disut_ad_9')])
complete_cases$total_disut_12 <- rowSums(complete_cases[,c('disut_mo_12','disut_sf_12','disut_ua_12','disut_pd_12','disut_ad_12')])


# Calculate EQindex (1- total disutility)

complete_cases$EQindex_0  <- 1-complete_cases$total_disut_0
complete_cases$EQindex_3  <- 1-complete_cases$total_disut_3
complete_cases$EQindex_6  <- 1-complete_cases$total_disut_6
complete_cases$EQindex_9  <- 1-complete_cases$total_disut_9
complete_cases$EQindex_12 <- 1-complete_cases$total_disut_12

complete_cases$EQindex_0  <- round(complete_cases$EQindex_0, 3)
complete_cases$EQindex_3  <- round(complete_cases$EQindex_3, 3)
complete_cases$EQindex_6  <- round(complete_cases$EQindex_6, 3)
complete_cases$EQindex_9  <- round(complete_cases$EQindex_9, 3)
complete_cases$EQindex_12 <- round(complete_cases$EQindex_12, 3)


########################################################################################
#ROWSUMS: 

# Number of patients with EQindex below 0 (SEVERER CASES)

sum(complete_cases$EQindex_0<0) #10
sum(complete_cases$EQindex_3<0) #15
sum(complete_cases$EQindex_6<0) #17
sum(complete_cases$EQindex_9<0) #18
sum(complete_cases$EQindex_12<0) #18

#NAs: Values EQ5D5L =0

table(T0$EQ5D5L.1_0)
table(T3$EQ5D5L.1_3)
table(T6$EQ5D5L.1_6)
table(T9$EQ5D5L.1_9)
table(T12$EQ5D5L.1_12)

#T0: NO 0 values in EQINDEX
healthy_pat <- complete_cases %>% 
  filter(disut_mo_0==0,disut_sf_0==0,disut_ua_0==0,disut_pd_0==0,disut_ad_0==0) %>%
  select(D1.2) %>%
  pull(D1.2)

healthy_pat <- complete_cases[complete_cases$D1.2 %in% healthy_pat, 
                              c('mobility_0', 'selfcare_0', 'activity_0', 'pain_0', 'anxiety_0',
                                'disut_mo_0','disut_sf_0','disut_ua_0','disut_pd_0','disut_ad_0',
                                'total_disut_0','EQindex_0')]

#T3: 
healthy_pat <- complete_cases %>% 
  filter(disut_mo_3==0,disut_sf_3==0,disut_ua_3==0,disut_pd_3==0,disut_ad_3==0) %>%
  select(D1.2) %>%
  pull(D1.2)

healthy_pat <- complete_cases[complete_cases$D1.2 %in% healthy_pat, 
                              c('mobility_3', 'selfcare_3', 'activity_3', 'pain_3', 'anxiety_3',
                                'disut_mo_3','disut_sf_3','disut_ua_3','disut_pd_3','disut_ad_3',
                                'total_disut_3','EQindex_3')]

#EQindex in t3 is NA: 8 patients
missing_patients_eqindex3 <- complete_cases %>% 
  filter(is.na(EQindex_3)==TRUE) %>%
  select(D1.2) %>%
  pull(D1.2)

#"JR4B" "SV3B" "KK1A" "KJ2A" "CJ3B" "PJ8A" "QF8A" "VB4A"
p_JR4B <- complete_cases[complete_cases$D1.2=='JR4B',c('D1.2','D1.3_3','ACT.SCORE_3','mobility_3', 'selfcare_3', 'activity_3', 'pain_3', 'anxiety_3',
                                                     'disut_mo_3','disut_sf_3','disut_ua_3','disut_pd_3','disut_ad_3',
                                                    'total_disut_3','EQindex_3') ]


#EQindex in t6 is NA: 2 patients
missing_patients_eqindex6 <- complete_cases %>% 
  filter(is.na(EQindex_6)==TRUE) %>%
  select(D1.2) %>%
  pull(D1.2)
# "QX7A" "XY5A"

#EQindex in t12 is NA: 1 patient
missing_patients_eqindex12 <- complete_cases %>% 
  filter(is.na(EQindex_12)==TRUE) %>%
  select(D1.2) %>%
  pull(D1.2)
# "CJ3B"




#EQindex = NA and values = 0 
p_miss_t3 <- T3[T3$D1.2 %in% missing_patients_eqindex3,]
p_miss_t6 <- T6[T6$D1.2 %in% missing_patients_eqindex6,]
p_miss_t12 <- T12[T12$D1.2 %in% missing_patients_eqindex12,]



# Patients with no disutility (QE5D5L questinnaire =1 in all dimensions) <-> replace with total_disutility=0 (rowSums create NA if all 0s)
#complete_cases$total_disut_0<-ifelse(is.na(complete_cases$total_disut_0)==TRUE, 0, complete_cases$total_disut_0)
#complete_cases$total_disut_3<-ifelse(is.na(complete_cases$total_disut_3)==TRUE, 0, complete_cases$total_disut_3)
#complete_cases$total_disut_6<-ifelse(is.na(complete_cases$total_disut_6)==TRUE, 0, complete_cases$total_disut_6)
#complete_cases$total_disut_9<-ifelse(is.na(complete_cases$total_disut_9)==TRUE, 0, complete_cases$total_disut_9)
#complete_cases$total_disut_12<-ifelse(is.na(complete_cases$total_disut_12)==TRUE, 0, complete_cases$total_disut_12)



############################################################################
# TESTS AND EXTRA TABLES: 

# Function RowSums treat 0 as NA 

#NA in EQindex in t3
#patients_na_t3<- complete_cases %>% 
#  filter(is.na(EQindex_3)==TRUE) %>%
#  select(D1.2) %>%
#  pull(D1.2)
# "JR4B" "SV3B" "KK1A" "KJ2A" "CJ3B" "PJ8A" "QF8A" "VB4A"

#NA in EQindex in t6
#patients_na_t6<- complete_cases %>% 
#  filter(is.na(EQindex_6)==TRUE) %>%
#  select(D1.2) %>%
#  pull(D1.2)
# "QX7A" "XY5A"

#NA in EQindex in t12
#patients_na_t12<- complete_cases %>% 
#  filter(is.na(EQindex_12)==TRUE) %>%
#  select(D1.2) %>%
#  pull(D1.2)
# "CJ3B"


#patient "CJ3B"
#p_CJ3B<- complete_cases[complete_cases$D1.2=="CJ3B",]
#p_XY5A<- complete_cases[complete_cases$D1.2=="XY5A",]

###############################################################################


#############################################
########### ECONOMIC DATASET ################
#############################################

# Set the working directory

setwd('C:/Users/lydiap/OneDrive - Nexus365/BOFE Project/Master data sets 18.02.2024/Master data sets 18.02.2024')

# Open the cost data sets and save in R

cost_M <- read.xlsx("Economic data.xlsx",sheet="COST M",startRow=1, colNames=TRUE)
cost_C <- read.xlsx("Economic data.xlsx",sheet="COST C",startRow=1, colNames=TRUE)
cost_F1 <-read.xlsx("Economic data.xlsx",sheet="COST F",startRow=1, colNames=TRUE)
cost_F2 <-read.xlsx("Economic data.xlsx",sheet="COST F_2023",startRow=1, colNames=TRUE)
cost_H <-read.xlsx("Economic data.xlsx",sheet="COST H",startRow=1, colNames=TRUE)
cost_O <-read.xlsx("Economic data.xlsx",sheet="COST O",startRow=1, colNames=TRUE)

# Replace NA with 0s (no cost) before merge

cost_M[is.na(cost_M)==TRUE]  <- 0
cost_C[is.na(cost_C)==TRUE]  <- 0
cost_F1[is.na(cost_F1)==TRUE]  <- 0
cost_F2[is.na(cost_F2)==TRUE]  <- 0
cost_H[is.na(cost_H)==TRUE]  <- 0
cost_O[is.na(cost_O)==TRUE]  <- 0

#Negative values

#Cost M: cost for Outpatients & clinic visits
neg_pat <- cost_M %>% 
           filter((rowSums(cost_M<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)
# no patients

#Cost C: cost for the laboratory analyses
neg_pat <- cost_C %>% 
           filter((rowSums(cost_C<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)

#"AC6A" "GA5A" "IT4B" "KU0A" "QY0A" "RZ3A"
p_negC <- cost_C[cost_C$PAZIENTE %in% neg_pat, ]

#Cost F1: cost of medications (2022)
neg_pat <- cost_F1 %>% 
           filter((rowSums(cost_F1<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)
# no patients

#Cost F2: cost of medications (2023)
neg_pat <- cost_F2 %>% 
           filter((rowSums(cost_F2<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)
# no patients

#cost H: NHS direct-to-patient medications
neg_pat <- cost_H %>% 
           filter((rowSums(cost_H<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)
#"IT4B"
p_negH <- cost_H[cost_H$PAZIENTE %in% neg_pat, ]


#cost O: hospital admissions
neg_pat <- cost_O %>% 
           filter((rowSums(cost_O<0)!=0)==TRUE) %>%
           select(PAZIENTE) %>%
           pull(PAZIENTE)
# no patients

# Turn cost<0 to NA 
cost_C$'2022_10_C' <- ifelse(cost_C$'2022_10_C'<0, NA, cost_C$'2022_10_C')
cost_C$'2023_02_C' <- ifelse(cost_C$'2023_02_C'<0, NA, cost_C$'2023_02_C')
cost_C$'2023_05_C' <- ifelse(cost_C$'2023_05_C'<0, NA, cost_C$'2023_05_C')
cost_H$'2023_02_H' <- ifelse(cost_H$'2023_02_H'<0, NA, cost_H$'2023_02_H')

# Merge cost data set: create economic_data data set (825 patients)

economic_data <- merge(cost_M, cost_C, by = c("PAZIENTE"), all=TRUE)
economic_data <- merge(economic_data,  cost_F1, by = c("PAZIENTE"), all=TRUE)
economic_data <- merge(economic_data,  cost_F2, by = c("PAZIENTE"), all=TRUE)
economic_data <- merge(economic_data,  cost_H,  by = c("PAZIENTE"), all=TRUE)
economic_data <- merge(economic_data,  cost_O,  by = c("PAZIENTE"), all=TRUE)

# Rename variable for patient

economic_data <- economic_data %>% 
  rename(D1.2 = PAZIENTE)

# Find patients in "all cases" and "complete cases" with missing economic data: 

# For all cases
miss_patient <- anti_join(all_cases, economic_data) %>% select(D1.2) %>% pull(D1.2) 
miss_patient 
#"YZ1B" "FA1B" "BG9A" "AM5A" "FR4B" "DG5A" "JV7A" "MY0A" "HV4A" "SS5A" "PR2B"

# For complete cases
miss_patient <- anti_join(complete_cases, economic_data) %>% select(D1.2) %>% pull(D1.2) 
miss_patient 
#"YZ1B" "FA1B" "BG9A" "AM5A" "FR4B" "DG5A" "JV7A" "MY0A" "HV4A" "SS5A"

#Patients with incomplete economic data
incomplete_pat <- economic_data %>% 
  filter(rowSums(is.na(economic_data))!=0) %>%
  select(D1.2) %>%
  pull(D1.2)

##Economic data: 

#Identify pathology: asthma vs COPD and group: intervention vs control

complete_cases_subset <- complete_cases[, c("D1.2", "D1.3_0","D1.4_0")]
economic_data <- merge(complete_cases_subset,economic_data, all.x =TRUE)
p_YZ1B <- economic_data[economic_data$D1.2=="YZ1B",]

#test_0 <- cost_C[cost_C$PAZIENTE=="AE0B",] 
#test_0$sum <- rowSums(test_0[, c("2022_06_C","2022_07_C", "2022_08_C", "2022_09_C", "2022_10_C", "2022_11_C")], na.rm=TRUE)


#Sumup accumulated costs by cost category and total costs from T0-T6 & T6-12. 

#Costs C
economic_data$cost_C6 <- NA
economic_data$cost_C6 <- rowSums(economic_data[, c("2022_06_C","2022_07_C", "2022_08_C", "2022_09_C", "2022_10_C", "2022_11_C")])  
economic_data$cost_C12 <- NA
economic_data$cost_C12 <- rowSums(economic_data[, c("2022_12_C","2023_01_C", "2023_02_C", "2023_03_C", "2023_04_C", "2023_05_C")])  

#Costs M
economic_data$cost_M6 <- NA
economic_data$cost_M6 <- rowSums(economic_data[, c("2022_06_M","2022_07_M", "2022_08_M", "2022_09_M", "2022_10_M", "2022_11_M")])  
economic_data$cost_M12 <- NA
economic_data$cost_M12 <- rowSums(economic_data[, c("2022_12_M","2023_01_M", "2023_02_M", "2023_03_M", "2023_04_M", "2023_05_M")])  

#Costs F
economic_data$cost_F6 <- NA
economic_data$cost_F6 <- rowSums(economic_data[, c("2022_06_F","2022_07_F", "2022_08_F", "2022_09_F", "2022_10_F", "2022_11_F")])  
economic_data$cost_F12 <- NA
economic_data$cost_F12 <- rowSums(economic_data[, c("2022_12_F","2023_01_F", "2023_02_F", "2023_03_F", "2023_04_F", "2023_05_F")])  

#Costs H
economic_data$cost_H6 <- NA
economic_data$cost_H6 <- rowSums(economic_data[, c("2022_06_H","2022_07_H", "2022_08_H", "2022_09_H", "2022_10_H", "2022_11_H")])  
economic_data$cost_H12 <- NA
economic_data$cost_H12 <- rowSums(economic_data[, c("2022_12_H","2023_01_H", "2023_02_H", "2023_03_H", "2023_04_H", "2023_05_H")])  

#Costs O
economic_data$cost_O6 <- NA
economic_data$cost_O6 <- rowSums(economic_data[, c("2022_06_O","2022_07_O", "2022_08_O", "2022_09_O", "2022_10_O", "2022_11_O")])  
economic_data$cost_O12 <- NA
economic_data$cost_O12 <- rowSums(economic_data[, c("2022_12_O","2023_01_O", "2023_02_O", "2023_03_O", "2023_04_O", "2023_05_O")])  


#################################################################################

######################################################
############# DATA FRAME COMPLETE ####################
######################################################

#Questionnaire + economic data

#Merge data_frame
df <- NA

#Questionnaire:

df <- merge(T0, T3, by = c("D1.1", "D1.2"), all=TRUE)
df <- merge(df, T6, by = c("D1.1", "D1.2"), all=TRUE)
df <- merge(df, T9, by = c("D1.1", "D1.2"), all=TRUE)
df <- merge(df, T12, by = c("D1.1", "D1.2"), all=TRUE)

# Remove patient D1.1=9999, D1.2=PR2B (only T12)

df <- df[df$D1.2!="PR2B",]


# Inconsistency: patient with COPD coded as Asthma

df$D1.3_6[df$D1.2=="OH5A"] <- 2

# Re-code height and BMI

# Re-code values in cm to m: 152  156  158  159  160  162  163  165  168  170  174  175  180

df$D3.1_0 <- ifelse(df$D3.1_0>100, df$D3.1_0/100,df$D3.1_0)

#Recalculate BMI D3.3_0

df$D3.3_0 <- df$D3.2_0/(df$D3.1_0)^2


# Primary outcome

# Turn NA ACT/CCQ SCORES not corresponding to the patient

# ACT SCORES 
df[which(df$D1.3_0 == 2 & !is.na(df$D1.3_0)), c("ACT.1_0", "ACT.2_0", "ACT.3_0", "ACT.4_0", "ACT.5_0")] <- NA
df[which(df$D1.3_3 == 2 & !is.na(df$D1.3_3)), c("ACT.1_3", "ACT.2_3", "ACT.3_3", "ACT.4_3", "ACT.5_3")] <- NA
df[which(df$D1.3_6 == 2 & !is.na(df$D1.3_6)), c("ACT.1_6", "ACT.2_6", "ACT.3_6", "ACT.4_6", "ACT.5_6")] <- NA
df[which(df$D1.3_9 == 2 & !is.na(df$D1.3_9)), c("ACT.1_9", "ACT.2_9", "ACT.3_9", "ACT.4_9", "ACT.5_9")] <- NA
df[which(df$D1.3_12 == 2 & !is.na(df$D1.3_12)), c("ACT.1_12", "ACT.2_12", "ACT.3_12", "ACT.4_12", "ACT.5_12")] <- NA

df$ACT.SCORE_0 [df$D1.3_0==2] <- NA
df$ACT.SCORE_3 [df$D1.3_3==2] <- NA
df$ACT.SCORE_6 [df$D1.3_6==2] <- NA
df$ACT.SCORE_9 [df$D1.3_9==2] <- NA
df$ACT.SCORE_12[df$D1.3_12==2] <- NA

# CCQ SCORES

df[which(df$D1.3_0 == 1 & !is.na(df$D1.3_0)), c("CCQ.1_0", "CCQ.2_0", "CCQ.3_0", "CCQ.4_0", "CCQ.5_0", "CCQ.6_0", "CCQ.7_0", "CCQ.8_0", "CCQ.9_0", "CCQ.10_0")] <- NA
df[which(df$D1.3_3 == 1 & !is.na(df$D1.3_3)), c("CCQ.1_3", "CCQ.2_3", "CCQ.3_3", "CCQ.4_3", "CCQ.5_3", "CCQ.6_3", "CCQ.7_3", "CCQ.8_3", "CCQ.9_3", "CCQ.10_3")] <- NA
df[which(df$D1.3_6 == 1 & !is.na(df$D1.3_6)), c("CCQ.1_6", "CCQ.2_6", "CCQ.3_6", "CCQ.4_6", "CCQ.5_6", "CCQ.6_6", "CCQ.7_6", "CCQ.8_6", "CCQ.9_6", "CCQ.10_6")] <- NA
df[which(df$D1.3_9 == 1 & !is.na(df$D1.3_9)), c("CCQ.1_9", "CCQ.2_9", "CCQ.3_9", "CCQ.4_9", "CCQ.5_9", "CCQ.6_9", "CCQ.7_9", "CCQ.8_9", "CCQ.9_9", "CCQ.10_9")] <- NA
df[which(df$D1.3_12 == 1 & !is.na(df$D1.3_12)), c("CCQ.1_12", "CCQ.2_12", "CCQ.3_12", "CCQ.4_12", "CCQ.5_12", "CCQ.6_12", "CCQ.7_12", "CCQ.8_12", "CCQ.9_12", "CCQ.10_12")] <- NA

df$CCQ.SCORE_0 [df$D1.3_0==1] <- NA
df$CCQ.SCORE_3 [df$D1.3_3==1] <- NA
df$CCQ.SCORE_6 [df$D1.3_6==1] <- NA
df$CCQ.SCORE_9 [df$D1.3_9==1] <- NA
df$CCQ.SCORE_12[df$D1.3_12==1] <- NA


# Re-code CCQ score: sum of items/10

df$CCQ.SCORE_0  <- df$CCQ.SCORE_0/10
df$CCQ.SCORE_3  <- df$CCQ.SCORE_3/10
df$CCQ.SCORE_6  <- df$CCQ.SCORE_6/10
df$CCQ.SCORE_9  <- df$CCQ.SCORE_9/10
df$CCQ.SCORE_12 <- df$CCQ.SCORE_12/10

df$CCQ.SCORE_0[df$D1.3_0==2]
df$CCQ.SCORE_3[df$D1.3_0==2]
df$CCQ.SCORE_6[df$D1.3_0==2]
df$CCQ.SCORE_9[df$D1.3_0==2]
df$CCQ.SCORE_12[df$D1.3_0==2]

df_COPD <- df[df$D1.3_0 == 2 & !is.na(df$D1.3_0), ]
df_ASTH <- df[df$D1.3_0 == 1 & !is.na(df$D1.3_0), ]

#Test primary outcomes
#ACT.SCORES: ranges 5-25
table(df$ACT.SCORE_0, useNA="ifany")
table(df$ACT.SCORE_3, useNA="ifany") # 6 patients with ACT score<5
table(df$ACT.SCORE_6, useNA="ifany")
table(df$ACT.SCORE_9, useNA="ifany")
table(df$ACT.SCORE_12, useNA="ifany")

#Replace NA if ACT SCORE is below 5
df$ACT.SCORE_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.SCORE_3)
df$ACT.1_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.1_3)
df$ACT.2_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.2_3)
df$ACT.3_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.3_3)
df$ACT.4_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.4_3)
df$ACT.5_3 <- ifelse(df$ACT.SCORE_3==0, NA, df$ACT.5_3)


#Test primary outcomes
#CCQ.SCORES: ranges 0-6
table(df$CCQ.SCORE_0, useNA="ifany") # Patients with CCQ=0 correspond with EQ5D5L=1
table(df$CCQ.SCORE_3, useNA="ifany") # 2 patients (KK1A, KJ2A) with CCQ=0 and EQ5D5L=0
table(df$CCQ.SCORE_6, useNA="ifany") # Patients with CCQ=0 correspond with EQ5D5L=1
table(df$CCQ.SCORE_9, useNA="ifany") # Patients with CCQ=0 correspond with EQ5D5L=1
table(df$CCQ.SCORE_12, useNA="ifany") # Patients with CCQ=0 correspond with EQ5D5L=1

test_COPD <- df[df$CCQ.SCORE_3==0 & !is.na(df$CCQ.SCORE_3), ]
#hist(df_COPD$CCQ.SCORE_0)

#Replace NA if CCQ SCORE is 0, patients have very different values in other time periods, and EQ5D5L is 0
df$CCQ.SCORE_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.SCORE_3)
df$CCQ.1_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.1_3)
df$CCQ.2_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.2_3)
df$CCQ.3_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.3_3)
df$CCQ.4_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.4_3)
df$CCQ.5_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.5_3)
df$CCQ.6_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.6_3)
df$CCQ.7_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.7_3)
df$CCQ.8_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.8_3)
df$CCQ.9_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.9_3)
df$CCQ.10_3 <- ifelse(df$D1.2=="KK1A", NA, df$CCQ.10_3)

df$CCQ.SCORE_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.SCORE_3)
df$CCQ.1_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.1_3)
df$CCQ.2_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.2_3)
df$CCQ.3_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.3_3)
df$CCQ.4_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.4_3)
df$CCQ.5_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.5_3)
df$CCQ.6_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.6_3)
df$CCQ.7_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.7_3)
df$CCQ.8_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.8_3)
df$CCQ.9_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.9_3)
df$CCQ.10_3 <- ifelse(df$D1.2=="KJ2A", NA, df$CCQ.10_3)

# Symptom = (item 1 + 2 + 5 + 6)/4;
# Functional state = (item 7 + 8 + 9 + 10)/4; 
# Mental state = (item 3 + 4)/2.27

#CCQ scores for symptom, functional state and mental state  (only for T0)

df$CCQ.1_0 <-as.numeric(df$CCQ.1_0)
df$CCQ.2_0 <-as.numeric(df$CCQ.2_0)
df$CCQ.3_0 <-as.numeric(df$CCQ.3_0)
df$CCQ.4_0 <-as.numeric(df$CCQ.4_0)
df$CCQ.5_0 <-as.numeric(df$CCQ.5_0)
df$CCQ.6_0 <-as.numeric(df$CCQ.6_0)
df$CCQ.7_0 <-as.numeric(df$CCQ.7_0)
df$CCQ.8_0 <-as.numeric(df$CCQ.8_0)
df$CCQ.9_0 <-as.numeric(df$CCQ.9_0)
df$CCQ.10_0 <-as.numeric(df$CCQ.10_0)


#CCQ SCORE FOR SYMPTOMS (only for T0)

df$CCQ.symptom_0 <- NA
df$CCQ.symptom_0[df$D1.3_0 == 2] <- rowSums(df[df$D1.3_0 == 2, c("CCQ.1_0", "CCQ.2_0", "CCQ.5_0", "CCQ.6_0")]/4, na.rm = TRUE)
df$CCQ.symptom_0[df$D1.3_0==1]
df$CCQ.symptom_0[df$D1.3_0==2]
#df_test <- df[df$D1.3_0==2& is.na(df$CCQ.symptom_0)==TRUE, ]



#ACTIVITIES  (only for T0)

df$CCQ.functional_0 <- NA
df$CCQ.functional_0[df$D1.3_0==2] <- rowSums(df[df$D1.3_0==2, c("CCQ.7_0","CCQ.8_0", "CCQ.9_0", "CCQ.10_0")]/4, na.rm=TRUE)
df$CCQ.functional_0[df$D1.3_0==1]
df$CCQ.functional_0[df$D1.3_0==2]


# MENTAL (only for T0)
df$CCQ.mental_0 <- NA
df$CCQ.mental_0[df$D1.3_0==2] <- rowSums(df[df$D1.3_0==2, c("CCQ.3_0","CCQ.4_0")]/2.27, na.rm=TRUE)
df$CCQ.mental_0[df$D1.3_0==1]
df$CCQ.mental_0[df$D1.3_0==2]



# Dichotomize ACT and CCQ using the thresholds in the study protocol
# Controlled: ACT≥20 and CCQ<2; Not controlled: ACT<20 and CCQ≥2 

df$ACT_controlled_0 <- ifelse(is.na(df$ACT.SCORE_0), NA, ifelse(df$ACT.SCORE_0 >= 20, 1, 0))
df$ACT_controlled_3 <- ifelse(is.na(df$ACT.SCORE_3), NA, ifelse(df$ACT.SCORE_3 >= 20, 1, 0))
df$ACT_controlled_6 <- ifelse(is.na(df$ACT.SCORE_6), NA, ifelse(df$ACT.SCORE_6 >= 20, 1, 0))
df$ACT_controlled_9 <- ifelse(is.na(df$ACT.SCORE_9), NA, ifelse(df$ACT.SCORE_9 >= 20, 1, 0))
df$ACT_controlled_12 <- ifelse(is.na(df$ACT.SCORE_12), NA, ifelse(df$ACT.SCORE_12 >= 20, 1, 0))

df$CCQ_controlled_0 <- ifelse(is.na(df$CCQ.SCORE_0), NA, ifelse(df$CCQ.SCORE_0<2, 1, 0))
df$CCQ_controlled_3 <- ifelse(is.na(df$CCQ.SCORE_3), NA, ifelse(df$CCQ.SCORE_3<2, 1, 0))
df$CCQ_controlled_6 <- ifelse(is.na(df$CCQ.SCORE_6), NA, ifelse(df$CCQ.SCORE_6<2, 1, 0))
df$CCQ_controlled_9 <- ifelse(is.na(df$CCQ.SCORE_9), NA, ifelse(df$CCQ.SCORE_9<2, 1, 0))
df$CCQ_controlled_12 <- ifelse(is.na(df$CCQ.SCORE_12), NA, ifelse(df$CCQ.SCORE_12<2, 1, 0))

# Create new variable for controlled for all sample (indistinctively of their condition)

df$controlled_0  <- NA 
df$controlled_3  <- NA 
df$controlled_6  <- NA
df$controlled_9  <- NA 
df$controlled_12 <- NA

# Controlled patients in T0

df$controlled_0[df$ACT_controlled_0==1] <- 1
df$controlled_0[df$ACT_controlled_0==0] <- 0
df$controlled_0[df$CCQ_controlled_0==1] <- 1
df$controlled_0[df$CCQ_controlled_0==0] <- 0
sum(is.na(df$controlled_0))
table(df$controlled_0)

# Controlled patients in T3

df$controlled_3[df$ACT_controlled_3==1] <- 1
df$controlled_3[df$ACT_controlled_3==0] <- 0
df$controlled_3[df$CCQ_controlled_3==1] <- 1
df$controlled_3[df$CCQ_controlled_3==0] <- 0
sum(is.na(df$controlled_3))
table(df$controlled_3, useNA="ifany")

# Controlled patients in T6

df$controlled_6[df$ACT_controlled_6==1] <- 1
df$controlled_6[df$ACT_controlled_6==0] <- 0
df$controlled_6[df$CCQ_controlled_6==1] <- 1
df$controlled_6[df$CCQ_controlled_6==0] <- 0
sum(is.na(df$controlled_6))
table(df$controlled_6, useNA="ifany")

# Controlled patients in T9

df$controlled_9[df$ACT_controlled_9==1] <- 1
df$controlled_9[df$ACT_controlled_9==0] <- 0
df$controlled_9[df$CCQ_controlled_9==1] <- 1
df$controlled_9[df$CCQ_controlled_9==0] <- 0
sum(is.na(df$controlled_9))
table(df$controlled_9, useNA="ifany")


# Controlled patients in T12

df$controlled_12[df$ACT_controlled_12==1] <- 1
df$controlled_12[df$ACT_controlled_12==0] <- 0
df$controlled_12[df$CCQ_controlled_12==1] <- 1
df$controlled_12[df$CCQ_controlled_12==0] <- 0
sum(is.na(df$controlled_12))
table(df$controlled_12, useNA="ifany")

### EQ5D5L - utilities 

#Rename the variables:
#Mobility

names(df)[names(df)=="EQ5D5L.1_0"] <- "mobility_0"
names(df)[names(df)=="EQ5D5L.1_3"] <- "mobility_3"
names(df)[names(df)=="EQ5D5L.1_6"] <- "mobility_6"
names(df)[names(df)=="EQ5D5L.1_9"] <- "mobility_9"
names(df)[names(df)=="EQ5D5L.1_12"] <- "mobility_12"

#Selfcare

names(df)[names(df)=="EQ5D5L.2_0"] <- "selfcare_0"
names(df)[names(df)=="EQ5D5L.2_3"] <- "selfcare_3"
names(df)[names(df)=="EQ5D5L.2_6"] <- "selfcare_6"
names(df)[names(df)=="EQ5D5L.2_9"] <- "selfcare_9"
names(df)[names(df)=="EQ5D5L.2_12"] <- "selfcare_12"

#Activity

names(df)[names(df)=="EQ5D5L.3_0"] <- "activity_0"
names(df)[names(df)=="EQ5D5L.3_3"] <- "activity_3"
names(df)[names(df)=="EQ5D5L.3_6"] <- "activity_6"
names(df)[names(df)=="EQ5D5L.3_9"] <- "activity_9"
names(df)[names(df)=="EQ5D5L.3_12"] <- "activity_12"

#Pain

names(df)[names(df)=="EQ5D5L.4_0"] <- "pain_0"
names(df)[names(df)=="EQ5D5L.4_3"] <- "pain_3"
names(df)[names(df)=="EQ5D5L.4_6"] <- "pain_6"
names(df)[names(df)=="EQ5D5L.4_9"] <- "pain_9"
names(df)[names(df)=="EQ5D5L.4_12"] <- "pain_12"

#Anxiety

names(df)[names(df)=="EQ5D5L.5_0"] <- "anxiety_0"
names(df)[names(df)=="EQ5D5L.5_3"] <- "anxiety_3"
names(df)[names(df)=="EQ5D5L.5_6"] <- "anxiety_6"
names(df)[names(df)=="EQ5D5L.5_9"] <- "anxiety_9"
names(df)[names(df)=="EQ5D5L.5_12"] <- "anxiety_12"


#EQ5D5L.SCORE to EQindex
names(df)[names(df)=="EQ5D5L.SCORE_0"] <- "EQindex_0"
names(df)[names(df)=="EQ5D5L.SCORE_3"] <- "EQindex_3"
names(df)[names(df)=="EQ5D5L.SCORE_6"] <- "EQindex_6"
names(df)[names(df)=="EQ5D5L.SCORE_9"] <- "EQindex_9"
names(df)[names(df)=="EQ5D5L.SCORE_12"] <- "EQindex_12"


#Generate new variable disut = disutility

#T0

#T0 disutility by dimensions
df$disut_mo_0 <- NA
df$disut_sf_0 <- NA
df$disut_ua_0 <- NA
df$disut_pd_0 <- NA
df$disut_ad_0 <- NA

#T0: disut mobility
df$disut_mo_0[is.na(df$disut_mo_0) & df$mobility_0==1]<-0
df$disut_mo_0[is.na(df$disut_mo_0) & df$mobility_0==2]<-0.051
df$disut_mo_0[is.na(df$disut_mo_0) & df$mobility_0==3]<-0.064
df$disut_mo_0[is.na(df$disut_mo_0) & df$mobility_0==4]<-0.244
df$disut_mo_0[is.na(df$disut_mo_0) & df$mobility_0==5]<-0.329

#T0: disut self-care
df$disut_sf_0[is.na(df$disut_sf_0) & df$selfcare_0==1]<-0
df$disut_sf_0[is.na(df$disut_sf_0) & df$selfcare_0==2]<-0.046
df$disut_sf_0[is.na(df$disut_sf_0) & df$selfcare_0==3]<-0.056
df$disut_sf_0[is.na(df$disut_sf_0) & df$selfcare_0==4]<-0.216
df$disut_sf_0[is.na(df$disut_sf_0) & df$selfcare_0==5]<-0.257

#T0: disut usual activities
df$disut_ua_0[is.na(df$disut_ua_0) & df$activity_0==1]<-0
df$disut_ua_0[is.na(df$disut_ua_0) & df$activity_0==2]<-0.050
df$disut_ua_0[is.na(df$disut_ua_0) & df$activity_0==3]<-0.064
df$disut_ua_0[is.na(df$disut_ua_0) & df$activity_0==4]<-0.225
df$disut_ua_0[is.na(df$disut_ua_0) & df$activity_0==5]<-0.255

#T0: disut pain and discomfort
df$disut_pd_0[is.na(df$disut_pd_0) & df$pain_0==1]<-0
df$disut_pd_0[is.na(df$disut_pd_0) & df$pain_0==2]<-0.047
df$disut_pd_0[is.na(df$disut_pd_0) & df$pain_0==3]<-0.088
df$disut_pd_0[is.na(df$disut_pd_0) & df$pain_0==4]<-0.353
df$disut_pd_0[is.na(df$disut_pd_0) & df$pain_0==5]<-0.408

#T0: disut anxiety and depression
df$disut_ad_0[is.na(df$disut_ad_0) & df$anxiety_0==1]<-0
df$disut_ad_0[is.na(df$disut_ad_0) & df$anxiety_0==2]<-0.044
df$disut_ad_0[is.na(df$disut_ad_0) & df$anxiety_0==3]<-0.109
df$disut_ad_0[is.na(df$disut_ad_0) & df$anxiety_0==4]<-0.318
df$disut_ad_0[is.na(df$disut_ad_0) & df$anxiety_0==5]<-0.322

#T3

#T3 disutility by dimensions
df$disut_mo_3 <- NA
df$disut_sf_3 <- NA
df$disut_ua_3 <- NA
df$disut_pd_3 <- NA
df$disut_ad_3 <- NA

#T3: disut mobility
df$disut_mo_3[is.na(df$disut_mo_3) & df$mobility_3==1]<-0
df$disut_mo_3[is.na(df$disut_mo_3) & df$mobility_3==2]<-0.051
df$disut_mo_3[is.na(df$disut_mo_3) & df$mobility_3==3]<-0.064
df$disut_mo_3[is.na(df$disut_mo_3) & df$mobility_3==4]<-0.244
df$disut_mo_3[is.na(df$disut_mo_3) & df$mobility_3==5]<-0.329

#T3: disut self-care
df$disut_sf_3[is.na(df$disut_sf_3) & df$selfcare_3==1]<-0
df$disut_sf_3[is.na(df$disut_sf_3) & df$selfcare_3==2]<-0.046
df$disut_sf_3[is.na(df$disut_sf_3) & df$selfcare_3==3]<-0.056
df$disut_sf_3[is.na(df$disut_sf_3) & df$selfcare_3==4]<-0.216
df$disut_sf_3[is.na(df$disut_sf_3) & df$selfcare_3==5]<-0.257

#T3: disut usual activities
df$disut_ua_3[is.na(df$disut_ua_3) & df$activity_3==1]<-0
df$disut_ua_3[is.na(df$disut_ua_3) & df$activity_3==2]<-0.050
df$disut_ua_3[is.na(df$disut_ua_3) & df$activity_3==3]<-0.064
df$disut_ua_3[is.na(df$disut_ua_3) & df$activity_3==4]<-0.225
df$disut_ua_3[is.na(df$disut_ua_3) & df$activity_3==5]<-0.255

#T3: disut pain and discomfort
df$disut_pd_3[is.na(df$disut_pd_3) & df$pain_3==1]<-0
df$disut_pd_3[is.na(df$disut_pd_3) & df$pain_3==2]<-0.047
df$disut_pd_3[is.na(df$disut_pd_3) & df$pain_3==3]<-0.088
df$disut_pd_3[is.na(df$disut_pd_3) & df$pain_3==4]<-0.353
df$disut_pd_3[is.na(df$disut_pd_3) & df$pain_3==5]<-0.408

#T3: disut anxiety and depression
df$disut_ad_3[is.na(df$disut_ad_3) & df$anxiety_3==1]<-0
df$disut_ad_3[is.na(df$disut_ad_3) & df$anxiety_3==2]<-0.044
df$disut_ad_3[is.na(df$disut_ad_3) & df$anxiety_3==3]<-0.109
df$disut_ad_3[is.na(df$disut_ad_3) & df$anxiety_3==4]<-0.318
df$disut_ad_3[is.na(df$disut_ad_3) & df$anxiety_3==5]<-0.322

#T6

#T6 disutility by dimensions
df$disut_mo_6 <- NA
df$disut_sf_6 <- NA
df$disut_ua_6 <- NA
df$disut_pd_6 <- NA
df$disut_ad_6 <- NA

#T6: disut mobility
df$disut_mo_6[is.na(df$disut_mo_6) & df$mobility_6==1]<-0
df$disut_mo_6[is.na(df$disut_mo_6) & df$mobility_6==2]<-0.051
df$disut_mo_6[is.na(df$disut_mo_6) & df$mobility_6==3]<-0.064
df$disut_mo_6[is.na(df$disut_mo_6) & df$mobility_6==4]<-0.244
df$disut_mo_6[is.na(df$disut_mo_6) & df$mobility_6==5]<-0.329

#T6: disut self-care
df$disut_sf_6[is.na(df$disut_sf_6) & df$selfcare_6==1]<-0
df$disut_sf_6[is.na(df$disut_sf_6) & df$selfcare_6==2]<-0.046
df$disut_sf_6[is.na(df$disut_sf_6) & df$selfcare_6==3]<-0.056
df$disut_sf_6[is.na(df$disut_sf_6) & df$selfcare_6==4]<-0.216
df$disut_sf_6[is.na(df$disut_sf_6) & df$selfcare_6==5]<-0.257

#T6: disut usual activities
df$disut_ua_6[is.na(df$disut_ua_6) & df$activity_6==1]<-0
df$disut_ua_6[is.na(df$disut_ua_6) & df$activity_6==2]<-0.050
df$disut_ua_6[is.na(df$disut_ua_6) & df$activity_6==3]<-0.064
df$disut_ua_6[is.na(df$disut_ua_6) & df$activity_6==4]<-0.225
df$disut_ua_6[is.na(df$disut_ua_6) & df$activity_6==5]<-0.255

#T6: disut pain and discomfort
df$disut_pd_6[is.na(df$disut_pd_6) & df$pain_6==1]<-0
df$disut_pd_6[is.na(df$disut_pd_6) & df$pain_6==2]<-0.047
df$disut_pd_6[is.na(df$disut_pd_6) & df$pain_6==3]<-0.088
df$disut_pd_6[is.na(df$disut_pd_6) & df$pain_6==4]<-0.353
df$disut_pd_6[is.na(df$disut_pd_6) & df$pain_6==5]<-0.408

#T6: disut anxiety and depression
df$disut_ad_6[is.na(df$disut_ad_6) & df$anxiety_6==1]<-0
df$disut_ad_6[is.na(df$disut_ad_6) & df$anxiety_6==2]<-0.044
df$disut_ad_6[is.na(df$disut_ad_6) & df$anxiety_6==3]<-0.109
df$disut_ad_6[is.na(df$disut_ad_6) & df$anxiety_6==4]<-0.318
df$disut_ad_6[is.na(df$disut_ad_6) & df$anxiety_6==5]<-0.322

#T9

#T9 disutility by dimensions
df$disut_mo_9 <- NA
df$disut_sf_9 <- NA
df$disut_ua_9 <- NA
df$disut_pd_9 <- NA
df$disut_ad_9 <- NA

#T9: disut mobility
df$disut_mo_9[is.na(df$disut_mo_9) & df$mobility_9==1]<-0
df$disut_mo_9[is.na(df$disut_mo_9) & df$mobility_9==2]<-0.051
df$disut_mo_9[is.na(df$disut_mo_9) & df$mobility_9==3]<-0.064
df$disut_mo_9[is.na(df$disut_mo_9) & df$mobility_9==4]<-0.244
df$disut_mo_9[is.na(df$disut_mo_9) & df$mobility_9==5]<-0.329

#T9: disut self-care
df$disut_sf_9[is.na(df$disut_sf_9) & df$selfcare_9==1]<-0
df$disut_sf_9[is.na(df$disut_sf_9) & df$selfcare_9==2]<-0.046
df$disut_sf_9[is.na(df$disut_sf_9) & df$selfcare_9==3]<-0.056
df$disut_sf_9[is.na(df$disut_sf_9) & df$selfcare_9==4]<-0.216
df$disut_sf_9[is.na(df$disut_sf_9) & df$selfcare_9==5]<-0.257

#T9: disut usual activities
df$disut_ua_9[is.na(df$disut_ua_9) & df$activity_9==1]<-0
df$disut_ua_9[is.na(df$disut_ua_9) & df$activity_9==2]<-0.050
df$disut_ua_9[is.na(df$disut_ua_9) & df$activity_9==3]<-0.064
df$disut_ua_9[is.na(df$disut_ua_9) & df$activity_9==4]<-0.225
df$disut_ua_9[is.na(df$disut_ua_9) & df$activity_9==5]<-0.255

#T9: disut pain and discomfort
df$disut_pd_9[is.na(df$disut_pd_9) & df$pain_9==1]<-0
df$disut_pd_9[is.na(df$disut_pd_9) & df$pain_9==2]<-0.047
df$disut_pd_9[is.na(df$disut_pd_9) & df$pain_9==3]<-0.088
df$disut_pd_9[is.na(df$disut_pd_9) & df$pain_9==4]<-0.353
df$disut_pd_9[is.na(df$disut_pd_9) & df$pain_9==5]<-0.408

#T9: disut anxiety and depression
df$disut_ad_9[is.na(df$disut_ad_9) & df$anxiety_9==1]<-0
df$disut_ad_9[is.na(df$disut_ad_9) & df$anxiety_9==2]<-0.044
df$disut_ad_9[is.na(df$disut_ad_9) & df$anxiety_9==3]<-0.109
df$disut_ad_9[is.na(df$disut_ad_9) & df$anxiety_9==4]<-0.318
df$disut_ad_9[is.na(df$disut_ad_9) & df$anxiety_9==5]<-0.322

#T12

#T12 disutility by dimensions
df$disut_mo_12 <- NA
df$disut_sf_12 <- NA
df$disut_ua_12 <- NA
df$disut_pd_12 <- NA
df$disut_ad_12 <- NA

#T12: disut mobility
df$disut_mo_12[is.na(df$disut_mo_12) & df$mobility_12==1]<-0
df$disut_mo_12[is.na(df$disut_mo_12) & df$mobility_12==2]<-0.051
df$disut_mo_12[is.na(df$disut_mo_12) & df$mobility_12==3]<-0.064
df$disut_mo_12[is.na(df$disut_mo_12) & df$mobility_12==4]<-0.244
df$disut_mo_12[is.na(df$disut_mo_12) & df$mobility_12==5]<-0.329

#T12: disut self-care
df$disut_sf_12[is.na(df$disut_sf_12) & df$selfcare_12==1]<-0
df$disut_sf_12[is.na(df$disut_sf_12) & df$selfcare_12==2]<-0.046
df$disut_sf_12[is.na(df$disut_sf_12) & df$selfcare_12==3]<-0.056
df$disut_sf_12[is.na(df$disut_sf_12) & df$selfcare_12==4]<-0.216
df$disut_sf_12[is.na(df$disut_sf_12) & df$selfcare_12==5]<-0.257

#T12: disut usual activities
df$disut_ua_12[is.na(df$disut_ua_12) & df$activity_12==1]<-0
df$disut_ua_12[is.na(df$disut_ua_12) & df$activity_12==2]<-0.050
df$disut_ua_12[is.na(df$disut_ua_12) & df$activity_12==3]<-0.064
df$disut_ua_12[is.na(df$disut_ua_12) & df$activity_12==4]<-0.225
df$disut_ua_12[is.na(df$disut_ua_12) & df$activity_12==5]<-0.255

#T12: disut pain and discomfort
df$disut_pd_12[is.na(df$disut_pd_12) & df$pain_12==1]<-0
df$disut_pd_12[is.na(df$disut_pd_12) & df$pain_12==2]<-0.047
df$disut_pd_12[is.na(df$disut_pd_12) & df$pain_12==3]<-0.088
df$disut_pd_12[is.na(df$disut_pd_12) & df$pain_12==4]<-0.353
df$disut_pd_12[is.na(df$disut_pd_12) & df$pain_12==5]<-0.408

#T12: disut anxiety and depression
df$disut_ad_12[is.na(df$disut_ad_12) & df$anxiety_12==1]<-0
df$disut_ad_12[is.na(df$disut_ad_12) & df$anxiety_12==2]<-0.044
df$disut_ad_12[is.na(df$disut_ad_12) & df$anxiety_12==3]<-0.109
df$disut_ad_12[is.na(df$disut_ad_12) & df$anxiety_12==4]<-0.318
df$disut_ad_12[is.na(df$disut_ad_12) & df$anxiety_12==5]<-0.322


#Total disutility by study period: SUM OF ALL DISUTILITY COMPONENTS 


df$total_disut_0 <- rowSums(df[,c('disut_mo_0','disut_sf_0','disut_ua_0','disut_pd_0','disut_ad_0')])
df$total_disut_3 <- rowSums(df[,c('disut_mo_3','disut_sf_3','disut_ua_3','disut_pd_3','disut_ad_3')])
df$total_disut_6 <- rowSums(df[,c('disut_mo_6','disut_sf_6','disut_ua_6','disut_pd_6','disut_ad_6')])
df$total_disut_9 <- rowSums(df[,c('disut_mo_9','disut_sf_9','disut_ua_9','disut_pd_9','disut_ad_9')])
df$total_disut_12 <- rowSums(df[,c('disut_mo_12','disut_sf_12','disut_ua_12','disut_pd_12','disut_ad_12')])

sum(is.na(df$total_disut_0)) #0
sum(is.na(df$total_disut_3)) #22
sum(is.na(df$total_disut_6)) #41
sum(is.na(df$total_disut_9)) #43
sum(is.na(df$total_disut_12)) #68

miss_total_disut <- df[is.na(df$total_disut_3), ]


# Calculate EQindex (1- total disutility)

df$EQindex_0  <- 1-df$total_disut_0
df$EQindex_3  <- 1-df$total_disut_3
df$EQindex_6  <- 1-df$total_disut_6
df$EQindex_9  <- 1-df$total_disut_9
df$EQindex_12 <- 1-df$total_disut_12

df$EQindex_0  <- round(df$EQindex_0, 3)
df$EQindex_3  <- round(df$EQindex_3, 3)
df$EQindex_6  <- round(df$EQindex_6, 3)
df$EQindex_9  <- round(df$EQindex_9, 3)
df$EQindex_12 <- round(df$EQindex_12, 3)

EQindex0_na <-  df %>%
               filter(mobility_0==0,selfcare_0==0, activity_0==0,pain_0==0,anxiety_0==0) %>%
               select(D1.2)%>%
               pull(D1.2)

EQindex3_na <-  df %>%
              filter(mobility_3==0,selfcare_3==0,activity_3==0,pain_3==0,anxiety_3==0) %>%
              select(D1.2)%>%
              pull(D1.2)

df$EQindex_3[df$D1.2 %in% EQindex3_na] <-NA

EQindex6_na <-  df %>%
  filter(mobility_6==0,selfcare_6==0,activity_6==0,pain_6==0,anxiety_6==0) %>%
  select(D1.2)%>%
  pull(D1.2)

df$EQindex_6[df$D1.2 %in% EQindex6_na] <-NA

EQindex9_na <-  df %>%
  filter(mobility_9==0,selfcare_9==0,activity_9==0,pain_9==0,anxiety_9==0) %>%
  select(D1.2)%>%
  pull(D1.2)

EQindex12_na <-  df %>%
  filter(mobility_12==0,selfcare_12==0,activity_12==0,pain_12==0,anxiety_12==0) %>%
  select(D1.2)%>%
  pull(D1.2)

df$EQindex_12[df$D1.2 %in% EQindex12_na] <-NA

sum(is.na(df$EQindex_3))

#Cost data:

df <- merge(df, economic_data, by="D1.2", all=TRUE)



#Completeness

#Based on the questionnaire:

#ACT SCORE and EQindex


complete_qASTH <- complete.cases(df[, c("ACT.SCORE_0", "ACT.SCORE_3", "ACT.SCORE_6", 
                                        "ACT.SCORE_9", "ACT.SCORE_12", 
                                        "EQindex_0", "EQindex_3", "EQindex_6", 
                                        "EQindex_9", "EQindex_12")])

#CCQ SCORE and EQindex

complete_qCOPD <- complete.cases(df[, c("CCQ.SCORE_0", "CCQ.SCORE_3", "CCQ.SCORE_6", 
                                        "CCQ.SCORE_9", "CCQ.SCORE_12", 
                                        "EQindex_0", "EQindex_3", "EQindex_6", 
                                        "EQindex_9", "EQindex_12")])

# Complete questionnaire

df$complete_q <- 0
df$complete_q[complete_qASTH] <- 1
df$complete_q[complete_qCOPD] <- 1
sum(is.na(df$complete_q))

test_complete_qASTH <- df[df$complete_q==1 & df$D1.3_0==1, c("D1.2", "ACT.SCORE_0", "ACT.SCORE_3", "ACT.SCORE_6", "ACT.SCORE_9", "ACT.SCORE_12",
                                                  "EQindex_0", "EQindex_3", "EQindex_6", "EQindex_9", "EQindex_12")]

# JR4B: NA IN ACTSCORE3 AND EQINDEX3
# LP5B: NA IN ACTSCORE3
# PJ8A: NA IN ACTSCORE3 AND EQINDEX3
# QF8A: NA IN ACTSCORE3 AND EQINDEX3
# QX7A: NA IN EQINDEX6
# SV3B: NA IN ACTSCORE3 AND EQINDEX3
# VB4A: NA IN ACTSCORE3 AND EQINDEX3
# XY5A: NA IN EQINDEX6 

test_complete_qCOPD <- df[df$complete_q==1 & df$D1.3_0==2, c("D1.2","CCQ.SCORE_0", "CCQ.SCORE_3", "CCQ.SCORE_6", 
                                                             "CCQ.SCORE_9", "CCQ.SCORE_12", 
                                                             "EQindex_0", "EQindex_3", "EQindex_6", 
                                                             "EQindex_9", "EQindex_12")]
# KJ2A: NA IN CCQSCORE3 AND EQINDEX3
# KK1A: NA IN CCQSCORE3 AND EQINDEX3

# Replace Patients with NA to 0: PATIENTS WITH eqindex =0
df$complete_q[df$D1.2=="JR4B"] <- 0
df$complete_q[df$D1.2=="LP5B"] <- 0
df$complete_q[df$D1.2=="PJ8A"] <- 0
df$complete_q[df$D1.2=="QF8A"] <- 0
df$complete_q[df$D1.2=="QX7A"] <- 0
df$complete_q[df$D1.2=="SV3B"] <- 0
df$complete_q[df$D1.2=="VB4A"] <- 0
df$complete_q[df$D1.2=="XY5A"] <- 0
df$complete_q[df$D1.2=="KJ2A"] <- 0
df$complete_q[df$D1.2=="KK1A"] <- 0


table(df$complete_q)



#Based on the cost data: 

df$complete_c <- 0
complete_cost_data <- complete.cases(df[, c("cost_C6", "cost_C12", "cost_M6", 
                                            "cost_M12", "cost_F6", 
                                            "cost_F12", "cost_H6", "cost_H12", 
                                            "cost_O6", "cost_O12")])
  

#Patients with cost data for all study periods 
df$complete_c[complete_cost_data] <-  1

#Other patients that have either cost or data on resource use: 
other_patients <- df %>%
                  filter(complete_c==0) %>%
                  select(D1.2) %>%
                  pull(D1.2)


# Completeness for cost from 0-6 months of the trial and use of resources in the last 6 months at T6
df_other_patients_complete6 <-df[df$D1.2 %in% other_patients,] 
is_complete <- function(cost_C6, cost_M6, cost_F6, cost_H6, cost_O6, D3_10_1_6) {
  cost_vars <- c(cost_C6, cost_M6, cost_F6, cost_H6, cost_O6)
  if (all(is.na(cost_vars)) && is.na(D3_10_1_6)) {
    return(0)
  } else {
    return(1)
  }
}

# Apply the function to each row of the filtered dataframe
df_other_patients_complete6$complete_c6 <- apply(df_other_patients_complete6 [, c("cost_C6", "cost_M6", "cost_F6", "cost_H6", "cost_O6", "D3.10_1_6")], 1, 
                                function(x) is_complete(x[1], x[2], x[3], x[4], x[5], x[6]))

# Update the original dataframe with the new complete_c values
df$complete_c6[df$D1.2 %in% other_patients] <- df_other_patients_complete6$complete_c6



#Completeness for cost from 6-12 months of the trial and use of resources in the last 6 months at T12
  
df_other_patients_complete12 <-df[df$D1.2 %in% other_patients,] 
is_complete <- function(cost_C12, cost_M12, cost_F12, cost_H12, cost_O12, D3_10_1_12) {
  cost_vars <- c(cost_C12, cost_M12, cost_F12, cost_H12, cost_O12)
  if (all(is.na(cost_vars)) && is.na(D3_10_1_12)) {
    return(0)
  } else {
    return(1)
  }
}

# Apply the function to each row of the filtered dataframe
df_other_patients_complete12$complete_c12 <- apply(df_other_patients_complete12 [, c("cost_C12", "cost_M12", "cost_F12", "cost_H12", "cost_O12", "D3.10_1_12")], 1, 
                                                function(x) is_complete(x[1], x[2], x[3], x[4], x[5], x[6]))

# Update the original dataframe with the new complete_c values
df$complete_c12[df$D1.2 %in% other_patients] <- df_other_patients_complete12$complete_c12
                                      

#Completeness if complete_c6 and complete_12 are =1.

table(df$complete_c)
table(df$complete_c6)
table(df$complete_c12)

df$complete_c[df$complete_c6==1 & df$complete_c12==1] <-  1

#Patients without complete cost data
missing_pat_costs <- df[df$complete_c==0,]
# Missing patients; XW0A, XZ4A

#Completeness for complete data frame:

df$complete <- NA
df$complete[df$complete_q==1 & df$complete_c==1] <- 1
df$complete[df$complete_q==0 & df$complete_c==1] <- 0
df$complete[df$complete_q==1 & df$complete_c==0] <- 0
df$complete[df$complete_q==0 & df$complete_c==0] <- 0

table(df$complete, useNA="ifany")



####Consort diagram
df_complete <- df[df$complete==1, ]
lost_follow_up <- anti_join(df, df_complete) %>% select(D1.2) %>% pull(D1.2) 

#JR4B SV3B LP5B PJ8A QF8A VB4A

# T3
# JR4B: NA IN ACTSCORE3 AND EQINDEX3 - CONTROL GROUP
# LP5B: NA IN ACTSCORE3 - CONTROL GROUP
# PJ8A: NA IN ACTSCORE3 AND EQINDEX3 - INTERVENTION GROUP
# QF8A: NA IN ACTSCORE3 AND EQINDEX3 - INTERVENTION GROUP
# SV3B: NA IN ACTSCORE3 AND EQINDEX3 - CONTROL GROUP
# VB4A: NA IN ACTSCORE3 AND EQINDEX3 - INTERVENTION GROUP

# KJ2A: NA IN CCQSCORE3 AND EQINDEX3
# KK1A: NA IN CCQSCORE3 AND EQINDEX3

miss_pat_copd <- df[df$D1.2=="KJ2A",]

df_incomplete <- df[df$complete==0,]
table(df_incomplete$D1.3_3, useNA="ifany")

misst3 <- df %>%
          filter(is.na(D1.3_3)) %>% 
          select(D1.2) %>% 
          pull(D1.2) 

misst3 <- df[df$D1.2 %in% misst3,]

#"AS7B" "BF5A" "FP9A" "HA0A" "JR6A" "KJ1A" "OH4A" "PA4A" "QP7B" "RM5A" "XJ3A" "XW0A" "XZ4A" "ZA9B"



# T6
# XY5A: NA IN EQINDEX6 
# QX7A: NA IN EQINDEX6

p_XY5A<-df[df$D1.2=="XY5A",]
p_QX7A<-df[df$D1.2=="QX7A",]


misst6 <- df %>%
  filter(is.na(D1.3_6)) %>% 
  select(D1.2) %>% 
  pull(D1.2) 

misst6 <- df[df$D1.2 %in% misst6,]


# T9

misst9 <- df %>%
  filter(is.na(D1.3_9)) %>% 
  select(D1.2) %>% 
  pull(D1.2) 

misst9 <- df[df$D1.2 %in% misst9,]


# T12

misst12 <- df %>%
  filter(is.na(D1.3_12)) %>% 
  select(D1.2) %>% 
  pull(D1.2) 

misst12 <- df[df$D1.2 %in% misst12,]

p_CJ3B <- df[df$D1.2=="CJ3B",]
p_XW0A <- df[df$D1.2=="XW0A",]
p_XZ4A <- df[df$D1.2=="XZ4A",]


table(df$complete)
df_complete <- df[df$complete==1,]
table(df_complete$D1.3_0.x, df_complete$D1.4_0.x)


#statistics and tests 

# Asthma and intervention group

#mean(economic_data$cost1A_C6, na.rm=TRUE)
#sd(economic_data$cost1A_C6, na.rm=TRUE)
#mean(economic_data$cost1A_C12, na.rm=TRUE)
#sd(economic_data$cost1A_C12, na.rm=TRUE)

mean(economic_data$cost1A_M6, na.rm=TRUE)
sd(economic_data$cost1A_M6, na.rm=TRUE)
mean(economic_data$cost1A_M12, na.rm=TRUE)
sd(economic_data$cost1A_M12, na.rm=TRUE)

mean(economic_data$cost1A_F6, na.rm=TRUE)
sd(economic_data$cost1A_F6, na.rm=TRUE)
mean(economic_data$cost1A_F12, na.rm=TRUE)
sd(economic_data$cost1A_F12, na.rm=TRUE)

