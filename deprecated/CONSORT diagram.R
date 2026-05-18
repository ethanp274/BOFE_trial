##############################
### CONSORT FLOW DIAGRAM #####
##############################

consort  <- data.frame(Screening = numeric(0), 
                       Recruitment = numeric(0), 
                       Randomization = numeric(0), 
                       `Follow-up T0` = numeric(0), 
                       `Follow-up T3` = numeric(0), 
                       `Follow-up T6` = numeric(0), 
                       `Follow-up T9` = numeric(0), 
                       `Follow-up T12` = numeric(0), 
                       Analysis = numeric(0))


consort
###########################################
#https://github.com/GerkeLab/consoRt

install.packages('devtools')
library(devtools)
devtools::install_github("gerkelab/consoRt@dev")
library(consoRt)
study_data <- consoRt::study_data
consort <- write_consort(study_data, "man/figures/README-consort-diagram.png")

# Create the dataframe
consort <- data.frame(
  Phase = c('Screening', 'Recruitment', 'Randomization', 'Follow-up T0', 'Follow-up T3', 'Follow-up T6', 'Follow-up T9', 'Follow-up T12', 'Analysis'),
  Intervention_Asthma = c(50, 45, 40, 38, 35, 33, 30, 28, 25),
  Intervention_COPD = c(50, 45, 40, 37, 35, 32, 30, 27, 25),
  Control_Asthma = c(50, 45, 40, 39, 36, 34, 31, 29, 26),
  Control_COPD = c(50, 45, 40, 38, 35, 32, 30, 28, 25)
)

# Install devtools if not already installed
# install.packages("devtools")

# Install consoRt from GitHub
devtools::install_github("mathesong/consoRt")

# Load the consoRt package
library(consoRt)


library(consoRt)

# Define the data for the consort diagram
consort_data <- list(
  n.screened = 100,
  n.excluded = 10,
  n.randomized = 90,
  n.group1 = list(
    label = "Intervention",
    n = 45,
    conditions = list(
      asthma = list(
        label = "Asthma",
        n = 25
      ),
      copd = list(
        label = "COPD",
        n = 20
      )
    )
  ),
  n.group2 = list(
    label = "Control",
    n = 45,
    conditions = list(
      asthma = list(
        label = "Asthma",
        n = 22
      ),
      copd = list(
        label = "COPD",
        n = 23
      )
    )
  ),
  n.followup = list(
    t0 = list(
      n.group1 = list(
        asthma = 23,
        copd = 18
      ),
      n.group2 = list(
        asthma = 21,
        copd = 22
      )
    ),
    t3 = list(
      n.group1 = list(
        asthma = 22,
        copd = 17
      ),
      n.group2 = list(
        asthma = 20,
        copd = 21
      )
    ),
    t6 = list(
      n.group1 = list(
        asthma = 21,
        copd = 16
      ),
      n.group2 = list(
        asthma = 19,
        copd = 20
      )
    ),
    t9 = list(
      n.group1 = list(
        asthma = 20,
        copd = 15
      ),
      n.group2 = list(
        asthma = 18,
        copd = 19
      )
    ),
    t12 = list(
      n.group1 = list(
        asthma = 19,
        copd = 14
      ),
      n.group2 = list(
        asthma = 17,
        copd = 18
      )
    )
  ),
  n.analyzed = list(
    n.group1 = list(
      asthma = 18,
      copd = 13
    ),
    n.group2 = list(
      asthma = 16,
      copd = 17
    )
  )
)

# Generate the consort diagram
consort_plot <- consort_plot(consort_data)

# Display the consort diagram
print(consort_plot)


# Install consort package if not already installed
install.packages("consort")

# Load the consort package
library(consort)
# Create the dataframe
consortdata <- data.frame(
  Phase = c('Screening', 'Recruitment', 'Randomization', 'Follow-up T0', 'Follow-up T3', 'Follow-up T6', 'Follow-up T9', 'Follow-up T12', 'Analysis'),
  Intervention_Asthma = c(50, 45, 40, 38, 35, 33, 30, 28, 25),
  Intervention_COPD = c(50, 45, 40, 37, 35, 32, 30, 27, 25),
  Control_Asthma = c(50, 45, 40, 39, 36, 34, 31, 29, 26),
  Control_COPD = c(50, 45, 40, 38, 35, 32, 30, 28, 25)
)

library(consort)

# Create the consort diagram
consort_plot <- consort_plot(
  n.screened = 200,
  n.excluded = 40,
  n.eligible = 160,
  n.randomized = 160,
  groups = list(
    intervention = list(
      label = "Intervention",
      n = 80,
      subgroups = list(
        asthma = list(label = "Asthma", n = 40),
        copd = list(label = "COPD", n = 40)
      ),
      follow_up = list(
        T0 = list(n = 78),
        T3 = list(n = 75),
        T6 = list(n = 70),
        T9 = list(n = 65),
        T12 = list(n = 60)
      ),
      analyzed = list(
        asthma = 35,
        copd = 30
      )
    ),
    control = list(
      label = "Control",
      n = 80,
      subgroups = list(
        asthma = list(label = "Asthma", n = 38),
        copd = list(label = "COPD", n = 42)
      ),
      follow_up = list(
        T0 = list(n = 77),
        T3 = list(n = 74),
        T6 = list(n = 69),
        T9 = list(n = 64),
        T12 = list(n = 59)
      ),
      analyzed = list(
        asthma = 34,
        copd = 31
      )
    )
  )
)

# Display the consort diagram
print(consort_plot)
