library(haven)
library(dplyr)

T0 <- read_sav('raw_data/T0.sav')
all_cases <- readRDS('data_processed/all_cases.rds')

cat('=== DATA INTEGRITY CHECK ===\n')
cat('T0 baseline rows:', nrow(T0), '\n')
cat('all_cases rows:', nrow(all_cases), '\n')
cat('Difference:', nrow(all_cases) - nrow(T0), '\n\n')

cat('Unique D1.2 patient IDs:\n')
cat('  T0:', length(unique(T0$D1.2)), '\n')
cat('  all_cases:', length(unique(all_cases$D1.2)), '\n\n')

# Check for duplicates in all_cases
dup_in_all <- all_cases %>% group_by(D1.2) %>% summarise(n = n()) %>% filter(n > 1)
cat('Patients with multiple rows in all_cases:', nrow(dup_in_all), '\n')

if (nrow(dup_in_all) > 0) {
  cat('\nPatient IDs with duplicates:\n')
  print(dup_in_all)
}

# Check if any patients appear without baseline
patients_in_all <- unique(all_cases$D1.2)
patients_in_t0 <- unique(T0$D1.2)

patients_only_in_all <- setdiff(patients_in_all, patients_in_t0)
cat('\n\nPatients in all_cases but NOT in T0 (baseline):', length(patients_only_in_all), '\n')
if (length(patients_only_in_all) > 0) {
  cat('  IDs:', paste(patients_only_in_all, collapse = ', '), '\n')
}
