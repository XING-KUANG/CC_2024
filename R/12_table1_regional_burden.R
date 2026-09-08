if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

library(dplyr)

all_ages_file <- file.path(DERIVED_DATA_DIR, "regional_burden_2024.csv")
age_file <- file.path(DERIVED_DATA_DIR, "regional_age_burden_2024.csv")
require_files(c(all_ages_file, age_file))

all_ages <- read.csv(all_ages_file, check.names = FALSE)
age_groups <- read.csv(age_file, check.names = FALSE)

table_1 <- all_ages %>%
  left_join(age_groups, by = "Label") %>%
  transmute(
    Region = Label,
    `ASIR (all ages)` = round(ASIR, 2),
    `ASMR (all ages)` = round(ASMR, 2),
    `Cases (all ages)` = Cases,
    `Deaths (all ages)` = Deaths,
    `ASIR (15-49 years)` = round(ASIR_15_49, 2),
    `ASMR (15-49 years)` = round(ASMR_15_49, 2),
    `Cases (15-49 years)` = Cases_15_49,
    `Deaths (15-49 years)` = Deaths_15_49,
    `ASIR (50 years or older)` = round(ASIR_50_plus, 2),
    `ASMR (50 years or older)` = round(ASMR_50_plus, 2),
    `Cases (50 years or older)` = Cases_50_plus,
    `Deaths (50 years or older)` = Deaths_50_plus
  ) %>%
  arrange(desc(`Cases (all ages)`))

print(table_1)
export_table("table_1_regional_burden.csv", table_1)
