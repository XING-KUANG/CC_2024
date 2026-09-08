if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

library(dplyr)

country_file <- file.path(DERIVED_DATA_DIR, "country_rates_2024.csv")
require_files(country_file)

table_s1 <- read.csv(country_file, check.names = FALSE) %>%
  transmute(
    Country = Label,
    `ISO alpha-3 code` = Alpha3,
    `New cases` = Cases,
    ASIR = round(ASIR, 2),
    Deaths = Deaths,
    ASMR = round(ASMR, 2),
    `GLOBOCAN region` = Region
  ) %>%
  arrange(desc(`New cases`))

print(table_s1)
export_table("table_s1_country_burden.csv", table_s1)
