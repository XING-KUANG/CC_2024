if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

library(readxl)
library(dplyr)
library(tidyr)

input_files <- c(
  country_incidence = file.path(RAW_DATA_DIR, "country_incidence_all_ages.xlsx"),
  country_mortality = file.path(RAW_DATA_DIR, "country_mortality_all_ages.xlsx"),
  regions_all = file.path(RAW_DATA_DIR, "regions_all_ages.xlsx"),
  regions_15_49 = file.path(RAW_DATA_DIR, "regions_age_15_49.xlsx"),
  regions_50_plus = file.path(RAW_DATA_DIR, "regions_age_50_plus.xlsx"),
  trend_all = file.path(RAW_DATA_DIR, "trend_all_ages.xlsx"),
  eapc_all = file.path(RAW_DATA_DIR, "eapc_all_ages.xlsx"),
  eapc_15_49 = file.path(RAW_DATA_DIR, "eapc_age_15_49.xlsx"),
  eapc_50_plus = file.path(RAW_DATA_DIR, "eapc_age_50_plus.xlsx"),
  development_groups = file.path(RAW_DATA_DIR, "age_specific_development_groups.xlsx"),
  crosswalk = file.path(PROJECT_ROOT, "data", "reference", "country_region_crosswalk.csv")
)
require_files(input_files)

crosswalk <- read.csv(input_files[["crosswalk"]], check.names = FALSE)

read_country_rate <- function(path, rate_name, count_name) {
  raw <- read_excel(path, sheet = "Dataset")
  required <- c("Population", "Alpha-3 code", "ASR (World) per 100 000", "Total")
  if (!all(required %in% names(raw))) {
    alt_alpha <- grep("Alpha.*3 code", names(raw), value = TRUE)
    if (length(alt_alpha) == 1) names(raw)[names(raw) == alt_alpha] <- "Alpha-3 code"
  }
  if (!all(required %in% names(raw))) {
    stop("Unexpected country-rate export columns in: ", basename(path))
  }
  raw %>%
    transmute(
      Label = Population,
      Alpha3 = `Alpha-3 code`,
      !!rate_name := `ASR (World) per 100 000`,
      !!count_name := Total
    )
}

country_data <- read_country_rate(input_files[["country_incidence"]], "ASIR", "Cases") %>%
  left_join(
    read_country_rate(input_files[["country_mortality"]], "ASMR", "Deaths") %>%
      select(Label, ASMR, Deaths),
    by = "Label"
  ) %>%
  left_join(crosswalk, by = "Label")

read_regional_burden <- function(path, suffix = "") {
  raw <- read_excel(path, sheet = "Dataset")
  required <- c("Label", "Type", "ASR (World)", "Total")
  if (!all(required %in% names(raw))) {
    stop("Unexpected regional export columns in: ", basename(path))
  }
  incidence <- raw %>%
    filter(Type == 0) %>%
    transmute(Label, ASIR = `ASR (World)`, Cases = Total)
  mortality <- raw %>%
    filter(Type == 1) %>%
    transmute(Label, ASMR = `ASR (World)`, Deaths = Total)
  out <- left_join(incidence, mortality, by = "Label")
  if (nzchar(suffix)) {
    names(out)[-1] <- paste0(names(out)[-1], suffix)
  }
  out
}

region_all <- read_regional_burden(input_files[["regions_all"]])
region_age <- read_regional_burden(input_files[["regions_15_49"]], "_15_49") %>%
  left_join(
    read_regional_burden(input_files[["regions_50_plus"]], "_50_plus"),
    by = "Label"
  )

excluded_populations <- c(
  "UK, England", "UK, England and wales", "UK, Northern Ireland",
  "UK, Scotland", "UK, Wales", "USA: Black", "USA: White"
)
population_aliases <- c(
  "USA" = "United States of America",
  "Moldova" = "Republic of Moldova"
)

normalise_population <- function(x) {
  mapped <- unname(population_aliases[x])
  ifelse(is.na(mapped), x, mapped)
}

trend_raw <- read_excel(input_files[["trend_all"]], sheet = "Dataset")
trend_data <- trend_raw %>%
  filter(!(`Country label` %in% excluded_populations), Year >= 1990) %>%
  mutate(
    Label = normalise_population(`Country label`),
    Type = ifelse(Type == 0, "Incidence", "Mortality"),
    AgeGroup = "All ages"
  ) %>%
  select(
    CancerLabel = `Cancer label`, Label, Type, Year,
    ASR = `ASR (World)`, Total, AgeGroup
  ) %>%
  inner_join(crosswalk, by = "Label")

read_eapc <- function(path, age_group) {
  raw <- read_excel(path, sheet = "Dataset")
  required <- c(
    "Population id", "Type", "EAPC Period", "EAPC", "EAPC up", "EAPC low"
  )
  if (!all(required %in% names(raw))) {
    stop("Unexpected EAPC export columns in: ", basename(path))
  }
  raw %>%
    distinct(across(all_of(required))) %>%
    mutate(
      Label = normalise_population(`Population id`),
      Type = ifelse(Type == 0, "Incidence", "Mortality"),
      AgeGroup = age_group
    ) %>%
    filter(!(Label %in% excluded_populations)) %>%
    inner_join(crosswalk, by = "Label")
}

eapc_data <- bind_rows(
  read_eapc(input_files[["eapc_all"]], "All ages"),
  read_eapc(input_files[["eapc_15_49"]], "15-49"),
  read_eapc(input_files[["eapc_50_plus"]], "50 years or older")
)

development_raw <- read_excel(input_files[["development_groups"]], sheet = "Dataset")
development_data <- development_raw %>%
  mutate(
    Type = ifelse(Type == 0, "Incidence", "Mortality"),
    Age = factor(Age, levels = c("15-29", "30-44", "45-59", "60-74", "75-85+"))
  ) %>%
  select(Label, Type, Age, ASR = `ASR (World)`, Total) %>%
  arrange(Label, Type, Age)

write.csv(
  country_data, file.path(DERIVED_DATA_DIR, "country_rates_2024.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  region_all, file.path(DERIVED_DATA_DIR, "regional_burden_2024.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  region_age, file.path(DERIVED_DATA_DIR, "regional_age_burden_2024.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  trend_data, file.path(DERIVED_DATA_DIR, "country_temporal_trends.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  eapc_data, file.path(DERIVED_DATA_DIR, "country_eapc.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  development_data,
  file.path(DERIVED_DATA_DIR, "age_specific_development_groups.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

message("Derived analysis datasets were written to: ", DERIVED_DATA_DIR)
