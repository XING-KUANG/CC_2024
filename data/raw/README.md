# Required source-data exports

Place the following files in this directory. Each export should use cervical
cancer (`C53`, Cervix uteri), females, and the population grouping stated in
the filename.

## Cancer Today

- `country_incidence_all_ages.xlsx`: 186 countries, incidence, all ages.
- `country_mortality_all_ages.xlsx`: 186 countries, mortality, all ages.
- `regions_all_ages.xlsx`: World plus 21 regions, incidence and mortality, all ages.
- `regions_age_15_49.xlsx`: World plus 21 regions, incidence and mortality, ages 15-49.
- `regions_age_50_plus.xlsx`: World plus 21 regions, incidence and mortality, ages 50-85+.
- `age_specific_development_groups.xlsx`: World, source-defined development
  groups, China, and India; incidence and mortality; exported age bands.
- `globocan_2024_annex_a.xlsx`: IARC Annex A estimation-method classifications.

## Cancer Over Time

- `trend_all_ages.xlsx`: annual incidence and mortality rates, all ages.
- `eapc_all_ages.xlsx`: population-specific EAPCs, all ages.
- `eapc_age_15_49.xlsx`: population-specific EAPCs, ages 15-49.
- `eapc_age_50_plus.xlsx`: population-specific EAPCs, ages 50-85+.

## Cancer Tomorrow

- `global_projection_scenarios.xlsx`: global incidence and mortality projections
  under the constant-risk and six alternative annual rate-change scenarios.
- `regional_projection_constant_risk.xlsx`: projections for World plus 21 regions
  under the constant-risk demographic scenario.

The scripts validate required columns before analysis. The raw exports are not
tracked by Git; do not commit them unless redistribution is permitted by the
data provider.

