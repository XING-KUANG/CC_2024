# Global cervical cancer burden in 2024: analysis code

This repository contains the R code used to produce the tables and figures in
the revised manuscript. It is organised to make the analytical workflow
transparent while keeping source data and generated outputs separate from the
code.

## Repository contents

- `R/00_config.R`: paths, package checks, and output controls.
- `R/01_prepare_data.R`: preparation of country, regional, temporal-trend,
  EAPC, and age-specific analysis datasets.
- `R/02_figure1_global_maps.R`: Figure 1.
- `R/03_figure2a_age_group_rates.R`: Figure 2A.
- `R/04_figure2b_age_group_counts.R`: Figure 2B.
- `R/05_figure2cd_age_specific_development_groups.R`: Figure 2C-D.
- `R/06_figure3_temporal_trajectories.R`: Figure 3.
- `R/07_figure4_eapc_forest.R`: Figure 4.
- `R/08_figure5ab_global_scenarios.R`: Figure 5A-B.
- `R/09_figure5cd_regional_projection.R`: Figure 5C-D.
- `R/10_figures_s2_s3_age_specific_eapc.R`: Supplementary Figures S2-S3.
- `R/11_figure_s4_estimation_methods.R`: Supplementary Figure S4.
- `R/12_table1_regional_burden.R`: Table 1.
- `R/13_table_s1_country_burden.R`: Supplementary Table S1.
- `R/99_session_info.R`: exact R and package versions for the executing system.
- `run_all.R`: runs the workflow in manuscript order.

## Data access

The analyses use aggregate, deidentified data from the International Agency for
Research on Cancer Global Cancer Observatory:

- Cancer Today: <https://gco.iarc.who.int/today>
- Cancer Over Time: <https://gco.iarc.who.int/overtime>
- Cancer Tomorrow: <https://gco.iarc.who.int/tomorrow>

Download the required exports using the settings documented in
`data/raw/README.md`, rename them to the English filenames listed there, and
place them in `data/raw/`. Source files are excluded from version control so
that their redistribution remains governed by IARC's terms.

## Running the analysis

Use R from the repository root:

```r
source("run_all.R", encoding = "UTF-8")
```

Figures are displayed in the active R graphics device and are not written to
disk by default. To export figures and tables into `outputs/`, set the
environment variable before running the workflow:

```r
Sys.setenv(EXPORT_OUTPUTS = "true")
source("run_all.R", encoding = "UTF-8")
```

Run `source("R/99_session_info.R")` in the final analysis environment and
retain the generated session-information file with the release used for peer
review or publication.

## Software dependencies

R packages used by the workflow are `dplyr`, `ggplot2`, `ggrepel`, `maps`,
`patchwork`, `readxl`, `scales`, `tibble`, and `tidyr`. Exact versions are
reported by `R/99_session_info.R` rather than being inferred from another
computer.

## Reuse

No open-source licence is granted at this stage. See `NOTICE.md`. Repository
visibility and access permissions are administrative controls and do not
replace copyright protection.
