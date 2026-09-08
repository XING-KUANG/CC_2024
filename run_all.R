source(file.path("R", "00_config.R"), encoding = "UTF-8")

analysis_scripts <- c(
  "01_prepare_data.R",
  "12_table1_regional_burden.R",
  "13_table_s1_country_burden.R",
  "02_figure1_global_maps.R",
  "03_figure2a_age_group_rates.R",
  "04_figure2b_age_group_counts.R",
  "05_figure2cd_age_specific_development_groups.R",
  "06_figure3_temporal_trajectories.R",
  "07_figure4_eapc_forest.R",
  "08_figure5ab_global_scenarios.R",
  "09_figure5cd_regional_projection.R",
  "10_figures_s2_s3_age_specific_eapc.R",
  "11_figure_s4_estimation_methods.R"
)

for (script in analysis_scripts) {
  message("Running ", script)
  source(file.path("R", script), encoding = "UTF-8", local = new.env(parent = globalenv()))
}

source(file.path("R", "99_session_info.R"), encoding = "UTF-8")
