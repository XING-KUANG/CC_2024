if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

###############################################################################
# Supplementary figure: GLOBOCAN 2024 data-source / estimation-method classes
# Cervical cancer incidence and mortality estimates, 186 countries
#
# This script displays the figure in the R plotting window only.
# It does not save PNG/PDF/TIFF files automatically.
###############################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)

annex_file <- "data/raw/globocan_2024_annex_a.xlsx"

to_ascii <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  y
}

# The five classes preserve the official Annex A estimation-method hierarchy.
# They describe the basis of the estimates and are not official IARC ratings of
# high, medium, or low data quality.
classify_method <- function(method) {
  method <- trimws(as.character(method))
  case_when(
    method %in% c("1", "2a")  ~ "National observed rates",
    method == "2b"             ~ "Subnational observed rates",
    method == "3a"             ~ "Model-based: country-specific M:I",
    method == "3c"             ~ "Model-based: survival-based M:I",
    method == "4"              ~ "Rates scaled to regional totals",
    method %in% c("3b", "5")  ~ "Neighbouring-country/no direct data",
    TRUE                        ~ NA_character_
  )
}

method_levels <- c(
  "National observed rates",
  "Subnational observed rates",
  "Model-based: country-specific M:I",
  "Model-based: survival-based M:I",
  "Rates scaled to regional totals",
  "Neighbouring-country/no direct data"
)

method_colours <- c(
  "National observed rates"                  = "#3B8F70",
  "Subnational observed rates"               = "#79AAC3",
  "Model-based: country-specific M:I"         = "#8C6BB1",
  "Model-based: survival-based M:I"           = "#D8B365",
  "Rates scaled to regional totals"           = "#E07A5F",
  "Neighbouring-country/no direct data"       = "#A94F4F"
)

read_annex_sheet <- function(sheet_name) {
  x <- read_excel(annex_file, sheet = sheet_name)
  names(x) <- trimws(names(x))

  country_col <- if (sheet_name == "Incidence") "Population" else "Region / Country"

  x %>%
    transmute(
      Country = .data[[country_col]],
      Primary_source = `Primary data source`,
      Method = trimws(as.character(Method)),
      Description = trimws(as.character(Description)),
      Estimate_basis = classify_method(Method)
    ) %>%
    # Region-heading rows have no primary source or method and are not countries.
    filter(!is.na(Primary_source), !is.na(Method), !is.na(Estimate_basis)) %>%
    mutate(
      Country = to_ascii(Country),
      Estimate_basis = factor(Estimate_basis, levels = method_levels)
    )
}

incidence_method <- read_annex_sheet("Incidence")
mortality_method <- read_annex_sheet("Mortality")

# GLOBOCAN country names -> ggplot2::map_data("world") names.
rename_tbl <- c(
  "United States of America"               = "USA",
  "Russian Federation"                     = "Russia",
  "United Kingdom"                         = "UK",
  "Congo, Republic of"                     = "Republic of Congo",
  "Congo, Democratic Republic of"          = "Democratic Republic of the Congo",
  "Iran, Islamic Republic of"              = "Iran",
  "Korea, Democratic People's Republic of" = "North Korea",
  "Korea, Republic of"                     = "South Korea",
  "Tanzania, United Republic of"           = "Tanzania",
  "Czechia"                                = "Czech Republic",
  "Republic of Moldova"                    = "Moldova",
  "Viet Nam"                               = "Vietnam",
  "Lao People's Democratic Republic"       = "Laos",
  "Syrian Arab Republic"                   = "Syria",
  "Brunei Darussalam"                      = "Brunei",
  "Turkiye"                                = "Turkey",
  "Bosnia Herzegovina"                     = "Bosnia and Herzegovina",
  "Eswatini"                               = "Swaziland",
  "France (metropolitan)"                  = "France",
  "The Netherlands"                        = "Netherlands",
  "Cote d'Ivoire"                          = "Ivory Coast",
  "The Republic of the Gambia"             = "Gambia",
  "Curacao"                                = "Curacao",
  "Bolivia (Plurinational State of)"       = "Bolivia",
  "French Guyana"                          = "French Guiana",
  "France, Guadeloupe"                     = "Guadeloupe",
  "France, Martinique"                     = "Martinique",
  "France, La Reunion"                     = "Reunion",
  "Gaza Strip and West Bank"               = "Palestine"
)

rename_countries <- function(dat) {
  for (old in names(rename_tbl)) {
    dat$Country[dat$Country == old] <- rename_tbl[[old]]
  }

  # map_data("world") represents Trinidad and Tobago as two polygons.
  tt <- dat[dat$Country == "Trinidad and Tobago", , drop = FALSE]
  if (nrow(tt) == 1L) {
    dat$Country[dat$Country == "Trinidad and Tobago"] <- "Trinidad"
    tt$Country <- "Tobago"
    dat <- bind_rows(dat, tt)
  }
  dat
}

incidence_method <- rename_countries(incidence_method)
mortality_method <- rename_countries(mortality_method)
world_map <- map_data("world")

join_world <- function(method_data) {
  world_map %>%
    left_join(method_data, by = c("region" = "Country"))
}

incidence_map <- join_world(incidence_method)
mortality_map <- join_world(mortality_method)

make_method_map <- function(dat, panel_title, panel_tag) {
  ggplot(dat, aes(long, lat, group = group, fill = Estimate_basis)) +
    geom_polygon(colour = "white", linewidth = 0.16) +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
    scale_fill_manual(
      values = method_colours,
      limits = method_levels,
      drop = FALSE,
      na.value = "#D9D9D9",
      name = "Basis of estimate"
    ) +
    labs(title = panel_title, tag = panel_tag) +
    theme_void(base_family = "Arial", base_size = 9) +
    theme(
      plot.title = element_text(size = 10.5, face = "bold", hjust = 0),
      plot.tag = element_text(size = 15, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.width = grid::unit(10, "mm"),
      legend.key.height = grid::unit(3.5, "mm"),
      plot.margin = margin(5, 5, 2, 5)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

fig_incidence_method <- make_method_map(
  incidence_map,
  "Incidence estimates",
  "A"
)

fig_mortality_method <- make_method_map(
  mortality_map,
  "Mortality estimates",
  "B"
)

fig_data_quality <-
  (fig_incidence_method / fig_mortality_method) +
  plot_annotation(
    title = "Data sources and estimation methods underlying GLOBOCAN 2024 cervical cancer estimates",
    caption = paste0(
      "Categories reproduce the methodological basis reported in GLOBOCAN 2024 Annex A; ",
      "they are not official IARC quality ratings. Grey indicates countries or territories ",
      "not represented in the ggplot2 world map or without a matched Annex A record."
    ),
    theme = theme(
      plot.title = element_text(family = "Arial", size = 13, face = "bold", hjust = 0.5),
      plot.caption = element_text(family = "Arial", size = 7.5, colour = "#444444", hjust = 0)
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Quality-control summaries shown in the console.
cat("\nIncidence estimation classes (countries):\n")
print(count(incidence_method, Estimate_basis, .drop = FALSE))

cat("\nMortality estimation classes (countries):\n")
print(count(mortality_method, Estimate_basis, .drop = FALSE))

cat("\nAnnex A countries not matched to map_data('world') polygons:\n")
unmatched_inc <- setdiff(unique(incidence_method$Country), unique(world_map$region))
unmatched_mort <- setdiff(unique(mortality_method$Country), unique(world_map$region))
print(sort(unique(c(unmatched_inc, unmatched_mort))))

# Display only; the user can export manually from the RStudio Plots pane.
print(fig_data_quality)
export_plot("figure_s4_estimation_methods.png", fig_data_quality, width = 14, height = 10)
