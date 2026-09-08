if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
##############################################################################

library(ggplot2)
library(dplyr)
library(patchwork)

country_data <- read.csv(
  "data/derived/country_rates_2024.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

names(country_data)

##########################################################################
##########################################################################

to_ascii <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  y
}

worldData <- map_data("world")

small_map_data <- country_data
small_map_data$Label <- to_ascii(small_map_data$Label)

rename_tbl <- c(
  "United States of America"                 = "USA",
  "Russian Federation"                       = "Russia",
  "United Kingdom"                           = "UK",
  "Congo, Republic of"                       = "Republic of Congo",
  "Congo, Democratic Republic of"            = "Democratic Republic of the Congo",
  "Iran, Islamic Republic of"                = "Iran",
  "Korea, Democratic People's Republic of"   = "North Korea",
  "Korea, Republic of"                       = "South Korea",
  "Tanzania, United Republic of"             = "Tanzania",
  "Czechia"                                  = "Czech Republic",
  "Republic of Moldova"                      = "Moldova",
  "Viet Nam"                                 = "Vietnam",
  "Lao People's Democratic Republic"         = "Laos",
  "Syrian Arab Republic"                     = "Syria",
  "Brunei Darussalam"                        = "Brunei",
  "Turkiye"                                  = "Turkey",
  "Bosnia Herzegovina"                       = "Bosnia and Herzegovina",
  "Eswatini"                                 = "Swaziland",
  "France (metropolitan)"                    = "France",
  "The Netherlands"                          = "Netherlands",
  "Cote d'Ivoire"                            = "Ivory Coast",
  "The Republic of the Gambia"               = "Gambia",
  "Curacao"                                  = "Curacao",
  "Bolivia (Plurinational State of)"         = "Bolivia",
  "French Guyana"                            = "French Guiana",
  "France, Guadeloupe"                       = "Guadeloupe",
  "France, Martinique"                       = "Martinique",
  "France, La Reunion"                       = "Reunion",
  "Gaza Strip and West Bank"                 = "Palestine"
)

for (old in names(rename_tbl)) {
  small_map_data$Label[small_map_data$Label == old] <- rename_tbl[[old]]
}

a <- small_map_data[small_map_data$Label == "Trinidad and Tobago", ]
if (nrow(a) == 1) {
  small_map_data$Label[small_map_data$Label == "Trinidad and Tobago"] <- "Trinidad"
  a$Label <- "Tobago"
  small_map_data <- rbind(small_map_data, a)
}

names(small_map_data)[names(small_map_data) == "Label"] <- "location"

small_map_data_asir <- small_map_data %>%
  full_join(worldData, ., by = c("region" = "location")) %>%
  filter(!is.na(ASIR) | region == "Greenland")

small_map_data_asmr <- small_map_data %>%
  full_join(worldData, ., by = c("region" = "location")) %>%
  filter(!is.na(ASMR) | region == "Greenland")

##########################################################################
##########################################################################

make_world_map <- function(dat, value_col, breaks, labels, colors, legend_title, tag = NULL) {

  dat$fill_grp <- cut(dat[[value_col]], breaks = breaks, labels = labels)

  base <- ggplot(dat) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = fill_grp),
                 colour = "black", linewidth = 0.35) +
    theme_bw() +
    scale_fill_manual(values = setNames(colors, labels), name = legend_title,
                       labels = labels, na.value = "grey70") +
    labs(tag = tag) +
    theme(
      legend.position = c(0.15, 0.23),
      legend.justification = c(0.5, 0.5),
      legend.box.just = "left",
      plot.title = element_text(color = "black", size = 10),
      plot.tag = element_text(color = "black", size = 18, face = "bold"),
      legend.title = element_text(color = "black", size = 12),
      legend.text = element_text(color = "black", size = 12),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()
    )

  inset <- function(title, xlim, ylim) {
    base + labs(x = " ", y = "", title = title, tag = NULL) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      theme(legend.position = "none")
  }

  p2 <- inset("Caribbean and Central America", c(-92, -60), c(5, 27))
  p3 <- inset("Persian Gulf",                  c(45, 57),   c(18, 31))
  p4 <- inset("Balkans and S. Europe",         c(12, 32),   c(33, 48))
  p5 <- inset("Southeast Asia",                c(95, 128),  c(-11, 10))
  p6 <- inset("West & Middle Africa",          c(-18, 30),  c(-10, 20))
  p7 <- inset("East & Southern Africa",        c(10, 52),   c(-35, 5))
  p8 <- inset("Northern Europe",               c(3, 30),    c(47, 61))

  A <- (p6 | p7)

  base + (p2 + p3 + p4 + p5 + A + p8 + plot_layout(ncol = 6, widths = c(1.3, 1, 1.1, 1.2, 2, 1))) +
    plot_layout(ncol = 1, heights = c(9, 3))
}

##########################################################################
##########################################################################

asir_colors <- c("#EAF3FB", "#BFDCF0", "#7FB8DE", "#4292C6", "#2166AC", "#0B3C6B")
asir_labels <- c("<5", "5-10", "10-15", "15-25", "25-40", ">=40")

fig_asir <- make_world_map(
  small_map_data_asir, "ASIR",
  breaks = c(0, 5, 10, 15, 25, 40, 100), labels = asir_labels,
  colors = asir_colors,
  legend_title = "Age-standardised incidence\n(per 100000 women-years)",
  tag = "A"
)

##########################################################################
##########################################################################

asmr_colors <- c("#FDF0EC", "#FBD5C6", "#F5A889", "#E4705A", "#B93A32", "#7A1B17")
asmr_labels <- c("<2", "2-4", "4-6", "6-10", "10-20", ">=20")

fig_asmr <- make_world_map(
  small_map_data_asmr, "ASMR",
  breaks = c(0, 2, 4, 6, 10, 20, 100), labels = asmr_labels,
  colors = asmr_colors,
  legend_title = "Age-standardised mortality\n(per 100000 women-years)",
  tag = "B"
)

print(fig_asir)
print(fig_asmr)
export_plot("figure_1a_global_asir.png", fig_asir, width = 14, height = 8)
export_plot("figure_1b_global_asmr.png", fig_asmr, width = 14, height = 8)

# final_fig <- fig_asir / fig_asmr
