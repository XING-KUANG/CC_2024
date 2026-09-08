if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
# Figure 3: all eligible populations, direct endpoint labels, four columns
#
# A: incidence; B: mortality.
# Eight geographic groups are arranged in a 4-column x 2-row layout.
# Northern, Western, and Southern Europe are displayed separately.
# Points are annual ASRs; LOESS curves (span 0.30) are descriptive only.
# No image file is saved automatically.
##############################################################################

library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

data_file <- "data/derived/country_temporal_trends.csv"
if (!file.exists(data_file)) stop("File not found: ", data_file)

trend <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
) %>%
  filter(AgeGroup == "All ages", Year >= 1988) %>%
  mutate(Year = as.numeric(Year), ASR = as.numeric(ASR)) %>%
  filter(is.finite(Year), is.finite(ASR))

##############################################################################
# Geographic grouping
##############################################################################

group8_map <- c(
  "Eastern Africa" = "Africa",
  "Middle Africa" = "Africa",
  "Northern Africa" = "Africa",
  "Southern Africa" = "Africa",
  "Western Africa" = "Africa",
  "Eastern Asia" = "Asia",
  "South-Eastern Asia" = "Asia",
  "South Central Asia" = "Asia",
  "Western Asia" = "Asia",
  "Eastern Europe" = "Central and eastern Europe",
  "Caribbean" = "Latin America",
  "Central America" = "Latin America",
  "South America" = "Latin America",
  "Northern America" = "Northern America and Oceania",
  "Australia-New Zealand" = "Northern America and Oceania",
  "Melanesia" = "Northern America and Oceania",
  "Micronesia" = "Northern America and Oceania",
  "Polynesia" = "Northern America and Oceania",
  "Northern Europe" = "Northern Europe",
  "Western Europe" = "Western Europe",
  "Southern Europe" = "Southern Europe"
)

# This order determines the left-to-right, top-to-bottom four-column layout.
group_order <- c(
  "Africa",
  "Asia",
  "Central and eastern Europe",
  "Latin America",
  "Northern America and Oceania",
  "Northern Europe",
  "Western Europe",
  "Southern Europe"
)

trend <- trend %>%
  mutate(
    DisplayGroup = unname(group8_map[Region]),
    DisplayGroup = factor(DisplayGroup, levels = group_order)
  )

unmapped_regions <- trend %>%
  filter(is.na(DisplayGroup)) %>%
  distinct(Region) %>%
  pull(Region)

if (length(unmapped_regions) > 0) {
  stop("Unmapped regions: ", paste(unmapped_regions, collapse = ", "))
}

##############################################################################
# Short display names only; no population is removed.
##############################################################################

label_short_map <- c(
  "United States of America" = "USA",
  "United Kingdom" = "UK",
  "Russian Federation" = "Russia",
  "Iran, Islamic Republic of" = "Iran",
  "Korea, Republic of" = "South Korea",
  "Korea, Democratic People's Republic of" = "North Korea",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Congo, Democratic Republic of" = "DR Congo",
  "Congo, Republic of" = "Congo",
  "Tanzania, United Republic of" = "Tanzania",
  "Lao People's Democratic Republic" = "Laos",
  "Syrian Arab Republic" = "Syria",
  "United Arab Emirates" = "UAE",
  "Central African Republic" = "CAR",
  "Dominican Republic" = "Dominican Rep.",
  "Trinidad and Tobago" = "Trinidad & Tobago",
  "Papua New Guinea" = "PNG",
  "Sao Tome and Principe" = "Sao Tome",
  "The Republic of the Gambia" = "Gambia",
  "Bosnia Herzegovina" = "Bosnia",
  "France (metropolitan)" = "France",
  "Viet Nam" = "Vietnam",
  "The Netherlands" = "Netherlands"
)

trend <- trend %>%
  mutate(
    DisplayLabel = ifelse(
      Label %in% names(label_short_map),
      unname(label_short_map[Label]),
      Label
    )
  )

duplicate_rows <- trend %>%
  count(Type, Label, Year) %>%
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  print(duplicate_rows)
  stop("Duplicate annual population-level observations were detected.")
}

cat("All eligible populations displayed in Figure 3:\n")
print(
  trend %>%
    distinct(Type, DisplayGroup, Label) %>%
    count(Type, DisplayGroup, name = "Populations")
)

##############################################################################
# Fixed low-saturation journal palette
##############################################################################

journal_palette <- c(
  "#4477AA", "#228833", "#EE6677", "#66CCEE", "#AA3377", "#CCBB44",
  "#332288", "#44AA99", "#CC6677", "#999933", "#6699CC", "#EE8866",
  "#117733", "#88CCEE", "#882255", "#DDCC77", "#AA4466", "#77AADD",
  "#44BB99", "#BBCC33", "#EE99AA", "#A6761D", "#7570B3", "#666666"
)

make_country_palette <- function(labels) {
  labels <- sort(unique(labels))
  if (length(labels) > length(journal_palette)) {
    stop("A regional panel contains more populations than the palette supports.")
  }
  stats::setNames(journal_palette[seq_along(labels)], labels)
}

##############################################################################
# Regional plotting function with labels at the final annual observation
##############################################################################

plot_one_group_direct <- function(dat, grp, y_lab) {
  d <- dat %>%
    filter(DisplayGroup == grp) %>%
    mutate(
      DisplayLabel = factor(DisplayLabel, levels = sort(unique(DisplayLabel)))
    ) %>%
    droplevels()

  if (nrow(d) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No eligible data", size = 3) +
        labs(title = grp) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  }

  n_population <- nlevels(d$DisplayLabel)
  palette <- make_country_palette(levels(d$DisplayLabel))

  endpoints <- d %>%
    group_by(DisplayLabel) %>%
    slice_max(order_by = Year, n = 1, with_ties = FALSE) %>%
    ungroup()

  min_year <- min(d$Year, na.rm = TRUE)
  max_year <- max(d$Year, na.rm = TRUE)
  year_span <- max_year - min_year
  label_nudge <- max(1.2, 0.035 * year_span)
  label_space <- max(8, 0.26 * year_span)

  year_breaks <- sort(unique(c(
    seq(5 * ceiling(min_year / 5), 5 * floor(max_year / 5), by = 5),
    min_year,
    max_year
  )))

  if (length(year_breaks) >= 2 && year_breaks[2] - year_breaks[1] < 3) {
    year_breaks <- year_breaks[-1]
  }
  if (length(year_breaks) >= 2 &&
      year_breaks[length(year_breaks)] - year_breaks[length(year_breaks) - 1] < 3) {
    year_breaks <- year_breaks[-length(year_breaks)]
  }

  # Four columns make each regional panel narrower, so dense panels use a
  # slightly smaller endpoint-label size.
  direct_label_size <- ifelse(
    n_population > 14,
    1.85,
    ifelse(n_population > 8, 2.05, 2.25)
  )

  ggplot(
    d,
    aes(x = Year, y = ASR, colour = DisplayLabel, group = DisplayLabel)
  ) +
    geom_point(
      size = 0.34,
      alpha = 0.40,
      shape = 16,
      na.rm = TRUE
    ) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      span = 0.30,
      se = FALSE,
      linewidth = 0.72,
      alpha = 0.88,
      na.rm = TRUE
    ) +
    geom_text_repel(
      data = endpoints,
      aes(label = DisplayLabel),
      direction = "y",
      hjust = 0,
      nudge_x = label_nudge,
      seed = 2026,
      size = direct_label_size,
      box.padding = 0.10,
      point.padding = 0.05,
      min.segment.length = 0,
      segment.size = 0.22,
      segment.alpha = 0.65,
      force = 1.8,
      force_pull = 0.12,
      max.overlaps = Inf,
      max.time = 2,
      max.iter = 20000,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = palette, guide = "none") +
    scale_x_continuous(
      breaks = year_breaks,
      expand = expansion(mult = c(0.02, 0))
    ) +
    coord_cartesian(
      xlim = c(min_year, max_year + label_space),
      clip = "on"
    ) +
    labs(
      title = grp,
      subtitle = paste0("n = ", n_population, " populations"),
      x = "Year",
      y = y_lab
    ) +
    theme_minimal(base_size = 9.5, base_family = "Arial") +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      axis.line = element_line(colour = "black", linewidth = 0.6),
      axis.ticks = element_line(colour = "black", linewidth = 0.6),
      plot.title = element_text(size = 10.2, hjust = 0.6, face = "bold"),
      plot.subtitle = element_text(size = 7.2, hjust = 0.6, colour = "grey35"),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(5, 4, 5, 4)
    )
}

##############################################################################
# Build incidence and mortality figures in a four-column layout
##############################################################################

build_metric_figure_direct <- function(metric_type, y_lab, panel_tag) {
  dat <- trend %>% filter(Type == metric_type)

  regional_plots <- lapply(
    group_order,
    function(grp) plot_one_group_direct(dat, grp, y_lab)
  )

  note_text <- paste0(
    "All populations meeting the outcome-specific eligibility criteria are shown. ",
    "Points indicate annual age-standardised rates; lines are descriptive LOESS ",
    "smooths (span = 0.30). Labels identify populations at their last available ",
    "annual observation."
  )

  wrap_plots(regional_plots, ncol = 4, nrow = 2) +
    plot_annotation(
      title = paste0(panel_tag, "  ", metric_type),
      caption = note_text,
      theme = theme(
        plot.title = element_text(
          face = "bold",
          size = 15,
          hjust = 0,
          margin = margin(b = 7)
        ),
        plot.caption = element_text(
          size = 8.3,
          hjust = 0,
          margin = margin(t = 7)
        )
      )
    )
}

fig3A_direct_europe3_4col <- build_metric_figure_direct(
  "Incidence",
  "ASIR (per 100,000 women-years)",
  "A"
)

fig3B_direct_europe3_4col <- build_metric_figure_direct(
  "Mortality",
  "ASMR (per 100,000 women-years)",
  "B"
)

print(fig3A_direct_europe3_4col)
print(fig3B_direct_europe3_4col)
export_plot("figure_3a_incidence_trajectories.png", fig3A_direct_europe3_4col, width = 16, height = 12)
export_plot("figure_3b_mortality_trajectories.png", fig3B_direct_europe3_4col, width = 16, height = 12)

