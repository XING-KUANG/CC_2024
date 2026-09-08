if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
# Figure S2 and Figure S3: age-stratified EAPC forest plots
#
# Figure S2: women aged 15-49 years
# Figure S3: women aged 50 years or older
#
# Style is aligned with the revised Figure 4:
#   1. populations are grouped by the six Figure 3 regions;
#   2. populations are ordered alphabetically within each region;
#   3. colour denotes geographic region;
#   4. each population-specific EAPC period is shown beside its name;
#   5. incidence and mortality are displayed side by side;
#   6. no image file is saved automatically.
##############################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

project_dir <- PROJECT_ROOT

file_15_49 <- file.path(
  project_dir,
  "data/raw/eapc_age_15_49.xlsx"
)

file_50_plus <- file.path(
  project_dir,
  "data/raw/eapc_age_50_plus.xlsx"
)

# The project-level country-region crosswalk is already retained in the
# previously prepared EAPC table. Only its Label and Region columns are used;
# none of its older EAPC values enter Figure S2 or Figure S3.
crosswalk_file <- file.path(
  project_dir,
  "data",
  "reference",
  "country_region_crosswalk.csv"
)

for (f in c(file_15_49, file_50_plus, crosswalk_file)) {
  if (!file.exists(f)) stop("File not found: ", f)
}

##############################################################################
# Data preparation
##############################################################################

crosswalk <- read.csv(
  crosswalk_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

required_crosswalk_columns <- c("Label", "Region")
missing_crosswalk_columns <- setdiff(required_crosswalk_columns, names(crosswalk))
if (length(missing_crosswalk_columns) > 0) {
  stop(
    "The regional mapping table is missing: ",
    paste(missing_crosswalk_columns, collapse = ", ")
  )
}

# Remove geographically or population-stratified series that would otherwise
# represent the same country more than once.
exclude_labels <- c(
  "UK, England",
  "UK, England and wales",
  "UK, Northern Ireland",
  "UK, Scotland",
  "UK, Wales",
  "USA: Black",
  "USA: White"
)

alias_map <- c(
  "USA" = "United States of America",
  "Moldova" = "Republic of Moldova"
)

read_age_eapc <- function(path, age_group) {
  raw <- read_excel(path, sheet = "Dataset")

  required_columns <- c(
    "Population id", "Sex", "Type", "EAPC Period",
    "EAPC", "EAPC up", "EAPC low"
  )

  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0) {
    stop(
      basename(path), " is missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  raw %>%
    filter(Sex == 2) %>%
    distinct(
      `Population id`, Type, `EAPC Period`,
      EAPC, `EAPC up`, `EAPC low`
    ) %>%
    transmute(
      Label = ifelse(
        `Population id` %in% names(alias_map),
        unname(alias_map[`Population id`]),
        `Population id`
      ),
      Type = ifelse(Type == 0, "Incidence", "Mortality"),
      EAPC.Period = `EAPC Period`,
      EAPC = as.numeric(EAPC),
      EAPC.up = as.numeric(`EAPC up`),
      EAPC.low = as.numeric(`EAPC low`),
      AgeGroup = age_group
    ) %>%
    filter(!Label %in% exclude_labels) %>%
    inner_join(
      crosswalk %>% distinct(Label, Region),
      by = "Label"
    )
}

eapc_age <- bind_rows(
  read_age_eapc(file_15_49, "15-49 years"),
  read_age_eapc(file_50_plus, "50 years or older")
)

##############################################################################
# Map the 21 GLOBOCAN regions to the six display groups used in Figure 3.
##############################################################################

group6_map <- c(
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

  "Northern Europe" = "Western and southern Europe",
  "Western Europe" = "Western and southern Europe",
  "Southern Europe" = "Western and southern Europe"
)

group_order <- c(
  "Africa",
  "Asia",
  "Central and eastern Europe",
  "Latin America",
  "Northern America and Oceania",
  "Western and southern Europe"
)

# Same colour-blind-friendly six-region palette as the revised Figure 4.
region_colours <- c(
  "Africa" = "#D55E00",
  "Asia" = "#E69F00",
  "Central and eastern Europe" = "#CC79A7",
  "Latin America" = "#009E73",
  "Northern America and Oceania" = "#0072B2",
  "Western and southern Europe" = "#56B4E9"
)

eapc_age <- eapc_age %>%
  mutate(
    Group6 = unname(group6_map[Region]),
    Group6 = factor(Group6, levels = group_order)
  )

unmapped_regions <- eapc_age %>%
  filter(is.na(Group6)) %>%
  distinct(Region) %>%
  pull(Region)

if (length(unmapped_regions) > 0) {
  stop(
    "The following regions are not mapped: ",
    paste(unmapped_regions, collapse = ", ")
  )
}

# Confirm that each population contributes only one estimate per age group and
# outcome after the annual rows have been collapsed.
duplicate_estimates <- eapc_age %>%
  count(AgeGroup, Type, Label) %>%
  filter(n != 1)

if (nrow(duplicate_estimates) > 0) {
  print(duplicate_estimates)
  stop("Duplicate population-level EAPC estimates remain after preparation.")
}

cat("Numbers of population-specific estimates used:\n")
print(eapc_age %>% count(AgeGroup, Type))

##############################################################################
# Lancet-style forest-panel functions
##############################################################################

make_display_rows <- function(data, metric_name) {
  d <- data %>%
    filter(Type == metric_name) %>%
    arrange(Group6, Label)

  if (nrow(d) == 0) stop("No observations found for ", metric_name)

  rows <- list()
  row_index <- 1

  for (grp in group_order) {
    subgroup <- d %>% filter(Group6 == grp)
    if (nrow(subgroup) == 0) next

    rows[[row_index]] <- data.frame(
      Label = grp,
      EAPC.Period = "",
      EAPC = NA_real_,
      EAPC.low = NA_real_,
      EAPC.up = NA_real_,
      Group6 = factor(grp, levels = group_order),
      IsHeader = TRUE,
      stringsAsFactors = FALSE
    )
    row_index <- row_index + 1

    subgroup$IsHeader <- FALSE
    rows[[row_index]] <- subgroup %>%
      select(
        Label, EAPC.Period, EAPC, EAPC.low,
        EAPC.up, Group6, IsHeader
      )
    row_index <- row_index + 1
  }

  bind_rows(rows) %>%
    mutate(
      Row = rev(seq_len(n())),
      LabelDisplay = ifelse(
        IsHeader,
        Label,
        paste0(Label, "  (", EAPC.Period, ")")
      ),
      EstimateDisplay = ifelse(
        IsHeader,
        "",
        sprintf("%.2f (%.2f to %.2f)", EAPC, EAPC.low, EAPC.up)
      )
    )
}

plot_eapc_panel <- function(data, metric_name) {
  display <- make_display_rows(data, metric_name)
  estimates <- display %>% filter(!IsHeader)
  headers <- display %>% filter(IsHeader)

  forest_min <- floor(min(c(estimates$EAPC.low, 0), na.rm = TRUE) / 5) * 5
  forest_max <- ceiling(max(c(estimates$EAPC.up, 0), na.rm = TRUE) / 5) * 5
  forest_span <- forest_max - forest_min

  if (!is.finite(forest_span) || forest_span <= 0) forest_span <- 10

  label_x <- forest_min - 0.78 * forest_span
  estimate_text_x <- forest_max + 0.16 * forest_span
  plot_right <- forest_max + 0.78 * forest_span

  top_row <- max(display$Row)
  header_y <- top_row + 1.85
  rule_y <- top_row + 1.05
  axis_y <- 0.35

  axis_breaks <- pretty(c(forest_min, forest_max), n = 5)
  axis_breaks <- axis_breaks[
    axis_breaks >= forest_min & axis_breaks <= forest_max
  ]

  ggplot() +
    annotate(
      "text", x = label_x, y = header_y,
      label = "Population (EAPC period)",
      hjust = 0, fontface = "bold", size = 3.15
    ) +
    annotate(
      "text", x = (forest_min + forest_max) / 2, y = header_y,
      label = paste0(metric_name, " EAPC"),
      hjust = 0.5, fontface = "bold", size = 3.25
    ) +
    annotate(
      "text", x = estimate_text_x, y = header_y,
      label = "EAPC (95% CI)",
      hjust = 0, fontface = "bold", size = 3.15
    ) +
    annotate(
      "segment", x = label_x, xend = plot_right,
      y = rule_y, yend = rule_y,
      colour = "grey30", linewidth = 0.45
    ) +
    annotate(
      "segment", x = 0, xend = 0,
      y = axis_y, yend = rule_y,
      colour = "grey35", linewidth = 0.45
    ) +
    geom_segment(
      data = estimates,
      aes(
        x = EAPC.low, xend = EAPC.up,
        y = Row, yend = Row,
        colour = Group6
      ),
      linewidth = 0.55,
      lineend = "butt"
    ) +
    geom_point(
      data = estimates,
      aes(x = EAPC, y = Row, colour = Group6),
      shape = 15,
      size = 2.2
    ) +
    geom_text(
      data = estimates,
      aes(x = label_x, y = Row, label = LabelDisplay),
      hjust = 0,
      colour = "black",
      size = 2.65
    ) +
    geom_text(
      data = estimates,
      aes(x = estimate_text_x, y = Row, label = EstimateDisplay),
      hjust = 0,
      colour = "black",
      size = 2.55
    ) +
    geom_text(
      data = headers,
      aes(x = label_x, y = Row, label = LabelDisplay, colour = Group6),
      hjust = 0,
      fontface = "bold",
      size = 3.0
    ) +
    annotate(
      "segment", x = forest_min, xend = forest_max,
      y = axis_y, yend = axis_y,
      colour = "black", linewidth = 0.4
    ) +
    geom_segment(
      data = data.frame(x = axis_breaks),
      aes(x = x, xend = x, y = axis_y, yend = axis_y - 0.18),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.35
    ) +
    geom_text(
      data = data.frame(x = axis_breaks),
      aes(x = x, y = axis_y - 0.42, label = x),
      inherit.aes = FALSE,
      colour = "black",
      size = 2.45,
      vjust = 1
    ) +
    scale_colour_manual(values = region_colours, guide = "none") +
    coord_cartesian(
      xlim = c(label_x, plot_right),
      ylim = c(-0.35, header_y + 0.65),
      clip = "off"
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.margin = margin(8, 8, 10, 8),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

build_age_figure <- function(age_group, title_text) {
  d <- eapc_age %>% filter(AgeGroup == age_group)

  incidence_panel <- plot_eapc_panel(d, "Incidence")
  mortality_panel <- plot_eapc_panel(d, "Mortality")

  interpretation_note <- paste0(
    "Each estimate summarizes the most recent available 10-year period shown in parentheses. ",
    "EAPCs are within-population summaries; estimates based on different calendar periods ",
    "should not be directly compared or ranked in magnitude across populations."
  )

  (incidence_panel | mortality_panel) +
    plot_annotation(
      title = title_text,
      caption = interpretation_note,
      tag_levels = "A",
      theme = theme(
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          size = 14,
          margin = margin(b = 7)
        ),
        plot.caption = element_text(
          size = 8.5,
          hjust = 0,
          margin = margin(t = 8)
        ),
        plot.tag = element_text(face = "bold", size = 16)
      )
    )
}

figS2 <- build_age_figure(
  "15-49 years",
  "Average annual percentage change in cervical cancer rates among women aged 15-49 years"
)

figS3 <- build_age_figure(
  "50 years or older",
  "Average annual percentage change in cervical cancer rates among women aged 50 years or older"
)

print(figS2)
print(figS3)
export_plot("figure_s2_eapc_age_15_49.png", figS2, width = 16, height = 13)
export_plot("figure_s3_eapc_age_50_plus.png", figS3, width = 16, height = 13)
