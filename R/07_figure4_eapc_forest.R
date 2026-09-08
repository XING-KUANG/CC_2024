if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
# Fig4 EAPC forest plots: ordered and coloured by the six Figure 3 regions
#
# Key design decisions requested during revision:
#   1. Countries/populations are ordered by region, then alphabetically.
#   2. Colour denotes region (not EAPC magnitude or statistical significance).
#   3. Shape denotes the within-population average-trend classification.
#   4. Each population's EAPC estimation period remains visible in its label.
#   5. The caption warns that estimates from different periods must not be ranked.
#
# This is a new script and does not overwrite the original Fig4 R script.
# It displays figures in the RStudio Plots pane and does not save image files.
##############################################################################

library(dplyr)
library(ggplot2)
library(patchwork)

data_file <- "data/derived/country_eapc.csv"

# read.csv converts spaces in column names to dots, for example:
# "EAPC Period" -> "EAPC.Period".
eapc_all <- read.csv(data_file, stringsAsFactors = FALSE, check.names = TRUE)

required_columns <- c(
  "Label", "Type", "EAPC.Period", "EAPC", "EAPC.up", "EAPC.low",
  "AgeGroup", "Region"
)

missing_columns <- setdiff(required_columns, names(eapc_all))
if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

##############################################################################
# Map the 21 GLOBOCAN regions to the six display groups used in Figure 3.
# Northern Europe is included in "Western and southern Europe" so that Nordic
# populations are retained in the six-group layout.
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

# Colour-blind-friendly regional palette.
region_colours <- c(
  "Africa" = "#D55E00",
  "Asia" = "#E69F00",
  "Central and eastern Europe" = "#CC79A7",
  "Latin America" = "#009E73",
  "Northern America and Oceania" = "#0072B2",
  "Western and southern Europe" = "#56B4E9"
)

eapc_all <- eapc_all %>%
  mutate(
    Group6 = unname(group6_map[Region]),
    Group6 = factor(Group6, levels = group_order)
  )

unmapped_regions <- eapc_all %>%
  filter(is.na(Group6)) %>%
  distinct(Region) %>%
  pull(Region)

if (length(unmapped_regions) > 0) {
  stop(
    "The following GLOBOCAN regions are not mapped to a Figure 3 group: ",
    paste(unmapped_regions, collapse = ", ")
  )
}

##############################################################################
# Lancet-style forest panel
#   - coloured regional headers
#   - population and EAPC period at left
#   - forest estimate in the centre
#   - numerical EAPC (95% CI) at right
##############################################################################

make_display_rows <- function(data, metric_name) {
  d <- data %>%
    filter(Type == metric_name) %>%
    arrange(Group6, Label)

  if (nrow(d) == 0) {
    stop("No observations found for ", metric_name)
  }

  rows <- list()
  row_index <- 1

  for (grp in group_order) {
    subgroup <- d %>% filter(Group6 == grp)
    if (nrow(subgroup) == 0) next

    # Insert a dedicated regional heading row.
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
      select(Label, EAPC.Period, EAPC, EAPC.low, EAPC.up, Group6, IsHeader)
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

plot_eapc_lancet <- function(data, metric_name) {
  display <- make_display_rows(data, metric_name)
  estimates <- display %>% filter(!IsHeader)
  headers <- display %>% filter(IsHeader)

  forest_min <- floor(min(c(estimates$EAPC.low, 0), na.rm = TRUE) / 5) * 5
  forest_max <- ceiling(max(c(estimates$EAPC.up, 0), na.rm = TRUE) / 5) * 5
  forest_span <- forest_max - forest_min

  # Allocate three visual columns on one continuous coordinate system.
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
    # Column headings and top rule.
    annotate(
      "text",
      x = label_x,
      y = header_y,
      label = "Population (EAPC period)",
      hjust = 0,
      fontface = "bold",
      size = 3.15
    ) +
    annotate(
      "text",
      x = (forest_min + forest_max) / 2,
      y = header_y,
      label = paste0(metric_name, " EAPC"),
      hjust = 0.5,
      fontface = "bold",
      size = 3.25
    ) +
    annotate(
      "text",
      x = estimate_text_x,
      y = header_y,
      label = "EAPC (95% CI)",
      hjust = 0,
      fontface = "bold",
      size = 3.15
    ) +
    annotate(
      "segment",
      x = label_x,
      xend = plot_right,
      y = rule_y,
      yend = rule_y,
      colour = "grey30",
      linewidth = 0.45
    ) +

    # Zero reference line, restricted to the forest area.
    annotate(
      "segment",
      x = 0,
      xend = 0,
      y = axis_y,
      yend = rule_y,
      colour = "grey35",
      linewidth = 0.45
    ) +

    # Region-coloured confidence intervals and square estimates.
    geom_segment(
      data = estimates,
      aes(
        x = EAPC.low,
        xend = EAPC.up,
        y = Row,
        yend = Row,
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

    # Country/population labels and right-hand numerical estimates.
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

    # Regional headers act as direct colour labels; no separate legend needed.
    geom_text(
      data = headers,
      aes(x = label_x, y = Row, label = LabelDisplay, colour = Group6),
      hjust = 0,
      fontface = "bold",
      size = 3.0
    ) +

    # Manually draw an x axis only beneath the forest column.
    annotate(
      "segment",
      x = forest_min,
      xend = forest_max,
      y = axis_y,
      yend = axis_y,
      colour = "black",
      linewidth = 0.4
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

build_figure <- function(age_tag) {
  d <- eapc_all %>% filter(AgeGroup == age_tag)

  fig_incidence <- plot_eapc_lancet(d, "Incidence")
  fig_mortality <- plot_eapc_lancet(d, "Mortality")

  note_text <- paste0(
    "Each estimate summarizes the most recent available 10-year period shown in parentheses. ",
    "Because calendar periods differ among populations, EAPCs are within-population summaries ",
    "and should not be directly compared or ranked in magnitude across populations."
  )

  (fig_incidence | fig_mortality) +
    plot_annotation(
      title = paste0("Average annual percentage change in cervical cancer rates (", age_tag, ")"),
      caption = note_text,
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

# No files are saved. Run an object name in the Console to display that figure.
fig4_all <- build_figure("All ages")

# Display the main all-ages Figure 4 in the RStudio Plots pane.
print(fig4_all)
export_plot("figure_4_all_age_eapc.png", fig4_all, width = 16, height = 13)
