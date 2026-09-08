if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
# Figure 2B: share of global cervical cancer burden by age group, 2024
#
# Proportions are calculated among women aged 15 years or older. The plot is
# displayed by default and exported only when requested.
##############################################################################

library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)

project_dir <- PROJECT_ROOT

file_15_49 <- file.path(
  project_dir,
  "data/raw/regions_age_15_49.xlsx"
)

file_50_plus <- file.path(
  project_dir,
  "data/raw/regions_age_50_plus.xlsx"
)

if (!file.exists(file_15_49)) {
  stop("Missing official GLOBOCAN 2024 export for ages 15-49: ", file_15_49)
}

if (!file.exists(file_50_plus)) {
  stop("Missing official GLOBOCAN 2024 export for ages 50 years or older: ", file_50_plus)
}

read_world_totals <- function(path, age_label) {
  d <- read_excel(path)

  required_columns <- c("Label", "Type", "Total")
  missing_columns <- setdiff(required_columns, names(d))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in ", basename(path), ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  world <- d %>%
    filter(Label == "World", Type %in% c(0, 1)) %>%
    transmute(
      Metric = if_else(Type == 0, "Incidence", "Mortality"),
      Age = age_label,
      Total = as.numeric(Total)
    )

  if (nrow(world) != 2 || !all(c("Incidence", "Mortality") %in% world$Metric)) {
    stop(
      "The file ", basename(path),
      " does not contain one World incidence row and one World mortality row."
    )
  }

  world
}

burden_by_age <- bind_rows(
  read_world_totals(file_15_49, "15-49 years"),
  read_world_totals(file_50_plus, "50 years or older")
) %>%
  group_by(Metric) %>%
  mutate(
    Denominator_15_plus = sum(Total),
    Proportion = Total / Denominator_15_plus * 100
  ) %>%
  ungroup() %>%
  mutate(
    Metric = factor(Metric, levels = c("Mortality", "Incidence")),
    Age = factor(Age, levels = c("50 years or older", "15-49 years"))
  )

## Print the exact counts, denominators, and percentages for verification.
print(
  burden_by_age %>%
    select(Metric, Age, Total, Denominator_15_plus, Proportion) %>%
    arrange(Metric, Age)
)

age_colors <- c(
  "15-49 years" = "#B7262F",
  "50 years or older" = "#C7C7C7"
)

lancet_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.y = element_blank(),
      legend.position = "top",
      legend.key = element_blank(),
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = base_size, hjust = 0.5)
    )
}

fig2B_15_49 <- ggplot(
  burden_by_age,
  aes(x = Metric, y = Proportion, fill = Age)
) +
  geom_col(width = 0.62, color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = paste0(round(Proportion, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = age_colors,
    breaks = c("15-49 years", "50 years or older"),
    labels = c("Women aged 15-49 years", "Women aged 50 years or older")
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = expansion(add = 0.35)) +
  coord_flip() +
  labs(
    title = "Share of global cervical cancer burden by age group",
    x = NULL,
    y = "Proportion among women aged 15 years or older (%)"
  ) +
  lancet_theme(12) +
  theme(
    axis.text.y = element_text(size = 11),
    plot.title = element_text(size = 11.5)
  )

print(fig2B_15_49)
export_plot("figure_2b_age_group_counts.png", fig2B_15_49, width = 8, height = 4.8)

