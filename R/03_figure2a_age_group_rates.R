if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
# Figure 2A: cervical cancer ASIR/ASMR by age group, 2024
#
# Rates are compared between women aged 15-49 years and women aged 50 years
# or older. The plot is displayed by default and exported only when requested.
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
  stop(
    paste0(
      "Missing official GLOBOCAN 2024 export for ages 15-49:\n",
      file_15_49,
      "\nDownload the World + 21 regions file with Cervix uteri, females, ",
      "incidence and mortality, and ages 15-49."
    )
  )
}

if (!file.exists(file_50_plus)) {
  stop("Missing official GLOBOCAN 2024 export for ages 50 years or older: ", file_50_plus)
}

read_world_age_group <- function(path, age_label) {
  d <- read_excel(path)

  required_columns <- c("Label", "Type", "ASR (World)", "Total")
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
      Age = age_label,
      Metric = if_else(Type == 0, "Incidence", "Mortality"),
      ASR = as.numeric(`ASR (World)`),
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

world_age <- bind_rows(
  read_world_age_group(file_15_49, "15-49 years"),
  read_world_age_group(file_50_plus, "50 years or older")
) %>%
  mutate(
    Age = factor(Age, levels = c("15-49 years", "50 years or older")),
    Metric = factor(Metric, levels = c("Incidence", "Mortality")),
    plot_value = if_else(Metric == "Incidence", ASR, -ASR)
  )

print(
  world_age %>%
    select(Age, Metric, ASR, Total) %>%
    arrange(Age, Metric)
)

metric_colors <- c(
  Incidence = "#2E5A87",
  Mortality = "#B7262F"
)

lancet_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.y = element_blank(),
      legend.position = "top",
      legend.key = element_blank(),
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = base_size, hjust = 0.5),
      plot.subtitle = element_text(size = base_size - 2, hjust = 0.5, color = "grey30")
    )
}

max_value <- max(world_age$ASR, na.rm = TRUE) * 1.25

fig2A_15_49 <- ggplot(
  world_age,
  aes(x = Age, y = plot_value, fill = Metric)
) +
  geom_col(width = 0.55, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(
    aes(
      label = sprintf("%.2f", ASR),
      hjust = if_else(Metric == "Incidence", -0.15, 1.15)
    ),
    size = 3.5,
    fontface = "bold",
    color = "grey20"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    limits = c(-max_value, max_value),
    labels = function(x) abs(x)
  ) +
  scale_fill_manual(values = metric_colors) +
  labs(
    title = "Cervical cancer incidence and mortality by age group, 2024",
    subtitle = "Incidence (right) and mortality (left), per 100,000 women",
    x = NULL,
    y = "Rate per 100,000"
  ) +
  lancet_theme(13)

print(fig2A_15_49)
export_plot("figure_2a_age_group_rates.png", fig2A_15_49, width = 9, height = 5.5)

