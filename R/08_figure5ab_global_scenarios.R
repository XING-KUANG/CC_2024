if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
#
#
#    based solely on population growth and aging and may be further
#    influenced by changes in incidence and mortality rates... 7 possible
#
##############################################################################

library(dplyr)
library(readxl)
library(ggplot2)
library(scales)

raw <- read_excel("data/raw/global_projection_scenarios.xlsx")
names(raw) <- trimws(names(raw))
raw <- raw %>% rename(ScenarioPct = `% change`)

years <- sort(unique(raw$Year))

dat_all <- raw %>%
  mutate(
    x = match(Year, years),
    ScenarioLabel = sprintf("%+.0f%%/yr", ScenarioPct * 100),
    ScenarioLabel = ifelse(ScenarioPct == 0, "0%/yr (constant rate)", ScenarioLabel),
    ScenarioLabel = factor(ScenarioLabel, levels = c("-3%/yr", "-2%/yr", "-1%/yr",
                                                      "0%/yr (constant rate)",
                                                      "+1%/yr", "+2%/yr", "+3%/yr"))
  ) %>%
  arrange(ScenarioPct, x)

scenario_colors <- c(
  "-3%/yr" = "#08519C", "-2%/yr" = "#3182BD", "-1%/yr" = "#9ECAE1",
  "0%/yr (constant rate)" = "#2E5A87",
  "+1%/yr" = "#FCAE91", "+2%/yr" = "#FB6A4A", "+3%/yr" = "#B7262F"
)
scenario_linewidth <- c(
  "-3%/yr" = 1, "-2%/yr" = 1, "-1%/yr" = 1,
  "0%/yr (constant rate)" = 1.6,
  "+1%/yr" = 1, "+2%/yr" = 1, "+3%/yr" = 1
)

SUBTITLE <- paste0(
  "Projections are based on population growth and aging (baseline); ",
  "7 scenarios further assume an annual ASR change of −3% to +3%, in addition to the baseline"
)

build_panel <- function(type_val, main_title, y_lab) {

  d <- dat_all %>% filter(Type == type_val)
  base_2024 <- d$Prediction[d$Year == 2024 & d$ScenarioPct == 0]
  end_labels <- d %>% filter(Year == max(Year))

  ggplot(d, aes(x = x, y = Prediction, color = ScenarioLabel,
                linewidth = ScenarioLabel, group = ScenarioLabel)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(data = end_labels,
              aes(label = comma(Prediction, big.mark = " ")),
              hjust = -0.15, size = 3.3, fontface = "bold", show.legend = FALSE) +
    annotate("text", x = 1, y = base_2024,
             label = paste0("2024 baseline: ", comma(base_2024, big.mark = " ")),
             hjust = 0, vjust = 2.2, size = 3.2, color = "grey30") +
    scale_color_manual(values = scenario_colors, name = "Assumed annual ASR change\n(on top of population growth/aging)") +
    scale_linewidth_manual(values = scenario_linewidth, guide = "none") +
    scale_x_continuous(breaks = seq_along(years), labels = years,
                        expand = expansion(mult = c(0.03, 0.16))) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(title = main_title, subtitle = SUBTITLE, x = "Year", y = y_lab) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
      axis.line.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.x = element_line(color = "black", linewidth = 0.4),
      axis.ticks.y = element_blank(),
      legend.position = c(0.02, 0.90),
      legend.justification = c(0, 1),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0)
    )
}

p_inc  <- build_panel(0, "Global cervical cancer burden — new cases, 2024–2050", "Estimated number of new cases")
p_mort <- build_panel(1, "Global cervical cancer burden — deaths, 2024–2050", "Estimated number of deaths")

print(p_inc)
print(p_mort)

export_plot("figure_5a_global_incidence_scenarios.png", p_inc, width = 8.5, height = 6.8)
export_plot("figure_5b_global_mortality_scenarios.png", p_mort, width = 8.5, height = 6.8)
export_table(
  "figure_5a_global_incidence_scenarios.csv",
  dat_all %>% filter(Type == 0) %>%
    select(Year, ScenarioPct, ScenarioLabel, Prediction)
)
export_table(
  "figure_5b_global_mortality_scenarios.csv",
  dat_all %>% filter(Type == 1) %>%
    select(Year, ScenarioPct, ScenarioLabel, Prediction)
)
