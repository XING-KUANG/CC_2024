if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
#
##############################################################################

library(dplyr)
library(ggplot2)

hdi <- read.csv("data/derived/age_specific_development_groups.csv", stringsAsFactors = FALSE)
age_labels <- c("15-29", "30-44", "45-59", "60-74", "75-85+")
hdi$Age <- factor(hdi$Age, levels = age_labels)

groups <- c("Very HDI country", "High HDI country (but China)", "Medium HDI country (but India)",
            "Low HDI country", "China", "India", "World")
group_labels <- c("Very high HDI", "High HDI (excl. China)", "Medium HDI (excl. India)",
                   "Low HDI", "China", "India", "World")
names(group_labels) <- groups

hdi <- hdi %>% filter(Label %in% groups) %>%
  mutate(Label = factor(Label, levels = groups))

group_colors <- c(
  "Very HDI country" = "#2166AC", "High HDI country (but China)" = "#4EA5D9",
  "Medium HDI country (but India)" = "#F4A93A", "Low HDI country" = "#C0392B",
  "China" = "#7A7A7A", "India" = "#4C9A4C", "World" = "black"
)
group_linetype <- c(
  "Very HDI country" = "solid", "High HDI country (but China)" = "solid",
  "Medium HDI country (but India)" = "solid", "Low HDI country" = "solid",
  "China" = "dashed", "India" = "dashed", "World" = "solid"
)

plot_metric <- function(metric_name, y_lab, title) {
  d <- hdi %>% filter(Type == metric_name)
  ggplot(d, aes(x = Age, y = ASR, group = Label, color = Label, linetype = Label)) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "#FDE9C8", alpha = 0.5) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_color_manual(values = group_colors, labels = group_labels, name = NULL) +
    scale_linetype_manual(values = group_linetype, labels = group_labels, name = NULL) +
    labs(title = title, x = "Age group (years)", y = y_lab) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "right"
    )
}

fig_inc  <- plot_metric("Incidence", "ASIR per 100,000", "Age-specific incidence by HDI tier, 2024")
fig_mort <- plot_metric("Mortality", "ASMR per 100,000", "Age-specific mortality by HDI tier, 2024")

library(patchwork)
final_fig <- fig_inc + fig_mort + plot_layout(guides = "collect") & theme(legend.position = "right")
print(final_fig)
export_plot("figure_2cd_age_specific_development_groups.png", final_fig, width = 15, height = 6.3)
