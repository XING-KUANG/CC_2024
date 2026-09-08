if (!exists("PROJECT_ROOT")) source(file.path("R", "00_config.R"))

##############################################################################
#
#
#
##############################################################################

library(dplyr)
library(readxl)
library(ggplot2)
library(scales)

raw <- read_excel("data/raw/regional_projection_constant_risk.xlsx")
names(raw) <- trimws(names(raw))

build_dat <- function(type_code) {
  d <- raw %>%
    filter(Type == type_code, Year %in% c(2024, 2050)) %>%
    select(Population, Year, Prediction) %>%
    tidyr::pivot_wider(names_from = Year, values_from = Prediction, names_prefix = "y")
  d %>%
    mutate(
      increase = y2050 - y2024,
      pct = increase / y2024 * 100,
      grow = increase >= 0
    ) %>%
    arrange(y2050) %>%
    mutate(Region = factor(Population, levels = Population),
           y = as.numeric(Region))
}

build_legend_geoms <- function(xmax) {
  bar_h <- 0.6
  xu <- xmax * 0.013
  x0 <- xmax * 0.60

  y_inc <- 2.65
  y_dec <- 1.05

  rects <- data.frame(
    xmin = x0 + c(0, 6*xu, 0, 6*xu),
    xmax = x0 + c(6*xu, 10*xu, 6*xu, 10*xu),
    ymin = c(y_inc, y_inc, y_dec, y_dec) - bar_h/2,
    ymax = c(y_inc, y_inc, y_dec, y_dec) + bar_h/2,
    fill = c("2024 baseline", "Projected increase", "2050 baseline", "Projected decrease")
  )

  brk <- data.frame(
    xmin = x0 + c(0, 0, 0, 0),
    xmax = x0 + c(6*xu, 10*xu, 6*xu, 10*xu),
    y    = c(y_inc - bar_h/2 - 0.08, y_inc - bar_h/2 - 0.50,
             y_dec - bar_h/2 - 0.08, y_dec - bar_h/2 - 0.50),
    label = c("2024", "2050", "2050", "2024")
  )

  txt <- data.frame(
    x = x0 + c(11*xu, 11*xu),
    y = c(y_inc, y_dec),
    label = c("Blue = 2024 baseline\nRed = increase to 2050",
              "Green = decrease to 2050\n(change only)")
  )

  list(rects = rects, brk = brk, txt = txt)
}

plot_region_bar <- function(type_code, title_txt, x_lab, fname) {
  dat <- build_dat(type_code)
  xmax <- max(c(dat$y2024, dat$y2050))
  n <- nrow(dat)

  rects <- bind_rows(
    dat %>% filter(grow) %>%
      transmute(y, xmin = 0, xmax = y2024, seg = "2024 baseline"),
    dat %>% filter(grow) %>%
      transmute(y, xmin = y2024, xmax = y2050, seg = "Projected increase"),
    dat %>% filter(!grow) %>%
      transmute(y, xmin = 0, xmax = y2050, seg = "2050 baseline"),
    dat %>% filter(!grow) %>%
      transmute(y, xmin = y2050, xmax = y2024, seg = "Projected decrease")
  )

  dat <- dat %>% mutate(end_x = ifelse(grow, y2050, y2024))

  seg_colors <- c("2024 baseline" = "#12335E", "Projected increase" = "#C8102E",
                  "2050 baseline" = "#12335E", "Projected decrease" = "#2E8B47")

  end_labels <- dat %>%
    mutate(lab = ifelse(grow,
                         sprintf("%s   +%.1f%%", comma(y2050, big.mark = " "), pct),
                         sprintf("%s   %.1f%%", comma(y2050, big.mark = " "), pct)),
           lab_color = ifelse(grow, "#12335E", "#1E6B3A"))

  inside_labels <- bind_rows(
    dat %>% filter(grow, y2024 > xmax * 0.05) %>%
      transmute(y, x = y2024 / 2, lab = comma(y2024, big.mark = " ")),
    dat %>% filter(grow, increase > xmax * 0.05) %>%
      transmute(y, x = y2024 + increase / 2, lab = comma(increase, big.mark = " ")),
    dat %>% filter(!grow, y2050 > xmax * 0.05) %>%
      transmute(y, x = y2050 / 2, lab = comma(y2050, big.mark = " ")),
    dat %>% filter(!grow, abs(increase) > xmax * 0.05) %>%
      transmute(y, x = y2050 + abs(increase) / 2, lab = comma(abs(increase), big.mark = " "))
  )

  leg <- build_legend_geoms(xmax)

  p <- ggplot() +
    geom_rect(data = rects, aes(ymin = y - 0.3, ymax = y + 0.3,
                                 xmin = xmin, xmax = xmax, fill = seg)) +
    geom_text(data = inside_labels, aes(x = x, y = y, label = lab),
              color = "white", fontface = "bold", size = 3) +
    geom_text(data = end_labels, aes(x = end_x + xmax * 0.012, y = y,
                                      label = lab, color = lab_color),
              hjust = 0, fontface = "bold", size = 3.2) +
    geom_rect(data = leg$rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
    geom_segment(data = leg$brk, aes(x = xmin, xend = xmax, y = y, yend = y), linewidth = 0.4) +
    geom_segment(data = leg$brk, aes(x = xmin, xend = xmin, y = y - 0.09, yend = y + 0.09), linewidth = 0.4) +
    geom_segment(data = leg$brk, aes(x = xmax, xend = xmax, y = y - 0.09, yend = y + 0.09), linewidth = 0.4) +
    geom_text(data = leg$brk, aes(x = (xmin + xmax) / 2, y = y - 0.28, label = label), size = 2.5, vjust = 1) +
    geom_text(data = leg$txt, aes(x = x, y = y, label = label), hjust = 0, size = 2.7, lineheight = 0.9) +
    scale_color_identity() +
    scale_fill_manual(values = seg_colors, guide = "none") +
    scale_y_continuous(breaks = dat$y, labels = levels(dat$Region),
                        expand = expansion(add = 0.7)) +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()),
                        expand = expansion(mult = c(0, 0.22))) +
    labs(title = title_txt, x = x_lab, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
      axis.text.y = element_text(color = "#12335E", size = 10.5),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 12.5, color = "#12335E")
    )

  print(p)
  export_plot(fname, p, width = 11, height = 10.3)
  dat
}

inc_dat <- plot_region_bar(
  type_code = 0,
  title_txt = "Cervical cancer: estimated new cases by GLOBOCAN region, 2024 vs. 2050 projection",
  x_lab = "Estimated number of new cases",
  fname = "figure_5c_regional_incidence_projection.png"
)

mort_dat <- plot_region_bar(
  type_code = 1,
  title_txt = "Cervical cancer: estimated deaths by GLOBOCAN region, 2024 vs. 2050 projection",
  x_lab = "Estimated number of deaths",
  fname = "figure_5d_regional_mortality_projection.png"
)

export_table(
  "figure_5c_regional_incidence_projection.csv",
  inc_dat %>% select(-Region, -y)
)
export_table(
  "figure_5d_regional_mortality_projection.csv",
  mort_dat %>% select(-Region, -y)
)
