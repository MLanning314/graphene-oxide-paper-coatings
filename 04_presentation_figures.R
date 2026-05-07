
# Figures for Undergraduate Research Expo - 2026

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_HF.rda"))
load(here("data/absorption_HF_fig.rda"))

# Figure 1: DI Water, 0.1% GO, 9.75% Joncryl - HelloFresh

pres_1_fig <- absorption_HF_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "25% WBBC")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = factor(
      dplyr::recode(coating, `25% WBBC` = "9.75% Joncryl"),
      levels = c("DI Water", "0.1% GO", "9.75% Joncryl")
    ))

pres_fig_1 <- ggplot(pres_1_fig, aes(x = time, y = absorption,
                                  color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "0.1% GO" = 17,
               "9.75% Joncryl" = 15)
  ) +
  scale_x_continuous(breaks = seq(10, 60, 10)) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 16, face = "plain"),
    axis.title.y = element_text(size = 16, face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12, face = "plain")
  )

ggsave("presentation_figures/pres_fig_1.png", plot = pres_fig_1,
       width = 10, height = 6, units = "in", dpi = 600)

# Figure 2 - DI water, GO, WBBC, Combination - HelloFresh

pres_2_fig <- absorption_HF_fig |>
  filter(coating %in% c("DI Water", "25% WBBC", "25% WBBC + 0.1% GO", 
                        "0.1% GO")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = dplyr::recode(coating,
                            `25% WBBC + 0.1% GO` = "9.75% Joncryl + 0.1% GO",
                            `25% WBBC` = "9.75% Joncryl"),
    coating = factor(coating,
                     levels = c("DI Water",
                                "0.1% GO",
                                "9.75% Joncryl",
                                "9.75% Joncryl + 0.1% GO")))


pres_fig_2 <- ggplot(pres_2_fig, aes(x = time, y = absorption,
                                  color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl" = "goldenrod1",
               "9.75% Joncryl + 0.1% GO" = "springgreen1"
    )) +
  scale_shape_manual(
    values = c( "DI Water" = 16,
                "0.1% GO" = 15,
                "9.75% Joncryl" = 18,
                "9.75% Joncryl + 0.1% GO" = 17)
  ) +
  scale_x_continuous(breaks = seq(10, 60, 10)) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 16, face = "plain"),
    axis.title.y = element_text(size = 16, face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("presentation_figures/pres_fig_2.png", plot = pres_fig_2,
       width = 10, height = 6, units = "in", dpi = 600)
