
# Figures for Undergraduate Research Expo - 2026

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_HF.rda"))
load(here("data/absorption_HF_fig.rda"))
load(here("data/tensile_strength_md.rda"))

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

# Figure 3 - Tensile Strength HelloFresh

md_pres <- tensile_strength_md |>
  group_by(coating) |>
  mutate(coating = factor(coating, 
                          levels = c("DI Water", "Stock", 
                                     "0.1 wt% GO",
                                     "15% WBBC",
                                     "15% WBBC + 0.1% GO"))) |>
  summarise(
    mean_strength = mean(tensile_strength_n),
    sd_strength = sd(tensile_strength_n),
    .groups = "drop")

# create figure 3.3.1
pres_fig_3 <- ggplot(md_pres, aes(x = coating, y = mean_strength, fill = coating)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_strength - sd_strength, ymax = mean_strength + sd_strength),
                width = 0.2, size = 0.5) +
  scale_fill_manual(values = c(
    "DI Water" = "dodgerblue",
    "Stock" = "grey65",
    "0.1 wt% GO" = "firebrick",
    "15% WBBC" = "goldenrod1",
    "15% WBBC + 0.1% GO" = "seagreen2"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 75)) +
  labs(x = "", y = "Tensile Strength (N)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank())

ggsave("presentation_figures/pres_fig_3.png", plot = pres_fig_3,
       width = 10, height = 6, units = "in", dpi = 600)

