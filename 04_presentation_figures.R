
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
# anova for DI water, 0.1% GO, 15% WBBC

hf_1 <- absorption_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_1_60 <- hf_1 |>
  filter(time == 60)

fit_oneway_3.1.1 <- aov(absorption ~ coating, data = hf_1)
summary(fit_oneway_3.1.1)

fit_oneway_3.1.1_weight <- aov(raw_weight ~ coating, data = hf_1)
summary(fit_oneway_3.1.1_weight)

fit_oneway_3.1.1_60 <- aov(absorption ~ coating, data = hf_1_60)
summary(fit_oneway_3.1.1_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.1 <- aov(absorption ~ coating * time, data = hf_1)
summary(fit_twoway_3.1.1)

TukeyHSD(fit_oneway_3.1.1)
TukeyHSD(fit_oneway_3.1.1_60)
TukeyHSD(fit_oneway_3.1.1_weight)
