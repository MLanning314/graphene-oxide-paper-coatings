# Analysis of Data for Water Absorption - International Paper

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_IP.rda"))
load(here("data/absorption_IP_fig.rda"))

# formatting for figure

IP_1_fig <- absorption_IP_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock",
                        "25% WBBC")) |>
  mutate(time = (as.numeric(time) - 1) * 10,
         coating = dplyr::recode(coating,
                                 `25% WBBC` = "9.75% Joncryl"),
         coating = factor(coating,
           levels = c("Stock", "DI Water", "0.1% GO", "9.75% Joncryl")))

fig_4.3.1 <- ggplot(IP_1_fig, aes(x = time, y = absorption,
                                     color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl" = "goldenrod1"
    )
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "9.75% Joncryl" = 18)
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

ggsave("figures/fig_4.3.1.png", plot = fig_4.3.1,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, Stock, 0.1% GO, and 25% WBBC
IP_1 <- absorption_data_IP |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock", "25% WBBC")) |>
  mutate(time = as.factor(time)) 

IP_1_60 <- IP_1 |>
  filter(time == 60)

fit_oneway_4.3.1 <- aov(absorption ~ coating, data = IP_1)
summary(fit_oneway_4.3.1)

fit_oneway_4.3.1_60 <- aov(absorption ~ coating, data = IP_1_60)
summary(fit_oneway_4.3.1_60)

fit_twoway_4.3.1 <- aov(absorption ~ coating * time, data = IP_1)
summary(fit_twoway_4.3.1)

TukeyHSD(fit_oneway_4.3.1)
TukeyHSD(fit_twoway_4.3.1)
TukeyHSD(fit_oneway_4.3.1_60)

# formatting for figure 4.3.2
IP_2_fig <- absorption_IP_fig |>
  filter(coating %in% c("0.2% GO", "0.1% GO", "0.5% GO",
                        "25% WBBC")) |>
  mutate(time = (as.numeric(time) - 1) * 10,
         coating = dplyr::recode(coating,
                                 `25% WBBC` = "9.75% Joncryl"),
         coating = factor(coating,
                          levels = c("0.5% GO", "0.1% GO", "0.2% GO", "9.75% Joncryl")))

fig_4.3.2 <- ggplot(IP_2_fig, aes(x = time, y = absorption,
                                  color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 0.1, linewidth = 0.5) +
  scale_color_manual(
    values = c("0.2% GO" = "goldenrod1",
               "0.5% GO" = "springgreen1",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl" = "dodgerblue"
    )
  ) +
  scale_shape_manual(
    values = c("0.2% GO" = 16,
               "0.1% GO" = 17,
               "0.5% GO" = 15,
               "9.75% Joncryl" = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
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

ggsave("figures/fig_4.3.2.png", plot = fig_4.3.2,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for 0.1% GO, 0.2% GO, 0.5% GO, and 25% WBBC
IP_2 <- absorption_data_IP |>
  filter(coating %in% c("0.2% GO", "0.1% GO", "0.5% GO", "25% WBBC")) |>
  mutate(time = as.factor(time)) 

IP_2_60 <- IP_2 |>
  filter(time == 60)

fit_oneway_4.3.2 <- aov(absorption ~ coating, data = IP_2)
summary(fit_oneway_4.3.2)

fit_oneway_4.3.2_60 <- aov(absorption ~ coating, data = IP_2_60)
summary(fit_oneway_4.3.2_60)

fit_twoway_4.3.2 <- aov(absorption ~ coating * time, data = IP_2)
summary(fit_twoway_4.3.2)

TukeyHSD(fit_oneway_4.3.2)
TukeyHSD(fit_twoway_4.3.2)
TukeyHSD(fit_oneway_4.3.2_60)


