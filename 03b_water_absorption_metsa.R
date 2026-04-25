
# Analysis of Data for Water Absorption - Metsa

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_metsa.rda"))
load(here("data/absorption_metsa_fig.rda"))

# formatting for figure 4.1.1

metsa_1_fig <- absorption_metsa_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock")) |>
  mutate(time = (as.numeric(time) - 1) * 10,
         coating = factor(
           coating,
           levels = c("DI Water", "0.1% GO",
                      "Stock")))

fig_4.1.1 <- ggplot(metsa_1_fig, aes(x = time, y = absorption,
                                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "0.1% GO" = 17,
               "Stock" = 15)
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

ggsave("figures/fig_4.1.1.png", plot = fig_4.1.1,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, Stock, 0.1% GO
metsa_1 <- absorption_data_metsa |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock")) |>
  mutate(time = as.factor(time)) 

metsa_1_60 <- metsa_1 |>
  filter(time == 60)

fit_oneway_4.1.1 <- aov(absorption ~ coating, data = metsa_1)
summary(fit_oneway_4.1.1)

fit_oneway_4.1.1_60 <- aov(absorption ~ coating, data = metsa_1_60)
summary(fit_oneway_4.1.1_60)

# If you want to account for time as well (recommended)
fit_twoway_4.1.1 <- aov(absorption ~ coating * time, data = metsa_1)
summary(fit_twoway_4.1.1)


TukeyHSD(fit_oneway_4.1.1)
TukeyHSD(fit_oneway_4.1.1_60)


# formatting for figure 4.1.2
metsa_2_fig <- absorption_metsa_fig |>
  filter(coating %in% c("25% WBBC","25% WBBC + 0.1% GO", "0.1% GO", "Stock")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = dplyr::recode(coating,
                            `25% WBBC + 0.1% GO` = "9.75% Joncryl + 0.1% GO",
                            `25% WBBC` = "9.75% Joncryl"),
    coating = factor(coating,
                     levels = c("0.1% GO",
                                "Stock",
                                "9.75% Joncryl",
                                "9.75% Joncryl + 0.1% GO")))

fig_4.1.2 <- ggplot(metsa_2_fig, aes(x = time, y = absorption,
                                       color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("9.75% Joncryl" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl + 0.1% GO" = "seagreen2"
               )
  ) +
  scale_shape_manual(
    values = c("9.75% Joncryl" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "9.75% Joncryl + 0.1% GO" = 18)
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

ggsave("figures/fig_4.1.2.png", plot = fig_4.1.2,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for synergistic coatings
metsa_2 <- absorption_data_metsa |>
  filter(coating %in% c("25% WBBC","25% WBBC + 0.1% GO", "0.1% GO", "Stock")) |>
  mutate(time = as.factor(time)) 

metsa_2_60 <- metsa_2 |>
  filter(time == 60)

fit_oneway_4.1.2 <- aov(absorption ~ coating, data = metsa_2)
summary(fit_oneway_4.1.2)

fit_oneway_4.1.2_60 <- aov(absorption ~ coating, data = metsa_2_60)
summary(fit_oneway_4.1.2_60)

fit_twoway_4.1.2 <- aov(absorption ~ coating * time, data = metsa_2)
summary(fit_twoway_4.1.2)

TukeyHSD(fit_oneway_4.1.2)
TukeyHSD(fit_twoway_4.1.2)
TukeyHSD(fit_oneway_4.1.2_60)

# check and see if variability is increased or decreasesd
leveneTest(absorption ~ coating, data = metsa_2)
