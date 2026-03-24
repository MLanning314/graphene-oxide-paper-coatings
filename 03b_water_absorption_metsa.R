
# Analysis of Data for Water Absorption - Metsa

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/water_data_metsa.rda"))
load(here("data/water_data_metsa_fig.rda"))

# formatting for figure 4.1.1

metsa_1_fig <- water_data_metsa_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock")) |>
  mutate(time = as.numeric(as.character(time))) 

metsa_1_img <- ggplot(metsa_1_fig, aes(x = time, y = absorption,
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
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    title = "DI water, Stock, 0.1% GO water absorption of Metsä",
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/metsa_1_img.png", plot = metsa_1_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, Stock, 0.1% GO

metsa_1 <- water_data_metsa |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock")) |>
  mutate(time = as.factor(time)) 

metsa_1_60 <- metsa_1 |>
  filter(time == 60)

fit_oneway_4.1.1 <- aov(raw_weight ~ coating, data = metsa_1)
summary(fit_oneway_4.1.1)

fit_oneway_4.1.1_60 <- aov(raw_weight ~ coating, data = metsa_1_60)
summary(fit_oneway_4.1.1)

# If you want to account for time as well (recommended)
fit_twoway_4.1.1 <- aov(raw_weight ~ coating * time, data = metsa_1)
summary(fit_twoway_4.1.1)

TukeyHSD(fit_oneway_4.1.1_60)

# formatting for figure 4.1.2

metsa_2_fig <- water_data_metsa_fig |>
  filter(coating %in% c("25% WBBC","25% WBBC + 0.1% GO", "0.1% GO", "Stock")) |>
  mutate(time = as.numeric(as.character(time))) 

metsa_2_img <- ggplot(metsa_2_fig, aes(x = time, y = absorption,
                                       color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("25% WBBC" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "25% WBBC + 0.1% GO" = "seagreen2"
               )
  ) +
  scale_shape_manual(
    values = c("25% WBBC" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "25% WBBC + 0.1% GO" = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    title = "Water absorption of Metsä paper with GO, WBBC, and synergistic coatings",
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/metsa_2_img.png", plot = metsa_2_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for synergistic coatings

metsa_2 <- water_data_metsa |>
  filter(coating %in% c("25% WBBC","25% WBBC + 0.1% GO", "0.1% GO", "Stock")) |>
  mutate(time = as.factor(time)) 

metsa_2_60 <- metsa_2 |>
  filter(time == 60)

fit_oneway_4.1.2 <- aov(raw_weight ~ coating, data = metsa_2)
summary(fit_oneway_4.1.2)

fit_oneway_4.1.2_60 <- aov(raw_weight ~ coating, data = metsa_2_60)
summary(fit_oneway_4.1.2_60)

# If you want to account for time as well (recommended)
fit_twoway_4.1.2 <- aov(raw_weight ~ coating * time, data = metsa_2)
summary(fit_twoway_4.1.2)

TukeyHSD(fit_oneway_4.1.2)
TukeyHSD(fit_twoway_4.1.2)
TukeyHSD(fit_oneway_4.1.2_60)

leveneTest(raw_weight ~ coating, data = metsa_2)
