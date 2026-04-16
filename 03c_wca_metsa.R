
# Analysis of Data for Water Contact Angle - Metsa

# Load Packages 
library(tidyverse)
library(here)
library(car)
library(emmeans)

# Load Data
load(here("data/wca_data_metsa.rda"))

# formatting for figure 4.2.2
wca_1_fig <- wca_data_metsa |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock")) |>
  mutate(
    time = as.numeric(time),
    coating = factor(
      coating,
      levels = c("Stock", "DI Water", "0.1% GO")))

fig_4.2.1 <- ggplot(wca_1_fig, aes(x = time, y = contact_angle,
                                       color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
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
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  labs(
    x = "Time (s)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_4.2.1.png", plot = fig_4.2.1,
       width = 10, height = 6, units = "in", dpi = 600)

# statistics for DI water, Stock, 0.1% GO

wca_lm_1 <- lm(contact_angle ~ coating * time, data = wca_1_fig)
anova(wca_lm_1)
summary(wca_lm_1)

# time-specific estimated means and pairwise differences
wca_1_emm <- emmeans(wca_lm_1, ~ coating | time,
                 at = list(time = c(100, 600)))
wca_1_emm                 
pairs(wca_1_emm)   

# formatting for figure 4.2.2
fig_4.2.2 <- ggplot(wca_1_fig, aes(x = time, y = volume,
                                   color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
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
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  labs(
    x = "Time (s)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_4.2.2.png", plot = fig_4.2.2,
       width = 10, height = 6, units = "in", dpi = 600)


# formatting for figure 4.2.3
wca_3_fig <- wca_data_metsa |>
  filter(coating %in% c("0.1% GO", "Stock", "25% WBBC", "25% WBBC + 0.1% GO")) |>
  mutate(
    time = as.numeric(time),
    coating = dplyr::recode(coating,
                            `25% WBBC + 0.1% GO` = "9.75% Joncryl + 0.1% GO",
                            `25% WBBC` = "9.75% Joncryl"),
    coating = factor(coating,
                     levels = c("9.75% Joncryl + 0.1% GO",
                                "9.75% Joncryl",
                                "Stock",
                                "0.1% GO")))

fig_4.2.3 <- ggplot(wca_3_fig, aes(x = time, y = contact_angle,
                                   color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("9.75% Joncryl" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl + 0.1% GO" = "seagreen2")
  ) +
  scale_shape_manual(
    values = c("9.75% Joncryl" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "9.75% Joncryl + 0.1% GO" = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  labs(
    x = "Time (s)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/wca_2_img.png", plot = wca_2_img,
       width = 10, height = 6, units = "in", dpi = 600)

# statistics for DI water, Stock, 0.1% GO

wca_lm_2 <- lm(contact_angle ~ coating * time, data = wca_3_fig)
anova(wca_lm_2)
summary(wca_lm_2)

# time-specific estimated means and pairwise differences
wca_2_emm <- emmeans(wca_lm_2, ~ coating | time,
                     at = list(time = c(100, 600)))
wca_2_emm                 
pairs(wca_2_emm)            


fig_4.2.4 <- ggplot(wca_3_fig, aes(x = time, y = volume,
                                   color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("9.75% Joncryl" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl + 0.1% GO" = "seagreen2")
  ) +
  scale_shape_manual(
    values = c("9.75% Joncryl" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "9.75% Joncryl + 0.1% GO" = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  labs(
    x = "Time (s)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_4.2.4.png", plot = fig_4.2.4,
       width = 10, height = 6, units = "in", dpi = 600)









