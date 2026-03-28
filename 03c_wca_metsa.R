
# Analysis of Data for Water Contact Angle - Metsa

# Load Packages 
library(tidyverse)
library(here)
library(car)
library(emmeans)

# Load Data
load(here("data/wca_data_metsa.rda"))

# formatting for figure 4.1.3
wca_1_fig <- wca_data_metsa |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock"))

wca_1_img <- ggplot(wca_1_fig, aes(x = time, y = contact_angle,
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
    title = "DI water, Stock, 0.1% GO water contact angle",
    x = "Time (s)",
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

ggsave("figures/wca_1_img.png", plot = wca_1_img,
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

# formatting for figure 4.1.4
wca_2_fig <- wca_data_metsa |>
  filter(coating %in% c("0.1% GO", "Stock", "25% WBBC", "25% WBBC + 0.1% GO"))

wca_2_img <- ggplot(wca_2_fig, aes(x = time, y = contact_angle,
                                   color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("25% WBBC" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "25% WBBC + 0.1% GO" = "seagreen2")
  ) +
  scale_shape_manual(
    values = c("25% WBBC" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "25% WBBC + 0.1% GO" = 18)
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
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/wca_2_img.png", plot = wca_2_img,
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







