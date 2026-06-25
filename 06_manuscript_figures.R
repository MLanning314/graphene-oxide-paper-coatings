
# Working on Figures for ACS Manuscript

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/tensile_burst_index.rda"))

# figure 3.3.1
# compute mean and sd per coating
fig_4a_summary <- tensile_burst_index |>
  filter(!is.na(tensile_index_j)) |>
  group_by(condition) |>
  mutate(condition = dplyr::recode(
    condition,
    `H1` = "AKD + 0 wt% GO",
    `H2` = "AKD + 0.1 wt% GO",
    `H3` = "AKD + 0.2 wt% GO",
    `H4` = "ASA + 0 wt% GO",
    `H5` = "ASA + 0.1 wt% GO",
    `H6` = "ASA + 0.2 wt% GO"
  ),
  condition = factor(condition, 
                   levels = c("ASA + 0 wt% GO", "ASA + 0.1 wt% GO", 
                              "ASA + 0.2 wt% GO", "AKD + 0 wt% GO",
                              "AKD + 0.1 wt% GO", "AKD + 0.2 wt% GO"
                              ))) |>
  summarise(
    mean_strength = mean(tensile_index_j),
    sd_strength = sd(tensile_index_j),
    .groups = "drop")

# create figure 4a
fig_4a <- ggplot(fig_4a_summary, aes(x = condition, y = mean_strength, fill = condition)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_strength - sd_strength, ymax = mean_strength + sd_strength),
                width = 0.2, size = 0.5) +
  scale_fill_manual(values = c(
    "ASA + 0 wt% GO" = "goldenrod1",
    "AKD + 0 wt% GO" = "goldenrod1",
    "ASA + 0.1 wt% GO" = "firebrick",
    "AKD + 0.1 wt% GO" = "firebrick",
    "ASA + 0.2 wt% GO" = "seagreen2",
    "AKD + 0.2 wt% GO" = "seagreen2"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 60),
                     breaks = seq(0, 60, by = 15)) +
  labs(x = "", y = "Tensile Index (N.m/g)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"))

ggsave("figures/fig_3.3.1.png", plot = fig_3.3.1,
       width = 10, height = 6, units = "in", dpi = 600)