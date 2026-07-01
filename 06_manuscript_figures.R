
# Working on Figures for ACS Manuscript

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/tensile_burst_index.rda"))
load(here("data/absorption_data_HF.rda"))
load(here("data/absorption_HF_fig.rda"))

# formatting for figure 3.1.3
fig_1_summary <- absorption_HF_fig |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.2% GO", 
                        "0.5% GO", "15% WBBC")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = factor(
      dplyr::recode(coating, 
                    `0.1% GO` = "0.1 wt% GO",
                    `0.2% GO` = "0.2 wt% GO",
                    `0.5% GO` = "0.5 wt% GO",
                    `15% WBBC` = "5.85 wt% SA"),
      levels = c("DI Water", "Stock", "0.1 wt% GO",
                 "0.2 wt% GO", "0.5 wt% GO", "5.85 wt% SA")))


fig_1 <- ggplot(fig_1_summary, aes(x = time, y = absorption,
                                  color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "grey65",
               "0.1 wt% GO" = "firebrick",
               "0.2 wt% GO" = "goldenrod1",
               "0.5 wt% GO" = "springgreen1",
               "5.85 wt% SA" = "purple")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "Stock" = 15,
               "0.1 wt% GO" = 8,
               "0.2 wt% GO" = 18,
               "0.5 wt% GO" = 17,
               "5.85 wt% SA" = 4)      
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
    legend.position.inside = c(0.75, 0.20),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_1.png", plot = fig_1,
       width = 10, height = 6, units = "in", dpi = 600)


# figure 4a
# compute mean and sd per coating
fig_4a_summary <- tensile_burst_index |>
  filter(!is.na(tensile_index_j),
         !is.na(burst_index_j)) |>
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
    .groups = "drop") |>
  mutate(xpos = c(1, 2, 3, 5, 6, 7))


# create figure 4a
fig_4a <- ggplot(fig_4a_summary, aes(x = xpos, y = mean_strength, fill = condition)) +
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
  scale_x_continuous(
    breaks = c(1, 2, 3, 5, 6, 7),
    labels = c(
      "0 wt% GO",
      "0.1 wt% GO",
      "0.2 wt% GO",
      "0 wt% GO",
      "0.1 wt% GO",
      "0.2 wt% GO"
    )
  ) +
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
        axis.line.x = element_line(color = "black")) +
  geom_vline(xintercept = 4, linewidth = 0.5) +
  annotate("text", x = 2, y = 58, label = "ASA", size = 5) +
  annotate("text", x = 6, y = 58, label = "AKD", size = 5)

ggsave("figures/fig_4a.png", plot = fig_4a,
       width = 10, height = 6, units = "in", dpi = 600)

# figure 4b
fig_4b_summary <- tensile_burst_index |>
  filter(!is.na(tensile_index_j),
         !is.na(burst_index_j)) |>
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
    mean_strength = mean(burst_index_j),
    sd_strength = sd(burst_index_j),
    .groups = "drop") |>
  mutate(xpos = c(1, 2, 3, 5, 6, 7))

fig_4b <- ggplot(fig_4b_summary, aes(x = xpos, y = mean_strength, fill = condition)) +
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
  scale_x_continuous(
    breaks = c(1, 2, 3, 5, 6, 7),
    labels = c(
      "0 wt% GO",
      "0.1 wt% GO",
      "0.2 wt% GO",
      "0 wt% GO",
      "0.1 wt% GO",
      "0.2 wt% GO"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 5),
                     breaks = seq(0, 5, by = 1)) +
  labs(x = "", y = expression(Burst~Index~(kPa %.% m^2 / g))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black")) +
  geom_vline(xintercept = 4, linewidth = 0.5) +
  annotate("text", x = 2, y = 4.8, label = "ASA", size = 5) +
  annotate("text", x = 6, y = 4.8, label = "AKD", size = 5)

ggsave("figures/fig_4b.png", plot = fig_4b,
       width = 10, height = 6, units = "in", dpi = 600)

