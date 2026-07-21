
# Figures for ACS Manuscript ----

## Load Packages ----
library(tidyverse)
library(here)
library(car)
library(patchwork)

## Load data ----
load(here("manuscript/data/tensile_burst_index.rda"))
load(here("manuscript/data/absorption_data_RF.rda"))
load(here("manuscript/data/absorption_RF_fig.rda"))

# --------------------------------------------------------------------------

## Figure 1 ----
# cleaning data for figure 1
fig_1_summary <- absorption_RF_fig |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.2% GO", 
                        "0.5% GO", "15% WBBC")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = factor(
      dplyr::recode(coating, 
                    `0.1% GO` = "0.1 wt % GO",
                    `0.2% GO` = "0.2 wt % GO",
                    `0.5% GO` = "0.5 wt % GO",
                    `15% WBBC` = "5.85 wt % SA"),
      levels = c("DI Water", "Stock", "0.1 wt % GO",
                 "0.2 wt % GO", "0.5 wt % GO", "5.85 wt % SA")))

# plotting data for figure 1
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
               "0.1 wt % GO" = "firebrick",
               "0.2 wt % GO" = "goldenrod1",
               "0.5 wt % GO" = "springgreen1",
               "5.85 wt % SA" = "purple")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "Stock" = 15,
               "0.1 wt % GO" = 8,
               "0.2 wt % GO" = 18,
               "0.5 wt % GO" = 17,
               "5.85 wt % SA" = 4)      
  ) +
  scale_x_continuous(breaks = seq(10, 60, 10)) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title.x = element_text(size = 16, face = "plain"),
    axis.title.y = element_text(size = 16, face = "plain"),
    legend.position.inside = c(0.75, 0.20),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

# saving figure 1
ggsave("manuscript/figures/fig_1.png", plot = fig_1,
       width = 10, height = 6, units = "in", dpi = 600)

# --------------------------------------------------------------------------

## Figure 2 ----
# cleaning data for figure 2
fig_2_summary <- tensile_data_clammp |>
  filter(size == "TAPPI",
         direction == "Machine") |>
  mutate(
    coating = factor(
      dplyr::recode(coating, 
                    `0.15 wt% GO` = "0.15 wt % GO",
                    `0.2 wt% GO` = "0.2 wt % GO",
                    `0.5 wt% GO` = "0.5 wt % GO",
                    `5% WBBC` = "1.95 wt % SA",
                    `15% WBBC` = "5.85 wt % SA",
                    `25% WBBC` = "9.75 wt % SA",
                    `5% WBBC + 0.1% GO` = "1.95 wt % SA + 0.1 wt % GO",
                    `25% WBBC + 0.1% GO` = "9.75 wt % SA + 0.1 wt % GO"),
      levels = c("DI Water", "Stock", "0.15 wt % GO",
                 "0.2 wt % GO", "0.5 wt % GO", "1.95 wt % SA", "5.85 wt % SA",
                 "9.75 wt % SA", "1.95 wt % SA + 0.1 wt % GO", "9.75 wt % SA + 0.1 wt % GO"))) |>
  group_by(coating) |>
  summarise(
    mean = mean(strength_n, na.rm = TRUE),      
    sd = sd(strength_n, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# plotting data for figure 2
fig_2 <- ggplot(fig_2_summary, aes(x = coating, y = mean)) +
  geom_col(fill = "#56B4E9",width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - sd,
        ymax = mean + sd),
    width = 0.2,
    linewidth = 0.7
  ) +
  labs(
    x = "",
    y = "Tensile Strength (N)") +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, face = "plain"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black")
  )

ggsave("manuscript/figures/fig_2.png", plot = fig_2,
       width = 10, height = 6, units = "in", dpi = 600)




# Figure 3 ----
# cleaning data for figure 3
fig_3_summary <- absorption_HF_fig |>
  filter(coating %in% c("Stock", "25% WBBC", "25% WBBC + 0.1% GO", 
                        "0.1% GO")) |>
  mutate(
    time = (as.numeric(time) - 1) * 10,
    coating = dplyr::recode(coating,
                            `25% WBBC + 0.1% GO` = "9.75 wt% SA + 0.1 wt% GO",
                            `25% WBBC` = "9.75 wt% SA",
                            `0.1% GO` = "0.1 wt% GO"),
    coating = factor(coating,
                     levels = c("Stock",
                                "0.1 wt% GO",
                                "9.75 wt% SA",
                                "9.75 wt% SA + 0.1 wt% GO")))


# plotting data for figure 2
fig_3 <- ggplot(fig_2_summary, aes(x = time, y = absorption,
                                  color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("Stock" = "grey65",
               "0.1 wt% GO" = "firebrick",
               "9.75 wt% SA" = "dodgerblue",
               "9.75 wt% SA + 0.1 wt% GO" = "springgreen1"
    )) +
  scale_shape_manual(
    values = c( "Stock" = 16,
                "0.1 wt% GO" = 15,
                "9.75 wt% SA" = 18,
                "9.75 wt% SA + 0.1 wt% GO" = 17)
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

# saving figure 2
ggsave("figures/fig_2.png", plot = fig_2,
       width = 10, height = 6, units = "in", dpi = 600)

# --------------------------------------------------------------------------

## Figure 4 ----

### Figure 4a: Tensile Index ----
# compute mean and sd per coating and summarise
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

# plotting data for figure 4a
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
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        plot.tag = element_text(face = "bold", size = 24),
        plot.margin = margin(5.5, 5.5, 5.5, 20)) +
  geom_vline(xintercept = 4, linewidth = 0.5) +
  annotate("text", x = 2, y = 58, label = "ASA", size = 10) +
  annotate("text", x = 6, y = 58, label = "AKD", size = 10)

### Figure 4b: Burst Index ----
# compute mean and sd per coating and summarise
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

# plotting data for figure 4b
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
  theme(axis.text.x = element_text(size = 14, face = "bold",
                                   angle = 45, hjust = 1, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        plot.tag = element_text(face = "bold", size = 24),
        plot.margin = margin(5.5, 5.5, 5.5, 20)) +
  geom_vline(xintercept = 4, linewidth = 0.5) +
  annotate("text", x = 2, y = 4.8, label = "ASA", size = 10) +
  annotate("text", x = 6, y = 4.8, label = "AKD", size = 10)

### Combination ----
# use patchwork to combine the plots together for figure 4 
fig_4 <- (fig_4a / fig_4b) +
  plot_annotation(tag_levels = "A")

# save figure 4
ggsave("figures/fig_4.png", plot = fig_4,
       width = 14, height = 14, units = "in", dpi = 600)


