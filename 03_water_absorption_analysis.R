
# Analysis of Data for Water Absorption

# Load Packages 
library(tidyverse)
library(here)

# Load Data
load(here("data/water_data_HF.rda"))

# formatting for figure 4.1.1

hf_1 <- water_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(time = as.numeric(as.character(time))) |>
  group_by(time, coating) |>
  summarise(mean_raw = mean(raw_weight, na.rm = TRUE),
            sd_raw = sd(raw_weight, na.rm = TRUE),
            .groups = "drop")


hf_1_img <- ggplot(hf_1, aes(x = time, y = mean_raw,
                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_raw - sd_raw,
                    ymax = mean_raw + sd_raw),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick",
               "15% WBBC" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,     
               "0.1% GO" = 17,
               "15% WBBC" = 15)      
  ) +
  labs(
    title = "DI water, 0.1% GO, 15% WBBC water absorption of HelloFresh",
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/hf_1_img.png", plot = hf_1_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, 0.1% GO, 15% WBBC

fit_oneway_4.1.1 <- aov(mean_raw ~ coating, data = hf_1)
summary(fit_oneway_4.1.1)

# If you want to account for time as well (recommended)
fit_twoway_4.1.1 <- aov(mean_raw ~ coating * time, data = hf_1)
summary(fit_twoway_4.1.1)

TukeyHSD(fit_oneway_4.1.1, "coating")


# get mean and sd at 60 minutes to see absorption differences
water_data_HF |> 
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC"), 
         time == "60") |> 
  group_by(coating) |> 
  summarise(
    mean_abs = round(mean(raw_weight, na.rm = TRUE), 3),
    sd_abs   = round(sd(raw_weight, na.rm = TRUE), 3),
    n        = n(),  # sample size per group
    .groups = "drop"
  )

water_data_HF |>
  filter(time %in% c(0, 60)) |>
  group_by(coating, time) |>
  summarise(
    mean_w = mean( raw_weight, na.rm = TRUE),
                  sd_w = sd(raw_weight, na.rm = TRUE),
    .groups = "drop")

# formatting for figure 4.1.2

hf_2 <- water_data_HF |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.20% GO", 
                        "0.5% GO")) |>
  mutate(time = as.numeric(time)) |>
  group_by(time, coating) |>
  summarise(mean_raw = mean(raw_weight, na.rm = TRUE),
            sd_raw = sd(raw_weight, na.rm = TRUE),
            .groups = "drop")


hf_2_img <- ggplot(hf_2, aes(x = time, y = mean_raw,
                             color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_raw - sd_raw,
                    ymax = mean_raw + sd_raw),
                width = 0.2, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "black",
               "0.1% GO" = "firebrick",
               "0.20% GO" = "goldenrod1",
               "0.5% GO" = "springgreen1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "Stock" = 15,
               "0.1% GO" = 17,
               "0.20% GO" = 18,
               "0.5% GO" = 8)      
  ) +
  labs(
    title = "GO-coatings vs. controls water absorption of HelloFresh",
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position = c(0.75, 0.20),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/hf_2_img.png", plot = hf_2_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, 0.1% GO, 0.2% GO, 0.5% GO, and stock

fit_oneway_4.1.2 <- aov(mean_raw ~ coating, data = hf_2)
summary(fit_oneway_4.1.2)

# If you want to account for time as well (recommended)
fit_twoway_4.1.2 <- aov(mean_raw ~ coating * time, data = hf_2)
summary(fit_twoway_4.1.2)

TukeyHSD(fit_oneway_4.1.2, "coating")


# get mean and sd at 60 minutes to see absorption differences
water_data_HF |> 
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.20% GO", 
                          "0.5% GO"),
         time == "60") |> 
  group_by(coating) |> 
  summarise(
    mean_abs = round(mean(raw_weight, na.rm = TRUE), 3),
    sd_abs   = round(sd(raw_weight, na.rm = TRUE), 3),
    n        = n(),  # sample size per group
    .groups = "drop"
  )

