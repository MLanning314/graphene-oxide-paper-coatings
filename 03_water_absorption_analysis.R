
# Analysis of Data for Water Absorption

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/water_data_HF.rda"))
load(here("data/water_data_HF_fig.rda"))

# formatting for figure 4.1.1

hf_1_fig <- water_data_HF_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(time = as.numeric(as.character(time))) 

hf_1_img <- ggplot(hf_1_fig, aes(x = time, y = absorption,
                             color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
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
  scale_x_continuous(breaks = seq(0, 60, 10)) +
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
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/hf_1_img.png", plot = hf_1_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, 0.1% GO, 15% WBBC

hf_1 <- water_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_1_60 <- hf_1 |>
  filter(time == 60)
  
fit_oneway_4.1.1 <- aov(raw_weight ~ coating, data = hf_1)
summary(fit_oneway_4.1.1)

fit_oneway_4.1.1_60 <- aov(raw_weight ~ coating, data = hf_1_60)
summary(fit_oneway_4.1.1)

# If you want to account for time as well (recommended)
fit_twoway_4.1.1 <- aov(raw_weight ~ coating * time, data = hf_1)
summary(fit_twoway_4.1.1)

TukeyHSD(fit_oneway_4.1.1_60)



# formatting for figure 4.1.2
hf_2_fig <- water_data_HF_fig |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.2% GO", "0.5% GO")) |>
  mutate(time = as.numeric(as.character(time))) 


hf_2_img <- ggplot(hf_2_fig, aes(x = time, y = absorption,
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
               "0.2% GO" = "goldenrod1",
               "0.5% GO" = "springgreen1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "Stock" = 15,
               "0.1% GO" = 17,
               "0.2% GO" = 18,
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

hf_2 <- water_data_HF |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.20% GO", "0.5% GO")) |>
  mutate(time = as.factor(time)) 

hf_2_60 <- hf_2 |>
  filter(time == 60)

fit_oneway_4.1.2 <- aov(raw_weight ~ coating, data = hf_2)
summary(fit_oneway_4.1.2)

fit_oneway_4.1.2_60 <- aov(raw_weight ~ coating, data = hf_2_60)
summary(fit_oneway_4.1.2_60)

# If you want to account for time as well (recommended)
fit_twoway_4.1.2 <- aov(raw_weight ~ coating * time, data = hf_2)
summary(fit_twoway_4.1.2)

TukeyHSD(fit_oneway_4.1.2, "coating")
TukeyHSD(fit_oneway_4.1.2_60, "coating")

# see differences in variance of coatings
leveneTest(raw_weight ~ coating, data = hf_2)

# visualize coating variability
fig_4.1.2_variability <- ggplot(hf_2, aes(x = coating, y = raw_weight, fill = coating)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "DI Water" = "dodgerblue",   
    "Stock" = "grey65",      
    "0.1% GO" = "firebrick",   
    "0.20% GO" = "goldenrod1",    
    "0.5% GO" = "springgreen1"     
  )) +
  ylab("Water Absorption") +
  xlab("Coating") +
  theme_minimal() +
  theme(legend.position = "none")  

hf_2 |>
  filter(time == 60) |>
  group_by(coating) |>
  summarize(
    sd_absorption = sd(raw_weight)
  )


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

# figure 4.1.13

hf_3_fig <- water_data_HF_fig |>
  filter(coating %in% c("0.2% GO", "0.5% GO", "15% WBBC")) |>
  mutate(time = as.numeric(as.character(time))) 

hf_3_img <- ggplot(hf_3_fig, aes(x = time, y = absorption,
                                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("0.2% GO" = "dodgerblue",
               "0.5% GO" = "firebrick",
               "15% WBBC" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("0.2% GO" = 16,
               "0.5% GO" = 17,
               "15% WBBC" = 15)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    title = "0.2 wt%, 0.5 wt%, and 15% WBBC water absorption of HelloFresh",
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

ggsave("figures/hf_3_img.png", plot = hf_3_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova

hf_3 <- water_data_HF |>
  filter(coating %in% c("0.20% GO", "0.5% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_3_60 <- hf_3 |>
  filter(time == 60)

fit_oneway_4.1.3 <- aov(raw_weight ~ coating, data = hf_3)
summary(fit_oneway_4.1.3)

fit_oneway_4.1.3_60 <- aov(raw_weight ~ coating, data = hf_3_60)
summary(fit_oneway_4.1.3_60)

# If you want to account for time as well (recommended)
fit_twoway_4.1.3 <- aov(raw_weight ~ coating * time, data = hf_3)
summary(fit_twoway_4.1.3)

TukeyHSD(fit_oneway_4.1.3, "coating")
TukeyHSD(fit_oneway_4.1.3_60, "coating")

# figure 4.1.4
hf_4_fig <- water_data_HF_fig |>
  filter(coating %in% c("DI Water", "Stock", "1% WBBC", "5% WBBC", 
                        "15% WBBC", "25% WBBC")) |>
  mutate(time = as.numeric(as.character(time))) |>
  mutate(coating = factor(coating,
                          levels = c("DI Water",
                                     "Stock",
                                     "1% WBBC",
                                     "5% WBBC",
                                     "15% WBBC",
                                     "25% WBBC")))

hf_4_img <- ggplot(hf_4_fig, aes(x = time, y = absorption,
                                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "grey65",
               "1% WBBC" = "firebrick",
               "5% WBBC" = "springgreen1",
               "15% WBBC" = "goldenrod1",
               "25% WBBC" = "mediumpurple1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "Stock" = 4,
               "1% WBBC" = 15,
               "5% WBBC" = 18,
               "15% WBBC" = 8,
               "25% WBBC" = 17)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    title = "Water absorption of WBBC-coated HelloFresh",
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

ggsave("figures/hf_4_img.png", plot = hf_4_img,
       width = 10, height = 6, units = "in", dpi = 600)
