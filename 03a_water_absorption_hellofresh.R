
# Analysis of Data for Water Absorption - HelloFresh

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_HF.rda"))
load(here("data/absorption_HF_fig.rda"))

# formatting for figure 3.1.1

hf_1_fig <- absorption_HF_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(
    time = as.numeric(time),
    coating = factor(
      dplyr::recode(coating, `15% WBBC` = "5.85% Joncryl"),
      levels = c("DI Water", "0.1% GO", "5.85% Joncryl")
    ))

fig_3.1.1 <- ggplot(hf_1_fig, aes(x = time, y = absorption,
                             color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 0.1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick",
               "5.85% Joncryl" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "0.1% GO" = 17,
               "5.85% Joncryl" = 15)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    x = "Time (min)",
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

ggsave("figures/fig_3.1.1.png", plot = fig_3.1.1,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, 0.1% GO, 15% WBBC

hf_1 <- absorption_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_1_60 <- hf_1 |>
  filter(time == 60)
  
fit_oneway_3.1.1 <- aov(absorption ~ coating, data = hf_1)
summary(fit_oneway_3.1.1)

fit_oneway_3.1.1_weight <- aov(raw_weight ~ coating, data = hf_1)
summary(fit_oneway_3.1.1_weight)

fit_oneway_3.1.1_60 <- aov(absorption ~ coating, data = hf_1_60)
summary(fit_oneway_3.1.1_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.1 <- aov(absorption ~ coating * time, data = hf_1)
summary(fit_twoway_3.1.1)

TukeyHSD(fit_oneway_3.1.1)
TukeyHSD(fit_oneway_3.1.1_60)
TukeyHSD(fit_oneway_3.1.1_weight)



# formatting for figure 3.1.2
hf_2_fig <- absorption_HF_fig |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.2% GO", "0.5% GO")) |>
  mutate(
    time = as.numeric(time),
    coating = factor(
      coating,
      levels = c("DI Water", "Stock", "0.1% GO",
                 "0.2% GO", "0.5% GO")))

fig_3.1.2 <- ggplot(hf_2_fig, aes(x = time, y = absorption,
                             color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 0.1, linewidth = 0.5) +
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
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.20),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_3.1.2.png", plot = fig_3.1.2,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for DI water, 0.1% GO, 0.2% GO, 0.5% GO, and stock

hf_2 <- absorption_data_HF |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.20% GO", "0.5% GO")) |>
  mutate(time = as.factor(time)) 

hf_2_60 <- hf_2 |>
  filter(time == 60)

fit_oneway_3.1.2 <- aov(absorption ~ coating, data = hf_2)
summary(fit_oneway_3.1.2)

fit_oneway_3.1.2_60 <- aov(absorption ~ coating, data = hf_2_60)
summary(fit_oneway_3.1.2_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.2 <- aov(absorption ~ coating * time, data = hf_2)
summary(fit_twoway_3.1.2)

TukeyHSD(fit_oneway_3.1.2, "coating")
TukeyHSD(fit_oneway_3.1.2_60, "coating")

# see differences in variance of coatings
leveneTest(absorption ~ coating, data = hf_2)


# formatting for figure 3.1.3

hf_3_fig <- absorption_HF_fig |>
  filter(coating %in% c("0.2% GO", "0.5% GO", "15% WBBC")) |>
  mutate(time = as.numeric(time),
         coating = factor(
           dplyr::recode(coating, `15% WBBC` = "5.85% Joncryl"),
           levels = c("0.2% GO", "0.5% GO", "5.85% Joncryl")))

fig_3.1.3 <- ggplot(hf_3_fig, aes(x = time, y = absorption,
                                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 0.1, linewidth = 0.5) +
  scale_color_manual(
    values = c("0.2% GO" = "dodgerblue",
               "0.5% GO" = "firebrick",
               "5.85% Joncryl" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("0.2% GO" = 16,
               "0.5% GO" = 17,
               "5.85% Joncryl" = 15)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    x = "Time (min)",
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

ggsave("figures/fig_3.1.3.png", plot = fig_3.1.3,
       width = 10, height = 6, units = "in", dpi = 600)

# anova for 0.2% GO, 0.5% GO, and 15% WBBC

hf_3 <- absorption_data_HF |>
  filter(coating %in% c("0.20% GO", "0.5% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_3_60 <- hf_3 |>
  filter(time == 60)

fit_oneway_3.1.3 <- aov(absorption ~ coating, data = hf_3)
summary(fit_oneway_3.1.3)

fit_oneway_3.1.3_60 <- aov(absorption ~ coating, data = hf_3_60)
summary(fit_oneway_3.1.3_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.3 <- aov(absorption ~ coating * time, data = hf_3)
summary(fit_twoway_3.1.3)

TukeyHSD(fit_oneway_3.1.3, "coating")
TukeyHSD(fit_oneway_3.1.3_60, "coating")

# figure 3.1.4
hf_4_fig <- absorption_HF_fig |>
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

fig_3.1.4 <- ggplot(hf_4_fig, aes(x = time, y = absorption,
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
    x = "Time (min)",
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

ggsave("figures/fig_3.1.4.png", plot = fig_3.1.4,
       width = 10, height = 6, units = "in", dpi = 600)

# anova

hf_4 <- water_data_HF |>
  filter(coating %in% c("DI Water", "Stock", "1% WBBC", "5% WBBC", 
                        "15% WBBC", "25% WBBC")) |>
  mutate(time = as.factor(time)) 

hf_4_60 <- hf_4 |>
  filter(time == 60)

fit_oneway_3.1.4 <- aov(raw_weight ~ coating, data = hf_4)
summary(fit_oneway_3.1.4)

fit_oneway_3.1.4_60 <- aov(raw_weight ~ coating, data = hf_4_60)
summary(fit_oneway_3.1.4_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.4 <- aov(raw_weight ~ coating * time, data = hf_4)
summary(fit_twoway_3.1.4)

TukeyHSD(fit_oneway_3.1.4, "coating")
TukeyHSD(fit_oneway_3.1.4_60, "coating")

leveneTest(raw_weight ~ coating, data = hf_4)

# figure 3.1.5
hf_5_fig <- water_data_HF_fig |>
  filter(coating %in% c("Stock", "25% WBBC", "25% WBBC + 0.1% GO", 
                        "0.1% GO")) |>
  mutate(time = as.numeric(as.character(time))) |>
  mutate(coating = factor(coating,
                          levels = c("Stock",
                                     "0.1% GO",
                                     "25% WBBC",
                                     "25% WBBC + 0.1% GO")))


hf_5_img <- ggplot(hf_5_fig, aes(x = time, y = absorption,
                                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "25% WBBC" = "dodgerblue",
               "25% WBBC + 0.1% GO" = "springgreen1"
  )) +
  scale_shape_manual(
    values = c( "Stock" = 16,
               "0.1% GO" = 15,
               "25% WBBC" = 18,
               "25% WBBC + 0.1% GO" = 17)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    title = "Water absorption of HelloFresh paper with GO, WBBC, and synergistic coatings",
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

ggsave("figures/hf_5_img.png", plot = hf_5_img,
       width = 10, height = 6, units = "in", dpi = 600)

# anova

hf_5 <- water_data_HF |>
  filter(coating %in% c("Stock", "25% WBBC", "25% WBBC + 0.1% GO", 
                        "0.1% GO")) |>
  mutate(time = as.factor(time)) 

hf_5_absorption <- absorption_data_HF |>
  filter(coating %in% c("Stock", "25% WBBC", "25% WBBC + 0.1% GO", 
                        "0.1% GO")) |>
  mutate(time = as.factor(time)) 
  
hf_5_60 <- hf_5 |>
  filter(time == 60)

fit_oneway_3.1.5 <- aov(raw_weight ~ coating, data = hf_5)
summary(fit_oneway_3.1.5)

fit_oneway_absorb_3.1.5 <- aov(absorption_large ~ coating, 
                               data = hf_5_absorption)
summary(fit_oneway_absorb_3.1.5)

fit_oneway_3.1.5_60 <- aov(raw_weight ~ coating, data = hf_5_60)
summary(fit_oneway_3.1.5_60)

# If you want to account for time as well (recommended)
fit_twoway_3.1.5 <- aov(raw_weight ~ coating * time, data = hf_5)
summary(fit_twoway_3.1.5)

fit_twoway_absorb_3.1.5 <- aov(absorption_large ~ coating * time, 
                               data = hf_5_absorption)
summary(fit_twoway_absorb_3.1.5)

TukeyHSD(fit_oneway_3.1.5, "coating")
TukeyHSD(fit_oneway_3.1.5_60, "coating")
TukeyHSD(fit_twoway_absorb_3.1.5)
TukeyHSD(fit_oneway_absorb_3.1.5)




