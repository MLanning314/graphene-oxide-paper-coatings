
# Analysis of Data for Tensile Strength

# Load Packages 
library(tidyverse)
library(here)

# Load data
load(here("data/tensile_strength_md.rda"))
load(here("data/tensile_strength_cd.rda"))

# machine direction anova
md_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_md)
summary(md_anova)

TukeyHSD(md_anova)

# cd anova
cd_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_cd)
summary(cd_anova)

TukeyHSD(cd_anova)

# figure 3.2.1
# compute mean and sd per coating
summary_3.2.1 <- tensile_strength_md |>
  filter(coating %in% c("DI Water", "Stock", "0.1 wt% GO", "0.2 wt% GO",
                        "0.35 wt% GO", "0.5 wt% GO")) |>
  group_by(coating) |>
  mutate(coating = factor(coating, 
                          levels = c("DI Water", "Stock", 
                                     "0.1 wt% GO", "0.2 wt% GO", 
                                     "0.35 wt% GO", "0.5 wt% GO"))) |>
  summarise(
    mean_strength = mean(tensile_strength_n),
    sd_strength = sd(tensile_strength_n),
    .groups = "drop")

# create figure 3.2.1
fig_3.2.1 <- ggplot(summary_3.2.1, aes(x = coating, y = mean_strength, fill = coating)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_strength - sd_strength, ymax = mean_strength + sd_strength),
                width = 0.2, size = 0.5) +
  scale_fill_manual(values = c(
    "DI Water" = "dodgerblue",
    "Stock" = "grey65",
    "0.1 wt% GO" = "seagreen2",
    "0.2 wt% GO" = "seagreen2",
    "0.35 wt% GO" = "seagreen2",
    "0.5 wt% GO" = "seagreen2"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 75)) +
  labs(x = "", y = "Tensile Strength (N)", title = "Machine direction tensile strength of GO-coated HelloFresh sheets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())
