
# Statistical Analysis for the Manuscript

# Load Packages
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_HF.rda"))
load(here("data/tensile_data_clammp.rda"))
load(here("data/tappi_clammp_md.rda"))
load(here("data/small_clammp_md.rda"))

# Water Absorption (WA)

WA_anova <- absorption_data_HF |>
  mutate(time = as.factor(time)) 

WA_anova_60 <- absorption_data_HF |>
  filter(time == "60") |>
  mutate(time = as.factor(time)) 

WA_anova_30 <- absorption_data_HF |>
  mutate(time = as.numeric(as.character(time))) |>
  filter(time <= 30) |>
  mutate(time = factor(time))

fit_oneway_WA <- aov(absorption ~ coating, data = WA_anova)
summary(fit_oneway_WA)

fit_oneway_WA_30 <- aov(absorption ~ coating, data = WA_anova_30)
summary(fit_oneway_WA_30)

fit_oneway_WA_60 <- aov(absorption ~ coating, data = WA_anova_60)
summary(fit_oneway_WA_60)

TukeyHSD(fit_oneway_WA, "coating")
TukeyHSD(fit_oneway_WA_60, "coating")

# Tensile Strength

# larger sample strips
tappi_md_anova <- aov(strength_n ~ coating, data = tappi_clammp_md)
summary(tappi_md_anova)

TukeyHSD(tappi_md_anova)
leveneTest(strength_n ~ coating, data = tappi_clammp_md)
