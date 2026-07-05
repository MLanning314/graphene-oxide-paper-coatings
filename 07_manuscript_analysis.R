
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

fit_oneway_WA <- aov(absorption ~ coating, data = WA_anova)
summary(fit_oneway_WA)

TukeyHSD(fit_oneway_WA, "coating")

# Tensile Strength

# larger sample strips
tappi_md_anova <- aov(strength_n ~ coating, data = tappi_clammp_md)
summary(tappi_md_anova)

TukeyHSD(tappi_md_anova)
leveneTest(strength_n ~ coating, data = tappi_clammp_md)
