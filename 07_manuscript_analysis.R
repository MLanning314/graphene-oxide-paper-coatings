
# Statistical Analysis for the Manuscript

# Load Packages
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_HF.rda"))

# Water Absorption (WA)
# anova for DI water, 0.1% GO, 0.2% GO, 0.5% GO, and stock

WA_anova <- absorption_data_HF |>
  filter(coating %in% c("DI Water", "Stock", "0.1% GO", "0.20% GO", 
                        "0.5% GO", "15% WBBC")) |>
  mutate(time = as.factor(time)) 

fit_oneway_WA <- aov(absorption ~ coating, data = WA_anova)
summary(fit_oneway_WA)

TukeyHSD(fit_oneway_WA, "coating")


