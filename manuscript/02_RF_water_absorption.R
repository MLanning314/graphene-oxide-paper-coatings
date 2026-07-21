
# Analysis of Data for Recycled Fiber Water Absorption ----

## Load Packages ----
library(tidyverse)
library(here)
library(car)

## Load Data ----
load(here("manuscript/data/absorption_data_RF.rda"))

## Statistics and Analysis ----

### Overall Anova for Performance ----
WA_anova <- absorption_data_RF |>
  mutate(time = as.factor(time)) 

fit_oneway_WA <- aov(absorption ~ coating, data = WA_anova)
summary(fit_oneway_WA)

TukeyHSD(fit_oneway_WA, "coating")


### Anova at only the 30-minute time period ----
WA_anova_30 <- absorption_data_RF |>
  mutate(time = as.numeric(as.character(time))) |>
  filter(time <= 30) |>
  mutate(time = factor(time))

fit_oneway_WA_30 <- aov(absorption ~ coating, data = WA_anova_30)
summary(fit_oneway_WA_30)

TukeyHSD(fit_oneway_WA_30, "coating")


### Anova at only the 60-minute time period ----
WA_anova_60 <- absorption_data_RF |>
  filter(time == "60") |>
  mutate(time = as.factor(time)) 

fit_oneway_WA_60 <- aov(absorption ~ coating, data = WA_anova_60)
summary(fit_oneway_WA_60)

TukeyHSD(fit_oneway_WA_60, "coating")

