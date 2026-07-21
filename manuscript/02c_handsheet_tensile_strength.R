
# Tensile Strength Analysis for Handsheets ----

## Load Packages ----
library(tidyverse)
library(here)
library(car)

## Load Data ----
load(here("manuscript/data/tensile_data_clammp.rda"))
load(here("manuscript/data/FDG_curves_clammp.rda"))
load(here("manuscript/data/tensile_burst_index.rda"))

## Clean Handsheet-specific Data ----
handsheet_strength <- tensile_data_clammp |>
  filter(paper_type == "Greif")

### AKD Samples ----
handsheet_strength_akd <- tensile_burst_index |>
  filter(condition %in% c("H1", "H2", "H3"))

### ASA samples ----
handsheet_strength_asa <- tensile_burst_index |>
  filter(condition %in% c("H4", "H5", "H6"))

## Analysis for Tensile Strength of Handsheet Samples ----
handsheet_strength_aov <- aov(strength_n ~ coating, data = handsheet_strength)
summary(handsheet_strength_aov)

TukeyHSD(handsheet_strength_aov)
leveneTest(strength_n ~ coating, data = handsheet_strength)

## Analysis for Tensile Index of Handsheet Samples ----
tensile_index <- aov(tensile_index ~ condition, data = tensile_burst_index)
summary(tensile_index)
TukeyHSD(tensile_index)

# AKD Samples
tensile_index_akd <- aov(tensile_index ~ condition, data = handsheet_strength_akd)
summary(tensile_index_akd)
TukeyHSD(tensile_index_akd)

# ASA Samples
tensile_index_asa <- aov(tensile_index ~ condition, data = handsheet_strength_asa)
summary(tensile_index_asa)
TukeyHSD(tensile_index_asa)

## Analysis for Burst Index of Handsheet Samples ----
burst_index <- aov(burst_index ~ condition, data = tensile_burst_index)
summary(burst_index)
TukeyHSD(burst_index)

# AKD Samples
burst_index_akd <- aov(burst_index ~ condition, data = handsheet_strength_akd)
summary(burst_index_akd)
TukeyHSD(burst_index_akd)

# ASA Samples
burst_index_asa <- aov(burst_index ~ condition, data = handsheet_strength_asa)
summary(burst_index_asa)
TukeyHSD(burst_index_asa)

