
# Tensile Strength Analysis for Handsheets

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/tensile_data_clammp.rda"))
load(here("data/distance_data_clammp.rda"))
load(here("data/tensile_burst_index.rda"))

# filter out handsheet data
handsheet_strength <- tensile_data_clammp |>
  filter(paper_type == "Greif")

# AKD samples
handsheet_strength_akd <- tensile_burst_index |>
  filter(condition %in% c("H1", "H2", "H3"))

# ASA samples
handsheet_strength_asa <- tensile_burst_index |>
  filter(condition %in% c("H4", "H5", "H6"))

# tensile strength of handsheet samples
handsheet_strength_aov <- aov(strength_n ~ coating, data = handsheet_strength)
summary(handsheet_strength_aov)

TukeyHSD(handsheet_strength_aov)
leveneTest(strength_n ~ coating, data = handsheet_strength)

# tensile index statistics
tensile_index_j <- aov(tensile_index_j ~ condition, data = tensile_burst_index)
summary(tensile_index_j)
TukeyHSD(tensile_index_j)

tensile_index_akd_j <- aov(tensile_index_j ~ condition, data = handsheet_strength_akd)
summary(tensile_index_akd_j)
TukeyHSD(tensile_index_akd_j)

tensile_index_asa_j <- aov(tensile_index_j ~ condition, data = handsheet_strength_asa)
summary(tensile_index_asa_j)
TukeyHSD(tensile_index_asa_j)

tensile_index_m <- aov(tensile_index_m ~ condition, data = tensile_burst_index)
summary(tensile_index_m)
TukeyHSD(tensile_index_m)

burst_index <- aov(burst_index_j ~ condition, data = tensile_burst_index)
summary(burst_index)
TukeyHSD(burst_index)
