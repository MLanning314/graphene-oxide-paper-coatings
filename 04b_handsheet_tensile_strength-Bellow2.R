
# Tensile Strength Analysis for Handsheets

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/tensile_data_clammp.rda"))
load(here("data/distance_data_clammp.rda"))

# filter out handsheet data
handsheet_strength <- tensile_data_clammp |>
  filter(paper_type == "Greif")

# tensile strength of handsheet samples
handsheet_strength_aov <- aov(strength_n ~ coating, data = handsheet_strength)
summary(handsheet_strength_aov)

TukeyHSD(handsheet_strength_aov)
leveneTest(strength_n ~ coating, data = handsheet_strength)
