
# Handsheet Absorption Analysis

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/handsheet_absorption.rda"))

# AKD coating for water absorption
handsheet_water_AKD <- handsheet_absorption |>
  filter(solution == "Water",
         coating %in% c("H1", "H2", "H3")) |>
  mutate(time = as.factor(time)) 

handsheet_water_AKD_60 <- handsheet_absorption |>
  filter(solution == "Water",
         coating %in% c("H1", "H2", "H3"),
         time == "60") 

handsheet_water_AKD_aov <- aov(absorption ~ coating, data = handsheet_water_AKD)
summary(handsheet_water_AKD_aov)

handsheet_water_AKD_60_aov <- aov(absorption ~ coating, data = handsheet_water_AKD_60)
summary(handsheet_water_AKD_60_aov)

TukeyHSD(handsheet_water_AKD_aov)
TukeyHSD(handsheet_water_AKD_60_aov)

# ASA coating for water absorption 
handsheet_water_ASA <- handsheet_absorption |>
  filter(solution == "Water",
         coating %in% c("H4", "H5", "H6")) |>
  mutate(time = as.factor(time)) 

handsheet_water_ASA_60 <- handsheet_absorption |>
  filter(solution == "Water",
         coating %in% c("H4", "H5", "H6"),
         time == "60") 

handsheet_water_ASA_aov <- aov(absorption ~ coating, data = handsheet_water_ASA)
summary(handsheet_water_ASA_aov)

handsheet_water_ASA_60_aov <- aov(absorption ~ coating, data = handsheet_water_ASA_60)
summary(handsheet_water_ASA_60_aov)

TukeyHSD(handsheet_water_ASA_aov)
TukeyHSD(handsheet_water_ASA_60_aov)

# AKD coating for oil absorption 
handsheet_oil_AKD <- handsheet_absorption |>
  filter(solution == "Oil",
         coating %in% c("H1", "H2", "H3")) |>
  mutate(time = as.factor(time)) 

handsheet_oil_AKD_20 <- handsheet_absorption |>
  filter(solution == "Oil",
         coating %in% c("H1", "H2", "H3"),
         time == "20") 

handsheet_oil_AKD_aov <- aov(absorption ~ coating, data = handsheet_oil_AKD)
summary(handsheet_oil_AKD_aov)

handsheet_oil_AKD_20_aov <- aov(absorption ~ coating, data = handsheet_oil_AKD_20)
summary(handsheet_oil_AKD_20_aov)

TukeyHSD(handsheet_oil_AKD_aov)
TukeyHSD(handsheet_oil_AKD_20_aov)

# ASA coating for oil absorption
handsheet_oil_ASA <- handsheet_absorption |>
  filter(solution == "Oil",
         coating %in% c("H4", "H5", "H6")) |>
  mutate(time = as.factor(time)) 

handsheet_oil_ASA_20 <- handsheet_absorption |>
  filter(solution == "Oil",
         coating %in% c("H4", "H5", "H6"),
         time == "20") 

handsheet_oil_ASA_aov <- aov(absorption ~ coating, data = handsheet_oil_ASA)
summary(handsheet_oil_ASA_aov)

handsheet_oil_ASA_20_aov <- aov(absorption ~ coating, data = handsheet_oil_ASA_20)
summary(handsheet_oil_ASA_20_aov)

TukeyHSD(handsheet_oil_ASA_aov)
TukeyHSD(handsheet_oil_ASA_20_aov)



