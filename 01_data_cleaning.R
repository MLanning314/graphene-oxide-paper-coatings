
# Cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)

# Read in data from Excel
tensile_data <- read_excel("Tensile_Strength_WI2026.xlsx",
                                      sheet = "New Data") |>
  janitor::clean_names() |>
  mutate(
    coating = as.factor(coating),
    paper_type = as.factor(type_of_paper),
    direction = as.factor(direction)
    ) |>
  select(-type_of_paper,
         -date,
         -average,
         -standard_deviation)

# make separate datasets for machine direction and cross direction
tensile_strength_md <- tensile_data |>
  filter(direction == "Machine Direction")

tensile_strength_cd <- tensile_data |>
  filter(direction == "Cross-Machine Direction")

# save data
save(tensile_data, file = here("data/tensile_data.rda"))
save(tensile_strength_md, file = here("data/tensile_strength_md.rda"))
save(tensile_strength_cd, file = here("data/tensile_strength_cd.rda"))


