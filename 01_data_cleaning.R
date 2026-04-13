
# Cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)

# clean tensile strength data
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


# clean water absorption data for HelloFresh
absorption_data_HF <- read_excel("HelloFresh_Absorption.xlsx",
                            sheet = "Formatting for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_HF_fig <- read_excel("HelloFresh_Absorption.xlsx",
                            sheet = "Figures for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_HF, file = here("data/absorption_data_HF.rda"))
save(absorption_HF_fig, file = here("data/absorption_HF_fig.rda"))


#clean water absorption data for Metsä
absorption_data_metsa <- read_excel("Metsa_Water_Absorption.xlsx",
                               sheet = "Formatting for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_metsa_fig <- read_excel("Metsa_Water_Absorption.xlsx",
                                   sheet = "Figures for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_metsa, file = here("data/water_data_metsa.rda"))
save(absorption_metsa_fig, file = here("data/water_data_metsa_fig.rda"))

# clean water contact angle data for Metsä
wca_data_metsa <- read_excel("Metsa_Water_ContactAngle.xlsx",
                               sheet = "Formatting for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

save(wca_data_metsa, file = here("data/wca_data_metsa.rda"))

# clean water absorption data for International Paper (IP)
absorption_data_IP <- read_excel("IP_Absorption.xlsx",
                             sheet = "Formatting for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# modifying data for cleaner figures
absorption_IP_fig <- read_excel("IP_Absorption.xlsx",
                                 sheet = "Figures for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_IP, file = here("data/absorption_data_IP.rda"))
save(absorption_IP_fig, file = here("data/absorption_IP_fig.rda"))


