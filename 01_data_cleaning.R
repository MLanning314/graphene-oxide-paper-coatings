
# Data Cleaning ----
# cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)

# --------------------------------------------------------------------------
# clean tensile strength data - clammp
tensile_data_clammp <- read_excel("tensile_strength_clammp.xlsx",
                           sheet = "Sheet1") |>
  janitor::clean_names() |>
  mutate(
    coating = as.factor(coating),
    paper_type = as.factor(paper_type),
    direction = as.factor(direction),
    size = as.factor(size)
  )

# make separate datasets for machine direction and cross direction
tappi_clammp_md <- tensile_data_clammp |>
  filter(direction == "Machine",
         size == "TAPPI")

small_clammp_md <- tensile_data_clammp |>
  filter(direction == "Machine",
         size == "Small")

# save data
save(tensile_data_clammp, file = here("manuscript/data/tensile_data_clammp.rda"))
save(tappi_clammp_md, file = here("manuscript/data/tappi_clammp_md.rda"))
save(small_clammp_md, file = here("manuscript/data/small_clammp_md.rda"))




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
save(absorption_data_metsa, file = here("data/absorption_data_metsa.rda"))
save(absorption_metsa_fig, file = here("data/absorption_metsa_fig.rda"))

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

# clean distance graph data for tensile testing on CLaMMP
distance_data_clammp <- read_excel("distance_graphs_clammp.xlsx",
                                   sheet = "Sheet1") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# save data
save(distance_data_clammp, file = here("data/distance_data_clammp.rda"))

# import handsheet data
handsheet_absorption <- read_excel("greif_handsheet_absorption.xlsx",
                                   sheet = "Formatting for R") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# save data
save(handsheet_absorption, file = here("data/handsheet_absorption.rda"))

# examine tensile and burst index from myself and Jason (WMU)
tensile_burst_index <- read_excel("tensile_burst_index.xlsx",
                                   sheet = "Sheet1") |>
  janitor::clean_names() |>
  mutate(condition = as.factor(condition))

# save data
save(tensile_burst_index, file = here("data/tensile_burst_index.rda"))




