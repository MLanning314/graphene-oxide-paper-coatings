
# Data Cleaning --------------------------------------------------------------------------------------
# cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)


## Water Absorption - Recycled Paper Substrate --------------------------------------------------------
# clean water absorption data for Recycled Paper (RF)
absorption_data_RF <- read_excel("graphene_oxide_data.xlsx",
                                 sheet = "RF_WA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_RF_fig <- read_excel("graphene_oxide_data.xlsx",
                                sheet = "RF_FIG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_RF, file = here("manuscript/data/absorption_data_RF.rda"))
save(absorption_RF_fig, file = here("manuscript/data/absorption_RF_fig.rda"))


## Tensile Strength - Recycled Paper Substrate and Handsheets -----------------------------------------
# clean tensile strength data for Recycled Paper (RF) and Handsheets
tensile_data_clammp <- read_excel("graphene_oxide_data.xlsx",
                                sheet = "TS_CL") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# create dataset for small Recycled Paper samples
tensile_data_small <- tensile_data_clammp |>
  filter(size == "Small") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# create dataset for larger Recycled Paper samples (TAPPI standard)
tensile_data_large <- tensile_data_clammp |>
  filter(size == "TAPPI") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# create dataset for handsheet samples
tensile_data_handsheet <- tensile_data_clammp |>
  filter(paper_type == "Greif") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# save data
save(tensile_data_small, file = here("manuscript/data/tensile_data_small.rda"))
save(tensile_data_large, file = here("manuscript/data/tensile_data_large.rda"))
save(tensile_data_handsheet, file = here("manuscript/data/tensile_data_handsheet.rda"))


## Force Displacement Curves - Recycled Paper Substrate and Handsheets --------------------------------
# clean force/displacement graph (FDG) data for tensile testing on CLaMMP
FDG_curves_clammp <- read_excel("graphene_oxide_data.xlsx",
                                   sheet = "TS_FDG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# save data
save(FDG_curves_clammp, file = here("manuscript/data/FDG_curves_clammp.rda"))


## Tensile and Burst Index for Handsheets -------------------------------------------------------------
# examine tensile and burst index from Jason (WMU)
tensile_burst_index <- read_excel("graphene_oxide_data.xlsx",
                                   sheet = "TBI_WMU") |>
  janitor::clean_names() |>
  mutate(condition = as.factor(condition))

# save data
save(tensile_burst_index, file = here("manuscript/data/tensile_burst_index.rda"))




