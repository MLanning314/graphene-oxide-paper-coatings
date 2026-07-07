
# Data Cleaning -------------------------------------------------------------------------------------
# cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)


## Tensile Strength - MxMoonfree ---------------------------------------------------------------------
# clean tensile strength data - mxmoon
tensile_data <- read_excel("graphene_oxide_data.xlsx",
                                      sheet = "RF_TSMX") |>
  janitor::clean_names() |>
  mutate(
    coating = as.factor(coating),
    direction = as.factor(direction)
    ) |>
  select(-type_of_paper,
         -date_m_d_yy)

# make separate datasets for machine direction and cross direction
tensile_strength_md <- tensile_data |>
  filter(direction == "Machine Direction")

tensile_strength_cd <- tensile_data |>
  filter(direction == "Cross-Machine Direction")

# save data
save(tensile_strength_md, file = here("honors_thesis/data/tensile_strength_md.rda"))
save(tensile_strength_cd, file = here("honors_thesis/data/tensile_strength_cd.rda"))


## Water Absorption - Recycled Paper Substrate -------------------------------------------------------
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
save(absorption_data_RF, file = here("honors_thesis/data/absorption_data_RF.rda"))
save(absorption_RF_fig, file = here("honors_thesis/data/absorption_RF_fig.rda"))


## Water Absorption - Food-Grade PaperBoard Substrate ------------------------------------------------
# clean water absorption data for Food-Grade PaperBoard (FGPB)
absorption_data_FGPB <- read_excel("graphene_oxide_data.xlsx",
                               sheet = "FGPB_WA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_FGPB_fig <- read_excel("graphene_oxide_data.xlsx",
                                   sheet = "FGPB_FIG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_FGPB, file = here("honors_thesis/data/absorption_data_FGPB.rda"))
save(absorption_FGPB_fig, file = here("honors_thesis/data/absorption_FGPB_fig.rda"))

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




