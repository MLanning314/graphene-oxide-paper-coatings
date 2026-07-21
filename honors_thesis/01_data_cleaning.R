
# Data Cleaning -------------------------------------------------------------------------------------
# cleaning raw Excel file and making data suitable for analysis

# Load Packages
library(tidyverse)
library(readxl)
library(here)


## Tensile Strength - MxMoonfree ---------------------------------------------------------------------
# clean tensile strength data - mxmoon
tensile_data <- read_excel("data/graphene_oxide_data.xlsx",
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
absorption_data_RF <- read_excel("data/graphene_oxide_data.xlsx",
                            sheet = "RF_WA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_RF_fig <- read_excel("data/graphene_oxide_data.xlsx",
                            sheet = "RF_FIG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_RF, file = here("honors_thesis/data/absorption_data_RF.rda"))
save(absorption_RF_fig, file = here("honors_thesis/data/absorption_RF_fig.rda"))


## Water Absorption - Food-Grade PaperBoard Substrate ------------------------------------------------
# clean water absorption data for Food-Grade PaperBoard (FGPB)
absorption_data_FGPB <- read_excel("data/graphene_oxide_data.xlsx",
                               sheet = "FGPB_WA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# modifying data for cleaner figures
absorption_FGPB_fig <- read_excel("data/graphene_oxide_data.xlsx",
                                   sheet = "FGPB_FIG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_FGPB, file = here("honors_thesis/data/absorption_data_FGPB.rda"))
save(absorption_FGPB_fig, file = here("honors_thesis/data/absorption_FGPB_fig.rda"))


## Water Contact Angle - Food-Grade PaperBoard Substrate ---------------------------------------------
# clean water contact angle data for Food-Grade PaperBoard (FGPB)
wca_data_FGPB <- read_excel("data/graphene_oxide_data.xlsx",
                               sheet = "FGPB_WCA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

save(wca_data_FGPB, file = here("honors_thesis/data/wca_data_FGPB.rda"))


## Water Contact Angle - Virgin Kraft Paper Substrate ------------------------------------------------
# clean water absorption data for Virgin Kraft Paper (VKP)
absorption_data_VKP <- read_excel("data/graphene_oxide_data.xlsx",
                             sheet = "VKP_WA") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating))

# modifying data for cleaner figures
absorption_VKP_fig <- read_excel("data/graphene_oxide_data.xlsx",
                                 sheet = "VKP_FIG") |>
  janitor::clean_names() |>
  mutate(coating = as.factor(coating),
         time = as.factor(time))

# save data
save(absorption_data_VKP, file = here("honors_thesis/data/absorption_data_VKP.rda"))
save(absorption_VKP_fig, file = here("honors_thesis/data/absorption_VKP_fig.rda"))




