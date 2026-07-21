
# Analysis of Data for Tensile Strength - CLaMMP ------------------------------------------------------

## Load Packages ----
library(tidyverse)
library(here)
library(car)

## Load Data ----
load(here("manuscript/data/tensile_data_small.rda"))
load(here("manuscript/data/tensile_data_large.rda"))

## Filter out Cross-Machine Direction and Machine Direction ----
large_anova_cd <- tensile_data_large |>
  filter(direction == "Cross-Machine")

large_anova_md <- tensile_data_large |>
  filter(direction == "Machine")

small_anova_cd <- tensile_data_small |>
  filter(direction == "Cross-Machine")

small_anova_md <- tensile_data_small |>
  filter(direction == "Machine")

## Statistics for Tensile Strength ClaMMP ----

### Cross-Machine Direction Analysis ----
aov_cd_large <- aov(strength_n ~ coating, data = large_anova_cd)
summary(aov_cd_large)

TukeyHSD(aov_cd_large)
leveneTest(strength_n ~ coating, data = large_anova_cd)

### Machine Direction Analysis ----
# tappi standard samples
aov_md_large <- aov(strength_n ~ coating, data = large_anova_md)
summary(aov_md_large)

TukeyHSD(aov_md_large)
leveneTest(strength_n ~ coating, data = large_anova_md)

# smaller sized samples
aov_md_small <- aov(strength_n ~ coating, data = small_anova_md)
summary(aov_md_small)

TukeyHSD(aov_md_small)
leveneTest(strength_n ~ coating, data = small_anova_md)
