
# Analysis of Data for Tensile Strength - CLaMMP ------------------------------------------------------

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("manuscript/data/tensile_data_small.rda"))
load(here("manuscript/data/tensile_data_large.rda"))

# Filter out Cross-Machine Direction and Machine Direction
large_anova_cd <- tensile_data_large |>
  filter(direction == "Cross-Machine")

# Statistics for Tensile Strength ClaMMP
large_anova <- aov(strength_n ~ coating, data = large_anova_cd)
summary(large_anova)

TukeyHSD(large_anova)
leveneTest(strength_n ~ coating, data = tappi_clammp_md)

small_md_anova <- aov(strength_n ~ coating, data = small_clammp_md)
summary(small_md_anova)

TukeyHSD(small_md_anova)
leveneTest(strength_n ~ coating, data = small_clammp_md)
