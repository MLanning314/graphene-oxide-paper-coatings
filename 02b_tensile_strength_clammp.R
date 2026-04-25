
# Analysis of Data for Tensile Strength - CLaMMP

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/tensile_data_clammp.rda"))
load(here("data/tappi_clammp_md.rda"))
load(here("data/small_clammp_md.rda"))

# statistics
tappi_md_anova <- aov(strength_n ~ coating, data = tappi_clammp_md)
summary(tappi_md_anova)

TukeyHSD(tappi_md_anova)

small_md_anova <- aov(strength_n ~ coating, data = small_clammp_md)
summary(small_md_anova)

TukeyHSD(small_md_anova)
