
# Analysis of Data for Tensile Strength - CLaMMP ------------------------------------------------------

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("manuscript/data/tensile_data_small.rda"))
load(here("manuscript/data/tensile_data_large.rda"))

# Statistics for Tensile Strength ClaMMP
tappi_md_anova <- aov(strength_n ~ coating, data = tappi_clammp_md)
summary(tappi_md_anova)

TukeyHSD(tappi_md_anova)
leveneTest(strength_n ~ coating, data = tappi_clammp_md)

small_md_anova <- aov(strength_n ~ coating, data = small_clammp_md)
summary(small_md_anova)

TukeyHSD(small_md_anova)
leveneTest(strength_n ~ coating, data = small_clammp_md)
