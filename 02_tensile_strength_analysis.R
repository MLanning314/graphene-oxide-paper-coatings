
# Analysis of Data for Tensile Strength

# Load Packages 
library(tidyverse)
library(here)

# Load data
load(here("data/tensile_strength_md.rda"))
load(here("data/tensile_strength_cd.rda"))

# machine direction anova
md_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_md)
summary(md_anova)

TukeyHSD(md_anova)

# cd anova
cd_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_cd)
summary(cd_anova)
