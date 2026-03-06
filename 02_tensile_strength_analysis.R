
# load packages 

# machine direction anova
md_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_md)
summary(md_anova)

TukeyHSD(md_anova)

# cd anova
cd_anova <- aov(tensile_strength_n ~ coating, data = tensile_strength_cd)
summary(cd_anova)
