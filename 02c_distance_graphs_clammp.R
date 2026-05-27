
# Analysis of Distance for Tensile Strength - CLaMMP

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/distance_data_clammp.rda"))

# convert displacement/time to stress/strain
speed <- 0.33 # mm/sec
width <- 25 # mm
gauge_length <- 150 # mm

distance_data_clammp <- distance_data_clammp |>
  mutate(
    displacement = time * speed,
    area = width * thickness,
    stress = tensile_strength / area,
    strain = displacement / gauge_length)

# define fitting regions
region_results <- distance_data_clammp |>
  group_by(sample, coating) |>
  group_modify(~{
    
    # isolate one specimen
    dat <- .x
    
    # maximum strain reached before failure
    max_strain <- max(dat$strain,
                      na.rm = TRUE)
    
    # 0-15% region
    region_0_15 <- dat |>
      filter(strain <= 0.15 * max_strain)
    
    # 15-50% region
    region_15_50 <- dat |>
      filter(
        strain > 0.15 * max_strain,
        strain <= 0.50 * max_strain
      )
    
    # save regions in list
    region_list <- list(
      pct_0_15 = region_0_15,
      pct_15_50 = region_15_50
    )
    
    # fit models to get slope
    map_dfr(names(region_list), function(reg){
      subdat <- region_list[[reg]]
      if(nrow(subdat) < 5){
        
        return(
          tibble(
            region = reg,
            modulus = NA_real_,
            intercept = NA_real_,
            r2 = NA_real_,
            n_points = nrow(subdat)
          )
        )
      }
      
      # linear fit model
      fit <- lm(stress ~ strain,
                data = subdat)
      
      # extract results in a tibble
      tibble(
        region = reg,
        modulus = coef(fit)[2],
        intercept = coef(fit)[1],
        r2 = summary(fit)$r.squared,
        n_points = nrow(subdat)
      )
    })
    
  }) |>
  
  ungroup()


# average slope and r2 by coating 
region_summary <- region_results |>
  group_by(sample, region, coating) |>
  summarise(
    mean_modulus =
      mean(modulus, na.rm = TRUE),
    mean_r2 =
      mean(r2, na.rm = TRUE),
    .groups = "drop"
  )


# anova and statistics

anova_results <- region_results |>
  group_by(region) |>
  group_modify(~{
    fit <- aov(modulus ~ coating,
               data = .x)
    broom::tidy(fit)
  })

tukey_results <- region_results |>
  group_by(region) |>
  group_modify(~{
    fit <- aov(modulus ~ coating,
               data = .x)
    TukeyHSD(fit)$coating |>
      as.data.frame() |>
      rownames_to_column("comparison")
  })



# ==================================================
# Plot modulus distributions
# ==================================================

ggplot(region_results,
       aes(coating, modulus)) +
  
  geom_boxplot() +
  
  geom_jitter(width = 0.1,
              alpha = 0.7) +
  
  facet_wrap(~region,
             scales = "free_y") +
  
  theme_bw() +
  
  labs(
    x = "Coating",
    y = "Elastic Modulus (MPa)",
    title = "Stress-Strain Modulus by Region"
  )



# calculate maximum displacement
max_displacement <- distance_data_clammp |>
  group_by(sample, coating) |>
  group_modify(~{
    dat <- .x
    max_disp <- max(dat$displacement, na.rm = TRUE)
    tibble(
      max_disp = max_disp
    )})












# define fitting regions
# initial region (toes)
region_initial <- dat |>
  filter(displacement <= 0.05 * max_disp)

library(tidyverse)
library(broom)

region_results <- distance_data_clammp |>
  
  group_by(sample, coating) |>
  
  group_modify(~{
    
    dat <- .x
    
    # maximum displacement for this specimen
    max_disp <- max(dat$displacement, na.rm = TRUE)
    
    # ----------------------------------------
    # REGION 1: Initial region
    # (example = first 5%)
    # ----------------------------------------
    
    region_initial <- dat |>
      filter(displacement <= 0.05 * max_disp)
    
    # ----------------------------------------
    # REGION 2: 0–15%
    # ----------------------------------------
    
    region_0_15 <- dat |>
      filter(displacement <= 0.15 * max_disp)
    
    # ----------------------------------------
    # REGION 3: 15–50%
    # ----------------------------------------
    
    region_15_50 <- dat |>
      filter(
        displacement > 0.15 * max_disp,
        displacement <= 0.50 * max_disp
      )
    
    # ----------------------------------------
    # Put regions into list
    # ----------------------------------------
    
    region_list <- list(
      initial = region_initial,
      pct_0_15 = region_0_15,
      pct_15_50 = region_15_50
    )
    
    # ----------------------------------------
    # Fit models to each region
    # ----------------------------------------
    
    map_dfr(names(region_list), function(reg){
      
      subdat <- region_list[[reg]]
      
      # prevent crashes if too few points
      if(nrow(subdat) < 5){
        
        return(
          tibble(
            region = reg,
            slope = NA_real_,
            intercept = NA_real_,
            r2 = NA_real_
          )
        )
      }
      
      fit <- lm(force ~ displacement,
                data = subdat)
      
      tibble(
        region = reg,
        slope = coef(fit)[2],
        intercept = coef(fit)[1],
        r2 = summary(fit)$r.squared
      )
    })
  }) |>
  
  ungroup()
