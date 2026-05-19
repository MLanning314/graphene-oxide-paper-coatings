
# Analysis of Distance for Tensile Strength - CLaMMP

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load data
load(here("data/distance_data_clammp.rda"))

# convert displacement/time to stress/strain

speed <- 0.33          # mm/sec
gauge_length <- 150    # mm
width <- 25            # mm
thickness <- 0.25      # mm

area <- width * thickness

distance_data_clammp <- distance_data_clammp |>
  mutate(displacement = time * speed)








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
