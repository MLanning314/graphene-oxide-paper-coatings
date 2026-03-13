
# Analysis of Data for Water Absorption

# Load Packages 
library(tidyverse)
library(here)

# Load Data
load(here("data/water_data_HF.rda"))

# formatting for figure 4.1.1

hf_1 <- water_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO")) |>
  group_by(time, coating) |>
  summarise(mean_raw = mean(raw_weight, na.rm = TRUE),
            sd_raw = sd(raw_weight, na.rm = TRUE),
            .groups = "drop")


ggplot(hf_1, aes(x = time, y = mean_raw,
                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_raw - sd_raw,
                    ymax = mean_raw + sd_raw),
                width = 2, linewidth = 0.6) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,     # filled circle
               "0.1% GO" = 17)      # filled triangle
  ) +
  labs(
    title = "HelloFresh",
    x = expression(italic(time)~"(min)"),
    y = "absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(face = "italic"),
    legend.position = c(0.75, 0.25),
    legend.background = element_blank()
  )
