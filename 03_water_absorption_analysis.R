
# Analysis of Data for Water Absorption

# Load Packages 
library(tidyverse)
library(here)

# Load Data
load(here("data/water_data_HF.rda"))

# formatting for figure 4.1.1

hf_1 <- water_data_HF |>
  filter(coating %in% c("DI Water", "0.1% GO", "15% WBBC")) |>
  group_by(time, coating) |>
  summarise(mean_raw = mean(raw_weight, na.rm = TRUE),
            sd_raw = sd(raw_weight, na.rm = TRUE),
            .groups = "drop")


hf_1_img <- ggplot(hf_1, aes(x = time, y = mean_raw,
                 color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_raw - sd_raw,
                    ymax = mean_raw + sd_raw),
                width = 1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "0.1% GO" = "firebrick",
               "15% WBBC" = "goldenrod1")
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,     
               "0.1% GO" = 17,
               "15% WBBC" = 15)      
  ) +
  labs(
    title = "DI water vs. 0.1% GO water absorption of HelloFresh",
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "plain"),
    legend.position = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/hf_1_img.png", plot = hf_1_img,
       width = 8, height = 5, units = "in", dpi = 600)
