# Analysis of Data for Water Absorption - International Paper

# Load Packages 
library(tidyverse)
library(here)
library(car)

# Load Data
load(here("data/absorption_data_IP.rda"))
load(here("data/absorption_IP_fig.rda"))

# formatting for figure

IP_1_fig <- absorption_IP_fig |>
  filter(coating %in% c("DI Water", "0.1% GO", "Stock",
                        "25% WBBC")) |>
  mutate(time = as.numeric(time),
         coating = dplyr::recode(coating,
                                 `25% WBBC` = "9.75% Joncryl"),
         coating = factor(coating,
           levels = c("Stock", "DI Water", "0.1% GO", "9.75% Joncryl")))

fig_4.3.1 <- ggplot(IP_1_fig, aes(x = time, y = absorption,
                                     color = coating, shape = coating)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = absorption - rms,
                    ymax = absorption + rms),
                width = 0.1, linewidth = 0.5) +
  scale_color_manual(
    values = c("DI Water" = "dodgerblue",
               "Stock" = "grey65",
               "0.1% GO" = "firebrick",
               "9.75% Joncryl" = "goldenrod1"
    )
  ) +
  scale_shape_manual(
    values = c("DI Water" = 16,
               "0.1% GO" = 17,
               "Stock" = 15,
               "9.75% Joncryl" = 18)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  labs(
    x = "Time (min)",
    y = "Absorbed water weight / unit dry paper weight (gm/gm)",
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "plain"),
    legend.position.inside = c(0.75, 0.25),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

ggsave("figures/fig_4.3.1.png", plot = fig_4.3.1,
       width = 10, height = 6, units = "in", dpi = 600)
