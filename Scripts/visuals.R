library(tidyverse)

ggplot(nn_station_merge, aes(x = count, y = rdrs_24)) +
  geom_point(color = "#FFFFFF") +
  labs(title = "Ridership and Train Frequency",
       x = "Number of Trains Daily",
       y = "Ridership in 2024") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#000000"),
        text = element_text(color = "#FFFFFF"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "MS Reference Sans Serif",
                                  size = 24,
                                  color = "#FFFFFF"),
        axis.text = element_text(family = "MS Reference Sans Serif",
                                   size = 10,
                                   color = "#FFFFFF"),
        axis.title = element_text(family = "MS Reference Sans Serif",
                                    size = 16,
                                    margin = margin(t = 0, r = 15, l = 0, b = 0),
                                    angle = 0,
                                    vjust = 0.5))