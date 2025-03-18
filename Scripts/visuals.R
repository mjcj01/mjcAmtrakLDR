library(tidyverse)

ggsave(plot = ggplot(nn_station_merge, aes(x = count, y = rdrs_24)) +
         geom_point(color = "#FFFFFF") +
         labs(title = "Ridership and Train Frequency",
              x = "Number of Trains Daily",
              y = "Ridership in 2024") +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "#000000"),
               text = element_text(color = "#FFFFFF"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(color = "#808080"),
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
                                         vjust = 0.5)),
       filename = "Documents//Exports//rdrs_freq_plot.png",
       width = 3840, height = 2160, units = "px")

station_data %>%
  filter(on_rt_n == 1) %>%
  mutate(nght_s_ = ifelse(id == "SPK", 2, nght_s_)) %>%
  group_by(nght_s_) %>%
  reframe(late_avg = mean(late),
          min_late_avg = mean(min_late),
          not_late_avg = mean(not_late))