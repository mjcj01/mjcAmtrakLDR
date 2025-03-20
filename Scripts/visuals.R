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
  mutate(nght_s_ = ifelse(id == "SPK", 2,
                   ifelse(id == "SAS", 2,
                          nght_s_))) %>%
  group_by(nght_s_) %>%
  reframe(late_avg = mean(late),
          min_late_avg = mean(min_late),
          not_late_avg = mean(not_late))

colors_night <- c("2" = "#FFC34D",
                  "1" = "#9B6F17",
                  "0" = "#332200")

ggsave(station_data %>%
         filter(on_rt_n == 1) %>%
         mutate(nght_s_ = ifelse(id == "SPK", 2,
                                 ifelse(id == "SAS", 2,
                                        nght_s_)),
                "cont_rdrs" = (rdrs_24/count)/T_POP) %>%
         group_by(nght_s_) %>%
         mutate("count_fct" = as.factor(nght_s_)) %>%
         ggplot() +
         geom_sf(aes(color = count_fct, size = cont_rdrs)) +
         scale_color_manual(values = colors_night) +
         guides(color = guide_legend(title = str_wrap("Number of Overnight Stops",
                                                      width = 15))) +
         theme_void() +
         theme(plot.background = element_rect(fill = "#000000"),
               legend.title = element_text(family = "MS Reference Sans Serif",
                                           color = "#FFFFFF",
                                           size = 24),
               legend.text = element_text(family = "MS Reference Sans Serif",
                                          color = "#FFFFFF",
                                          size = 12)),
       filename = "Documents//Exports//overnight_stop_map.png",
       width = 3840, height = 2160, units = "px")

ggsave(ggplot(data = nn_station_merge, aes(x = ic_bus_dist, y = cont_rdrs)) +
         geom_point(color = "#FFFFFF") +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "#000000"),
               text = element_text(color = "#FFFFFF"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(color = "#808080"),
               plot.title = element_text(family = "MS Reference Sans Serif",
                                         size = 24,
                                         color = "#FFFFFF"),
               axis.text.x = element_text(family = "MS Reference Sans Serif",
                                          size = 10,
                                          color = "#FFFFFF"),
               axis.text.y = element_blank(),
               axis.title = element_text(family = "MS Reference Sans Serif",
                                         size = 16,
                                         margin = margin(t = 0, r = 15, l = 0, b = 0),
                                         angle = 0,
                                         vjust = 0.5)),
       filename = "Documents//Exports//cont_rdrs_ic_bus_dist_plot.png",
       width = 3840, height = 2160, units = "px")

nn_station_merge %>% 
  st_as_sf() %>% 
  mutate("var" = late - mean(late)) %>% 
  ggplot(.) + 
  geom_sf(aes(color = var)) + 
  scale_size_continuous(range = c(0.0625, 6)) + 
  scale_color_fermenter(type = "div", breaks = c(-.25, -.15, -.05, .05, .15, .25)) +
  guides(color = guide_legend(title = str_wrap("Pct. Pt. Distance from Mean Delay %",
                                               width = 15))) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#000000"),
        text = element_text(color = "#FFFFFF"),
        legend.title = element_text(family = "MS Reference Sans Serif",
                                    color = "#FFFFFF",
                                    size = 24),
        legend.text = element_text(family = "MS Reference Sans Serif",
                                   color = "#FFFFFF",
                                   size = 12))
