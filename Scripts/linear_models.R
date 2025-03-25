library(tidyverse)

#source("Scripts//data_load.R")
source("Scripts//airport_comparison.R")

airport_nn_short <- airport_nn %>%
  as.data.frame() %>%
  select(Code, dist, FAC_NAME) %>%
  rename(airport_dist = "dist",
         airport_fac = "FAC_NAME")

ic_bus_nn_short <- ic_bus_nn %>%
  as.data.frame() %>%
  select(Code, dist, FAC_NAME) %>%
  rename(ic_bus_dist = "dist",
         ic_bus_fac = "FAC_NAME")

nn_merge <- merge(airport_nn_short, ic_bus_nn_short, by = "Code")

nn_station_merge <- merge(nn_merge, station_data, by.x = "Code", by.y = "id") %>%
  mutate("cont_rdrs" = (rdrs_24/count)/T_POP)

median_rdrs_night <- nn_station_merge %>%
  filter(on_rt_n == 1) %>%
  group_by(pct_ng_) %>%
  reframe("median" = median(rdrs_24)) %>%
  mutate(pct_ng_ = pct_ng_ * 2)

median_pop_night <- nn_station_merge %>%
  filter(on_rt_n == 1) %>%
  group_by(pct_ng_) %>%
  reframe("median" = median(T_POP)) %>%
  mutate(pct_ng_ = pct_ng_ * 2)

ggsave(plot = ggplot(median_rdrs_night, aes(x = as.factor(pct_ng_), y = median, group = 1)) +
         geom_point(color = "#FFFFFF", size = 3) +
         geom_line(data = median_rdrs_night, aes(as.factor(pct_ng_), y = median), color = "#FFFFFF", size = 2) +
         geom_text(data = median_rdrs_night, aes(x = pct_ng_ + 0.9, y = median - 300, label = round(median, digits = 0)), color = "#FFFFFF") +
         labs(x = "Number of Overnight Stops",
              y = str_wrap("Median Ridership in 2024", width = 10)) +
         ylim(0, 10000) +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "#000000"),
               text = element_text(color = "#FFFFFF"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(color = "#666666"),
               axis.text = element_text(family = "MS Reference Sans Serif",
                                        size = 10,
                                        color = "#FFFFFF"),
               axis.title = element_text(family = "MS Reference Sans Serif",
                                         size = 16,
                                         margin = margin(t = 0, r = 15, l = 0, b = 0),
                                         angle = 0,
                                         vjust = 0.5),
               axis.title.y = element_text(family = "MS Reference Sans Serif",
                                           size = 16,
                                           margin = margin(t = 0, r = 15, l = 0, b = 0),
                                           angle = 0,
                                           vjust = 0.5)),
       "Documents//Exports//median_rdrs_overnight.png",
       width = 3840, height = 2160, units = "px")

ggsave(plot = ggplot(median_pop_night, aes(x = as.factor(pct_ng_), y = median, group = 1)) +
         geom_point(color = "#FFFFFF", size = 3) +
         geom_line(data = median_rdrs_night, aes(as.factor(pct_ng_), y = median), color = "#FFFFFF", size = 2) +
         geom_text(data = median_rdrs_night, aes(x = pct_ng_ + 0.8, y = median, label = round(median, digits = 0)), color = "#FFFFFF") +
         labs(x = "Number of Overnight Stops",
              y = str_wrap("Median Number of People Living Within a 30 Minute Drive in 2024", width = 15)) +
         ylim(50000, 200000) +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "#000000"),
               text = element_text(color = "#FFFFFF"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(color = "#666666"),
               axis.text = element_text(family = "MS Reference Sans Serif",
                                        size = 10,
                                        color = "#FFFFFF"),
               axis.title = element_text(family = "MS Reference Sans Serif",
                                         size = 16,
                                         margin = margin(t = 0, r = 15, l = 0, b = 0),
                                         angle = 0,
                                         vjust = 0.5),
               axis.title.y = element_text(family = "MS Reference Sans Serif",
                                           size = 16,
                                           margin = margin(t = 0, r = 15, l = 0, b = 0),
                                           angle = 0,
                                           vjust = 0.5)),
       "Documents//Exports//median_pop_overnight.png",
       width = 3840, height = 2160, units = "px")



# merge(nn_station_merge, amtrak_stations %>% select(Code, geometry), by = "Code") %>%
#   st_write(., "Data//station_characteristics.shp", append = FALSE)

ggplot(median_rdrs_night, aes(x = as.factor(pct_ng_), y = median, group = 1)) +
  geom_point(color = "#FFFFFF", size = 3) +
  geom_line(data = median_rdrs_night, aes(as.factor(pct_ng_), y = median), color = "#FFFFFF", size = 2) +
  geom_text(data = median_rdrs_night, aes(x = pct_ng_ + 0.9, y = median - 300, label = round(median, digits = 0)), color = "#FFFFFF") +
  labs(x = "Number of Overnight Stops",
       y = str_wrap("Median Ridership in 2024", width = 10)) +
  ylim(0, 10000) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#000000"),
        text = element_text(color = "#FFFFFF"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#666666"),
        axis.text = element_text(family = "MS Reference Sans Serif",
                                 size = 10,
                                 color = "#FFFFFF"),
        axis.title = element_text(family = "MS Reference Sans Serif",
                                  size = 16,
                                  margin = margin(t = 0, r = 15, l = 0, b = 0),
                                  angle = 0,
                                  vjust = 0.5),
        axis.title.y = element_text(family = "MS Reference Sans Serif",
                                    size = 16,
                                    margin = margin(t = 0, r = 15, l = 0, b = 0),
                                    angle = 0,
                                    vjust = 0.5))

ggsave(plot = ggplot(nn_station_merge %>% filter(on_rt_n == 1), aes(x = as.factor(pct_ng_ * 2), y = cont_rdrs)) +
         geom_boxplot(outlier.color = "#FFFFFF", outlier.size = 3, 
                      color = "#FFFFFF", fill = "#262626",
                      linewidth = 1.25) +
         labs(x = "Number of Overnight Stops",
              y = str_wrap("2024 Ridership, Controlled for Number of Trains and Surrounding Population", width = 10)) +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "#000000"),
               text = element_text(color = "#FFFFFF"),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_line(color = "#666666"),
               axis.text = element_text(family = "MS Reference Sans Serif",
                                        size = 10,
                                        color = "#FFFFFF"),
               axis.title = element_text(family = "MS Reference Sans Serif",
                                         size = 16,
                                         margin = margin(t = 0, r = 15, l = 0, b = 0),
                                         angle = 0,
                                         vjust = 0.5),
               axis.title.y = element_text(family = "MS Reference Sans Serif",
                                           size = 16,
                                           margin = margin(t = 0, r = 15, l = 0, b = 0),
                                           angle = 0,
                                           vjust = 0.5)),
       "Documents//Exports//rdrs_overnight_boxplot.png",
       width = 3840, height = 2160, units = "px")

nn_station_merge %>%
  filter(on_rt_n == 1) %>%
  lm(data = ., formula = cont_rdrs ~ pct_ng_) %>% summary()
