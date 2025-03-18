library(tidyverse)
library(sf)
library(nngeo)
library(extrafont)
# font_import()
loadfonts(device = "win")

source("Scripts//data_load.R")

p_airports <- ipcd %>%
  filter(FAC_TYPE == 1) %>%
  mutate("index" = 1:n())

airport_nn <- st_nn(amtrak_stations %>% filter(Code %in% delay_data_pct$StationCode), 
                   p_airports, 
                   returnDist = TRUE, 
                   k = 1) %>%
  map_df(., unlist) %>%
  cbind(., amtrak_stations %>% filter(Code %in% delay_data_pct$StationCode)) %>%
  merge(., p_airports, by.x = "nn", by.y = "index") %>%
  select(nn, dist, Code, FAC_NAME) %>%
  mutate(dist = dist/1609,
         "percentile" = percent_rank(dist),
         "far" = ifelse(percentile >= 0.75, "Airport is farther than from 75% of other stations", FALSE)) %>%
  merge(., amtrak_stations %>% select(Code, geometry), by = "Code") %>%
  mutate("distance" = ifelse(dist <= 35, "35 miles or closer to nearest airport",
                      ifelse(dist > 35 & dist <= 70, "36 - 70 miles to nearest airport",
                      ifelse(dist > 70 & dist <= 105, "71 - 105 miles to nearest airport",
                      ifelse(dist > 105, "106 - 140 miles to nearest airport", "check"))))) %>%
  st_as_sf()

p_ic_bus <- ipcd %>%
  filter(FAC_TYPE %in% c(2)) %>%
  mutate("index" = 1:n())

ic_bus_nn <- st_nn(amtrak_stations %>% filter(Code %in% delay_data_pct$StationCode), 
                   p_ic_bus, 
                   returnDist = TRUE, 
                   k = 1) %>%
  map_df(., unlist) %>%
  cbind(., amtrak_stations %>% filter(Code %in% delay_data_pct$StationCode)) %>%
  merge(., p_ic_bus, by.x = "nn", by.y = "index") %>%
  select(nn, dist, Code, FAC_NAME) %>%
  mutate(dist = dist/1609) %>%
  merge(., amtrak_stations %>% select(Code, geometry), by = "Code") %>%
  mutate("distance" = ifelse(dist <= 2.5, "2.5 miles or closer to nearest intercity bus station",
                      ifelse(dist > 2.5 & dist <= 10, "2.5 - 10 miles to nearest intercity bus station",
                      ifelse(dist > 10 & dist <= 25, "10 - 25 miles to nearest intercity bus station",
                      ifelse(dist > 25, "Farther than 25 miles to nearest intercity bus station", "check"))))) %>%
  st_as_sf()

airport_nn_plot <- ggplot(data = airport_nn, aes(y = dist)) +
  geom_boxplot(outlier.color = "#FFFFFF", outlier.size = 3, 
               color = "#FFFFFF", fill = "#262626",
               linewidth = 1) +
  labs(y = str_wrap("Distance between an LDR Amtrak Station and Nearest Airport (in miles)",
                    width = 10)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#000000"),
        text = element_text(color = "#FFFFFF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "MS Reference Sans Serif",
                                   size = 12,
                                   color = "#FFFFFF"),
        axis.title.y = element_text(family = "MS Reference Sans Serif",
                                    size = 24,
                                    margin = margin(t = 0, r = 15, l = 0, b = 0),
                                    angle = 0,
                                    vjust = 0.5))

ic_bus_nn_plot <- ggplot(data = ic_bus_nn, aes(y = dist)) +
  geom_boxplot(outlier.color = "#FFFFFF", outlier.size = 3, 
               color = "#FFFFFF", fill = "#262626",
               linewidth = 1) +
  labs(y = str_wrap("Distance between an LDR Amtrak Station and Nearest Intercity Bus Station (in miles)",
                    width = 10)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#000000"),
        text = element_text(color = "#FFFFFF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "MS Reference Sans Serif",
                                   size = 12,
                                   color = "#FFFFFF"),
        axis.title.y = element_text(family = "MS Reference Sans Serif",
                                    size = 24,
                                    margin = margin(t = 0, r = 15, l = 0, b = 0),
                                    angle = 0,
                                    vjust = 0.5))

colors_airport <- c("35 miles or closer to nearest airport" = "#404040",
                    "36 - 70 miles to nearest airport" = "#808080",
                    "71 - 105 miles to nearest airport" = "#BFBFBF",
                    "106 - 140 miles to nearest airport" = "#FFFFFF")

airport_nn_map <- ggplot(airport_nn) +
  geom_sf(aes(color = distance)) +
  scale_color_manual(values = colors_airport,
                     breaks = c(c("35 miles or closer to nearest airport",
                                  "36 - 70 miles to nearest airport",
                                  "71 - 105 miles to nearest airport",
                                  "106 - 140 miles to nearest airport")),
                     labels = function(x) str_wrap(x, width = 20)) +
  guides(color = guide_legend(title = str_wrap("Distance from Nearest Airport (in miles)",
                                               width = 15))) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#000000"),
        legend.title = element_text(family = "MS Reference Sans Serif",
                                    color = "#FFFFFF",
                                    size = 24),
        legend.text = element_text(family = "MS Reference Sans Serif",
                                   color = "#FFFFFF",
                                   size = 12),
        legend.key.spacing.y = unit(1.0, 'cm'),
        legend.key.spacing = unit(1.0, 'cm'))

colors <- c("2.5 miles or closer to nearest intercity bus station" = "#404040",
            "2.5 - 10 miles to nearest intercity bus station" = "#808080", 
            "10 - 25 miles to nearest intercity bus station" = "#BFBFBF", 
            "Farther than 25 miles to nearest intercity bus station" = "#FFFFFF")

ic_bus_nn_map <- ggplot(ic_bus_nn) +
  geom_sf(aes(color = distance), size = 2) +
  scale_color_manual(values = colors,
                     breaks = c(c("2.5 miles or closer to nearest intercity bus station",
                                  "2.5 - 10 miles to nearest intercity bus station", 
                                  "10 - 25 miles to nearest intercity bus station", 
                                  "Farther than 25 miles to nearest intercity bus station")),
                     labels = function(x) str_wrap(x, width = 20)) +
  guides(color = guide_legend(title = str_wrap("Distance from Nearest Intercity Bus Station (in miles)",
                                               width = 15))) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#000000"),
        legend.title = element_text(family = "MS Reference Sans Serif",
                                    color = "#FFFFFF",
                                    size = 24),
        legend.text = element_text(family = "MS Reference Sans Serif",
                                   color = "#FFFFFF",
                                   size = 12),
        legend.key.spacing.y = unit(1.0, 'cm'),
        legend.key.spacing = unit(1.0, 'cm'))

# ggsave(plot = airport_nn_plot,
#        filename = "Documents//Exports//airport_nn_plot.png",
#        width = 3840, height = 2160, units = "px")
# ggsave(plot = ic_bus_nn_plot,
#        filename = "Documents//Exports//ic_bus_nn_plot.png",
#        width = 3840, height = 2160, units = "px")
# 
# ggsave(plot = airport_nn_map,
#        filename = "Documents//Exports//airport_nn_map.png",
#        width = 3840, height = 2160, units = "px")
# ggsave(plot = ic_bus_nn_map,
#        filename = "Documents//Exports//ic_bus_nn_map.png",
#        width = 3840, height = 2160, units = "px")
