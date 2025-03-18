library(tidyverse)
library(sf)
library(nngeo)
library(extrafont)
# font_import()
loadfonts(device = "win")

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

legend_label <- "Airport is farther than from 75% of other stations"

ggplot(airport_nn) +
  geom_sf(aes(color = dist)) +
  scale_color_binned() +
  theme_void() +
theme(plot.background = element_rect(fill = "#000000"),
      text = element_text(color = "#FFFFFF"))

ggsave(plot = airport_nn_plot,
       filename = "Documents//Exports//airport_nn_plot.png",
       width = 3840, height = 2160, units = "px")
ggsave(plot = ic_bus_nn_plot,
       filename = "Documents//Exports//ic_bus_nn_plot.png",
       width = 3840, height = 2160, units = "px")