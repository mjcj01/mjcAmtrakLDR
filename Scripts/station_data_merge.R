library(tidyverse)
library(sf)

source("Scripts//data_load.R")

station_data <- w_isochrone_bg_join_filter %>%
  as.data.frame() %>%
  select(-geometry, -P1_001N) %>%
  pivot_longer(cols = contains("P2_")) %>%
  filter(name == "P2_001N") %>%
  group_by(id, name) %>%
  reframe("T_POP" = sum(value)) %>%
  merge(., amtrak_stations, by.x = "id", by.y = "Code") %>%
  select(id, T_POP, geometry) %>%
  merge(., delay_data_pct, by.x = "id", by.y = "StationCode") %>%
  merge(., night_stops, by.x = "id", by.y = "stop_id") %>%
  select(-pct, -late_class) %>%
  unique() %>%
  st_as_sf()

# st_write(station_data, "Data//Amtrak Station Characteristics//station_characteristics.shp")