library(tidyverse)
library(sf)

delay_data <- read_rds("Data//SM_delay_data.rds") %>%
  mutate(StationCode = gsub(" ", "", StationCode))

train_number_list <- delay_data %>%
  group_by(StationCode, train_number) %>%
  reframe("count" = n()) %>%
  filter(count >= 30) %>%
  pull(train_number) %>%
  unique()

delay_data <- delay_data %>%
  filter(train_number %in% train_number_list)

amtrak_stations <- st_read("Data//Amtrak Stations//Amtrak_Stations.shp")

delay_data_class <- delay_data %>%
  filter(StationCode %in% amtrak_gtfs_feed$stops$stop_id) %>%
  mutate("late_class" = ifelse(sec_diff == 0, "not_late",
                               ifelse(sec_diff > 0 & sec_diff <= 600, "min_late", "late")))

delay_data_pct <- delay_data_class %>%
  group_by(StationCode, late_class) %>%
  reframe("count" = n()) %>%
  group_by(StationCode) %>%
  reframe("pct" = count / sum(count),
          "late_class" = late_class)

delay_data_pct_train <- delay_data %>%
  filter(StationCode %in% amtrak_gtfs_feed$stops$stop_id) %>%
  mutate("late_class" = ifelse(sec_diff == 0, "not_late",
                               ifelse(sec_diff > 0 & sec_diff <= 600, "min_late", "late"))) %>%
  group_by(train_number, late_class, StationCode) %>%
  reframe("count" = n()) %>%
  group_by(train_number, StationCode) %>%
  reframe("pct" = (count / sum(count)),
          "late_class" = late_class,
          "obs" = n())

delay_data_pct_train$pct[is.na(delay_data_pct_train$pct)] <- 0

ipcd <- st_read("Data//IPCD Shapefile//Intermodal_Passenger_Connectivity_Database_(IPCD).shp")

w_isochrone_bg_join_filter <- st_read("Data//Isochrones//Walking//w_isochrone_bg.shp")
w_isochrone_bg_centroids <- st_read("Data//Isochrones/Walking//w_isochrone_bg_centroids.shp")
d_isochrone_bg_join_filter <- st_read("Data//Isochrones//Driving//d_isochrone_bg.shp")
d_isochrone_bg_centroids <- st_read("Data//Isochrones/Driving//d_isochrone_bg_centroids.shp")

night_stops <- read_rds("Data//night_stops.rds")

station_data <- d_isochrone_bg_join_filter %>%
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
