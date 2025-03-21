library(tidyverse)
library(tidycensus)
library(sf)
library(mapboxapi)

source("Scripts//data_load.R")
source("Scripts//amtrak_gtfs_ldr_filter.R")

states <- tigris::states() %>% pull(STUSPS)

amtrak_states <- st_read("Data//Amtrak Stations//Amtrak_Stations.shp") %>%
  as.data.frame() %>%
  filter(StnType == "TRAIN" & State %in% states) %>%
  pull(State) %>%
  unique()

census_block_groups <- NULL

variables <- c("P2_002N", "P2_005N", "P2_006N", "P2_007N", "P2_008N", "P2_009N", "P2_010N", "P2_011N")

for (i in amtrak_states) {
  df <- get_decennial(geography = "block",
                       variables = variables,
                       year = 2020,
                       state = i,
                       geometry = TRUE)
  census_block_groups <- rbind(census_block_groups, df)
}

census_block_groups <- census_block_groups %>%
  st_transform(crs = 4326)

amtrak_ldr_stations <- amtrak_stations %>%
  filter(Code %in% amtrak_gtfs_feed$stops$stop_id)

walking_isochrone <- amtrak_ldr_stations %>%
  mb_isochrone(., profile = "walking", time = 30, id_column = "Code") %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

st_write(walking_isochrone, "Data//Isochrones//Walking//walking_isochrone.shp")

driving_isochrone <- amtrak_ldr_stations %>%
  mb_isochrone(., profile = "driving", time = 30, id_column = "Code") %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

st_write(driving_isochrone, "Data//Isochrones//Driving//driving_isochrone.shp")

w_isochrone_bg_join <- st_join(census_block_groups, walking_isochrone)

d_isochrone_bg_join <- st_join(census_block_groups, driving_isochrone)

w_isochrone_bg_names <- w_isochrone_bg_join %>%
  as.data.frame() %>%
  select(-geometry) %>% 
  filter(!is.na(time)) %>% 
  unique()

d_isochrone_bg_names <- d_isochrone_bg_join %>%
  as.data.frame() %>%
  select(-geometry) %>% 
  filter(!is.na(time)) %>% 
  unique()

w_isochrone_bg_join_filter <- w_isochrone_bg_join %>%
  filter(NAME %in% w_isochrone_bg_names$NAME) %>%
  st_as_sf() %>%
  pivot_wider(names_from = "variable",
              values_from = "value")

d_isochrone_bg_join_filter <- d_isochrone_bg_join %>%
  filter(NAME %in% d_isochrone_bg_names$NAME) %>%
  st_as_sf() %>%
  pivot_wider(names_from = "variable",
              values_from = "value")

w_isochrone_bg_centroids <- st_centroid(w_isochrone_bg_join_filter)

d_isochrone_bg_centroids <- st_centroid(d_isochrone_bg_join_filter)

st_write(w_isochrone_bg_join_filter, "Data//Isochrones//Walking//w_isochrone_bg.shp")
st_write(w_isochrone_bg_centroids, "Data//Isochrones/Walking//w_isochrone_bg_centroids.shp")
st_write(d_isochrone_bg_join_filter, "Data//Isochrones//Walking//d_isochrone_bg.shp")
st_write(d_isochrone_bg_centroids, "Data//Isochrones/Walking//d_isochrone_bg_centroids.shp")