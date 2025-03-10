library(tidyverse)
library(tidycensus)
library(sf)
library(mapboxapi)

states <- tigris::states() %>% pull(STUSPS)

amtrak_states <- amtrak_stations %>%
  as.data.frame() %>%
  filter(StnType == "TRAIN" & State %in% states) %>%
  pull(State) %>%
  unique()

census_tracts <- NULL

for (i in amtrak_states) {
  df <- get_decennial(geography = "tract",
                      variables = "P1_001N",
                      year = 2020,
                      state = i,
                      geometry = TRUE)
  census_tracts <- rbind(census_tracts, df)
}

census_tracts <- st_transform(census_tracts, crs = 4326)

census_county_subdivisions <- NULL
for (i in amtrak_states) {
  df <- tigris::county_subdivisions(state = i)
  census_county_subdivisions <- rbind(census_county_subdivisions, df)
}

ipcd <- st_read("Data//IPCD Shapefile//Intermodal_Passenger_Connectivity_Database_(IPCD).shp") %>%
  filter(!is.na(AMTRAKCODE)) %>%
  st_transform(crs = 4326) %>%
  filter(AMTRAKCODE %in% amtrak_ldr_stations)

census_county_subdivisions <- st_transform(census_county_subdivisions, crs = 4326)

ipcd_join <- st_join(x = ipcd, y = census_tracts, join = st_within)

ipcd_join <- ipcd_join %>%
  select(X, Y, POINT_ID, AMTRAKCODE, CITY, STATE, NAME, geometry) %>%
  st_transform(crs = 4326)

# census_county_subdivisions_filtered <- census_county_subdivisions %>%
#   filter(COUSUBFP %in% ipcd_join$COUSUBFP) %>%
#   st_transform(crs = 4326)
# 
# census_tracts_filtered <- census_tracts %>%
#   st_transform(crs = 4326) %>%
#   st_join(census_county_subdivisions_filtered, ., join = st_within)

census_tract_stations <- census_tracts %>%
  mutate("station_check" = ifelse(NAME %in% ipcd_join$NAME, TRUE, FALSE))

station_driving_isochrone <- ipcd_join %>%
  mb_isochrone(., profile = "driving", time = 30, id_column = "AMTRAKCODE") %>%
  st_as_sf()

station_walking_isochrone <- ipcd_join %>%
  mb_isochrone(., profile = "walking", time = 30, id_column = "AMTRAKCODE") %>%
  st_as_sf()

census_tracts_driving <- census_tracts %>%
  st_join(., station_driving_isochrone, join = st_within, largest = TRUE) %>%
  drop_na(time)

census_tracts_walking <- census_tracts %>%
  st_join(., station_walking_isochrone, join = st_within, largest = TRUE) %>%
  drop_na(time)

station_pop_drive <- census_tracts_driving %>%
  as.data.frame() %>%
  group_by(id) %>%
  reframe("D_POP" = sum(value))

station_pop_walk <- census_tracts_walking %>%
  as.data.frame() %>%
  group_by(id) %>%
  reframe("W_POP" = sum(value))

station_pop_all <- merge(station_pop_drive, station_pop_walk, by = "id", all = TRUE)
station_pop_all[is.na(station_pop_all)] <- 0
