library(tidyverse)
library(tidycensus)
library(sf)

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

ipcd_join <- st_join(x = ipcd, y = census_county_subdivisions, join = st_within)

ipcd_join <- ipcd_join %>%
  select(X, Y, POINT_ID, AMTRAKCODE, STATE, CBSA_CODE, COUSUBFP, COUSUBNS, geometry) %>%
  st_transform(crs = 4326)

census_county_subdivisions_filtered <- census_county_subdivisions %>%
  filter(COUSUBFP %in% ipcd_join$COUSUBFP) %>%
  st_transform(crs = 4326)

census_tracts_filtered <- census_tracts %>%
  st_transform(crs = 4326) %>%
  st_join(census_county_subdivisions_filtered, ., join = st_within)
