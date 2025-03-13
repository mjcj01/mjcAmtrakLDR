library(tidyverse)
library(sf)

delay_data <- read_rds("Data//SM_delay_data.rds") %>%
  mutate(StationCode = gsub(" ", "", StationCode))

ipcd_stations <- st_read("Data//IPCD Shapefile//Intermodal_Passenger_Connectivity_Database_(IPCD).shp") %>%
  filter(AMTRAKCODE %in% delay_data$StationCode)