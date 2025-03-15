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
