library(tidyverse)
library(sf)

night_stops_ldr <- st_read("Data//Night Routes//night_routes.shp") %>%
  as.data.frame() %>%
  select(-geometry) %>%
  drop_na(pct_ng_) %>%
  merge(., delay_data_pct, by.x = "stop_id", by.y = "StationCode")