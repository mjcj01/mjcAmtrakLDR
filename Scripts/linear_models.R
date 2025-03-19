library(tidyverse)



airport_nn_short <- airport_nn %>%
  as.data.frame() %>%
  select(Code, dist, FAC_NAME) %>%
  rename(airport_dist = "dist",
         airport_fac = "FAC_NAME")

ic_bus_nn_short <- ic_bus_nn %>%
  as.data.frame() %>%
  select(Code, dist, FAC_NAME) %>%
  rename(ic_bus_dist = "dist",
         ic_bus_fac = "FAC_NAME")

nn_merge <- merge(airport_nn_short, ic_bus_nn_short, by = "Code")

nn_station_merge <- merge(nn_merge, station_data, by.x = "Code", by.y = "id") %>%
  mutate("cont_rdrs" = (rdrs_24/count)/T_POP)

glm(data = nn_station_merge,
   formula = rdrs_24 ~ airport_dist + ic_bus_dist) %>% summary()

merge(nn_station_merge, amtrak_stations %>% select(Code, geometry), by = "Code") %>%
  st_write(., "Data//station_characteristics.shp", append = FALSE)
