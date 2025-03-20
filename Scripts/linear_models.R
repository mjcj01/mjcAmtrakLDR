library(tidyverse)

#source("Scripts//data_load.R")
source("Scripts//airport_comparison.R")

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

lm(data = nn_station_merge,
   formula = cont_rdrs ~ ic_bus_dist) %>% summary()

lm(data = nn_station_merge %>% filter(nght_s_ <= 2),
   formula = cont_rdrs ~ not_late) %>% summary()

ggplot(nn_station_merge %>% filter(nght_s_ <= 2), aes(x = nght_s_, y = rdrs_24, group = nght_s_)) +
  geom_violin()

# merge(nn_station_merge, amtrak_stations %>% select(Code, geometry), by = "Code") %>%
#   st_write(., "Data//station_characteristics.shp", append = FALSE)
