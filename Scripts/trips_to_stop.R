library(tidyverse)
library(sf)

night_stop_one_route <- st_read("Data//Night Routes//night_routes.shp") %>%
  as.data.frame() %>%
  select(-geometry)

table(night_stop_one_route$on_rt_n)

night_stop_one_route %>%
  filter(on_rt_n == 1) %>%
  drop_na(pct_ng_) %>%
  pull(pct_ng_) %>%
  table() %>%
  as.data.frame() %>%
  pivot_wider(values_from = "Freq", names_from = ".")
