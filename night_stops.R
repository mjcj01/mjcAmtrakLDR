library(tidyverse)

gtfs$routes %>% 
  merge(., gtfs$trips, by = "route_id") %>% 
  merge(., gtfs$stop_times, by = "trip_id") %>%
  filter(route_long_name != "Commuter Rail" & 
         route_long_name != "Amtrak Thruway Connecting Service") %>%
  select(route_long_name, stop_id, departure_time, direction_id) %>%
  mutate(departure_time = dttr2::dtt_time(departure_time)) %>%
  mutate("night_stop" = ifelse(departure_time >= hms("20:00:00") |
                               departure_time < hms("08:00:00"), 1, 0)) %>%
  unique() %>%
  group_by(stop_id) %>%
  reframe("night_stop" = sum(night_stop)) %>%
  merge(., amtrak_station_characteristics, by = "stop_id") %>%
  mutate("only_night_stops" = ifelse(count == night_stop, "yes", "no")) %>%
  merge(., amtrak_stations_ldr, by = "stop_id") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(color = only_night_stops))
