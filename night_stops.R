library(tidyverse)

amtrak_stations_ldr <- amtrak_stations %>%
  filter(Code %in% amtrak_ridership_24$Code)

night_routes <- gtfs$routes %>% 
  merge(., gtfs$trips, by = "route_id") %>% 
  merge(., gtfs$stop_times, by = "trip_id") %>%
  merge(., service_dates, by = "service_id") %>%
  filter(route_long_name != "Commuter Rail" & 
         route_long_name != "Amtrak Thruway Connecting Service" &
         stop_id %in% amtrak_ridership_24$Code) %>%
  select(route_long_name, stop_id, departure_time, direction_id) %>%
  mutate(departure_time = dttr2::dtt_time(departure_time)) %>%
  mutate("night_stop" = ifelse(departure_time >= hms("20:00:00") |
                               departure_time < hms("08:00:00"), 1, 0)) %>%
  unique() %>%
  group_by(stop_id) %>%
  reframe("night_stop_sum" = sum(night_stop)) %>%
  merge(., amtrak_station_characteristics %>% select(stop_id, ridership_24, one_route_only, count) %>% unique(), by = "stop_id") %>%
  mutate("only_night_stops" = ifelse(night_stop_sum > 0, "yes", "no")) %>%
  merge(., amtrak_stations_ldr, by.x = "stop_id", by.y = "Code") %>%
  select(stop_id, ridership_24, one_route_only, night_stop_sum, count, geometry) %>%
  mutate("pct_night_stops" = night_stop_sum / count) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = night_routes %>%
            filter(one_route_only == FALSE),
          color = "gray") +
  geom_sf(data = night_routes %>% 
            filter(one_route_only == TRUE) %>%
            mutate(at_least_one_night_stop = ifelse(night_stop_sum > 0, "yes", "no")), 
          aes(color = only_night_stops))
  

          