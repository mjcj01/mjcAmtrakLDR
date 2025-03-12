library(tidyverse)

amtrak_stations_ldr <- amtrak_stations %>%
  filter(Code %in% amtrak_ridership_24$Code)

night_routes_cardinal_sunset <- merge(gtfs$.$dates_servicepatterns %>%
        filter(date %in% calendar$date) %>%
        merge(., gtfs$.$servicepatterns, by = "servicepattern_id") %>%
        filter(date %in% c(as.Date("2025-03-25"),
                           as.Date("2025-03-26"),
                           as.Date("2025-03-27"),
                           as.Date("2025-03-28"),
                           as.Date("2025-03-29"),
                           as.Date("2025-03-30"))),
      gtfs$trips %>%
        merge(., gtfs$stop_times, by = "trip_id") %>%
        merge(., gtfs$routes, by = "route_id"),
      by = "service_id") %>%
  #filter(stop_id == "CHI" | stop_id == "LAX") %>%
  filter(route_long_name == "Cardinal" |
           route_long_name == "Sunset Limited") %>%
  merge(., station_check, by = "stop_id") %>%
  select(stop_id, departure_time, route_long_name, one_route_only) %>%
  merge(.,
        gtfs$trips %>%
          merge(., gtfs$stop_times, by = "trip_id") %>%
          merge(., gtfs$routes, by = "route_id") %>%
          filter(route_long_name == "Cardinal" |
                   route_long_name == "Sunset Limited") %>%
          select(stop_id, departure_time) %>%
          unique() %>%
          group_by(stop_id) %>%
          summarise("count" = 1) %>%
          merge(., amtrak_ridership_24, by.x = "stop_id", by.y = "Code", all.y = TRUE),
        by = "stop_id") %>%
  mutate(departure_time = dttr2::dtt_time(departure_time)) %>%
  mutate("night_stop" = ifelse(departure_time >= hms("20:00:00") |
                                 departure_time < hms("08:00:00"), 1, 0)) %>%
  unique() %>%
  group_by(stop_id) %>%
  reframe("night_stop_sum" = sum(night_stop),
          "count" = sum(count)) %>%
  merge(., amtrak_station_characteristics %>% 
          select(stop_id, ridership_24, one_route_only) %>% 
          unique(), 
        by = "stop_id") %>%
  mutate("only_night_stops" = ifelse(night_stop_sum > 0, "yes", "no")) %>%
  merge(., amtrak_stations_ldr, by.x = "stop_id", by.y = "Code") %>%
  select(stop_id, ridership_24, one_route_only, night_stop_sum, count, geometry) %>%
  mutate("pct_night_stops" = night_stop_sum / count) %>%
  st_as_sf()

night_routes <- gtfs$routes %>% 
  merge(., gtfs$trips, by = "route_id") %>% 
  merge(., gtfs$stop_times, by = "trip_id") %>%
  merge(., service_dates, by = "service_id") %>%
  filter(route_long_name != "Commuter Rail" & 
         route_long_name != "Amtrak Thruway Connecting Service" &
         route_long_name != "Cardinal" &
         route_long_name != "Sunset Limited" &
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
  st_as_sf() %>%
  rbind(., night_routes_cardinal_sunset)

st_write(night_routes, "Data//Night Routes//night_routes.shp")

ggplot() +
  geom_sf(data = night_routes %>%
            filter(one_route_only == FALSE),
          color = "gray") +
  geom_sf(data = night_routes %>% 
            filter(one_route_only == TRUE) %>%
            mutate(at_least_one_night_stop = ifelse(night_stop_sum == 1, "yes", "no")), 
          aes(color = at_least_one_night_stop))
