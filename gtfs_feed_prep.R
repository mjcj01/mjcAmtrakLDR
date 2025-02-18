library(tidyverse)
library(tidytransit)
library(sf)

gtfs <- read_gtfs("https://content.amtrak.com/content/gtfs/GTFS.zip")
gtfs <- set_servicepattern(gtfs)

calendar = tibble(date = c(as.Date("2025-03-23"),
                           as.Date("2025-03-24"),
                           as.Date("2025-03-25"),
                           as.Date("2025-03-26"),
                           as.Date("2025-03-27"),
                           as.Date("2025-03-28"),
                           as.Date("2025-03-29"))) %>% 
  mutate(weekday = (function(date) {
    c("Sunday", "Monday", "Tuesday",
      "Wednesday", "Thursday", "Friday",
      "Saturday")[as.POSIXlt(date)$wday + 1]})(date))

service_dates <- gtfs$.$dates_servicepatterns %>%
  #filter(date %in% calendar$date) %>%
  merge(., gtfs$.$servicepatterns, by = "servicepattern_id")# %>%
  #filter(date == as.Date("2025-03-24"))

amtrak_station_characteristics <- gtfs$trips %>%
  merge(., gtfs$stop_times, by = "trip_id") %>%
  merge(., service_dates, by = "service_id") %>%
  merge(., gtfs$routes, by = "route_id") %>%
  filter(route_long_name != "Commuter Rail") %>%
  group_by(date, stop_id) %>%
  summarise("count" = n()) %>%
  merge(., amtrak_ridership_24, by.x = "stop_id", by.y = "Code", all.y = TRUE)

station_check <- gtfs$routes %>% 
  merge(., gtfs$trips, by = "route_id") %>% 
  merge(., gtfs$stop_times, by = "trip_id") %>%
  filter(route_long_name != "Commuter Rail" & 
         route_long_name != "Amtrak Thruway Connecting Service") %>%
  select(route_long_name, stop_id) %>%
  mutate("stops_along_route" = TRUE) %>%
  merge(.,
        gtfs$stops %>%
          merge(., gtfs$routes) %>%
          select(route_long_name, stop_id) %>%
          filter(route_long_name != "Commuter Rail" & 
                   route_long_name != "Amtrak Thruway Connecting Service"),
        by = c("route_long_name", "stop_id"), all = TRUE) %>%
  distinct() %>%
  mutate("stops_along_route" = ifelse(is.na(stops_along_route), FALSE, TRUE)) %>%
  filter(stop_id %in% amtrak_ridership_24$Code &
         route_long_name != "Auto Train") %>%
  mutate(route_long_name = gsub(" ", "", route_long_name)) %>%
  pivot_wider(names_from = "route_long_name", values_from = "stops_along_route") %>%
  .[ rowSums(. == "TRUE") == 1, ] %>%
  mutate("one_route_only" = TRUE) %>%
  select(stop_id, one_route_only)

amtrak_station_characteristics <- amtrak_station_characteristics %>%
  merge(., station_check, by = "stop_id", all = TRUE) %>%
  mutate(one_route_only = ifelse(is.na(one_route_only), FALSE, TRUE))

amtrak_station_characteristics %>%
  merge(., amtrak_stations, by.x = "stop_id", by.y = "Code") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(color = one_route_only))
