library(tidyverse)
library(tidytransit)
library(rvest)

amtrak_gtfs_feed <- read_gtfs("https://content.amtrak.com/content/gtfs/GTFS.zip")

ldr_routes <- read_html("https://en.wikipedia.org/wiki/Long-distance_Amtrak_routes") %>%
  html_node(., ".wikitable") %>%
  html_table(.) %>%
  mutate(Name = ifelse(grepl("note", Name), substr(Name, 1, nchar(Name) - 8), Name)) %>%
  pull(Name)
  
amtrak_gtfs_feed$routes <- amtrak_gtfs_feed$routes %>%
  filter(route_long_name %in% ldr_routes) %>%
  filter(route_long_name != "Auto Train")

amtrak_gtfs_feed$trips <- amtrak_gtfs_feed$trips %>%
  filter(route_id %in% amtrak_gtfs_feed$routes$route_id)

amtrak_gtfs_feed$calendar <- amtrak_gtfs_feed$calendar %>%
  filter(service_id %in% amtrak_gtfs_feed$trips$service_id) %>%
  mutate(start_date = format(start_date, "%Y%m%d"),
         end_date = format(end_date, "%Y%m%d"))

amtrak_gtfs_feed$shapes <- amtrak_gtfs_feed$shapes %>%
  filter(shape_id %in% amtrak_gtfs_feed$trips$shape_id)

amtrak_gtfs_feed$stop_times <- amtrak_gtfs_feed$stop_times %>%
  filter(trip_id %in% amtrak_gtfs_feed$trips$trip_id) %>%
  mutate(arrival_time = format(arrival_time, "%T"),
         departure_time = format(departure_time, "%T"))

amtrak_gtfs_feed$stops <- amtrak_gtfs_feed$stops %>%
  filter(stop_id %in% amtrak_gtfs_feed$stop_times$stop_id)

write.table(amtrak_gtfs_feed$agency, "Amtrak GTFS (LDR Only)//agency.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$calendar, "Amtrak GTFS (LDR Only)//calendar.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$feed_info, "Amtrak GTFS (LDR Only)//feed_info.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$routes, "Amtrak GTFS (LDR Only)//routes.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$stops, "Amtrak GTFS (LDR Only)//stops.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$stop_times, "Amtrak GTFS (LDR Only)//stop_times.txt", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(amtrak_gtfs_feed$trips, "Amtrak GTFS (LDR Only)//trips.txt", sep = ",", row.names = FALSE, col.names = TRUE)
