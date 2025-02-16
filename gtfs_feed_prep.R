library(tidyverse)
library(tidytransit)

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
  filter(date %in% calendar$date) %>%
  merge(., gtfs$.$servicepatterns, by = "servicepattern_id")

gtfs$trips %>%
  merge(., gtfs$stop_times, by = "trip_id") %>%
  merge(., service_dates, by = "service_id") %>%
  filter()
  group_by(date, stop_id) %>%
  summarise("count" = n()) %>% View()
