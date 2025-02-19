library(tidyverse)
library(rvest)
library(sf)

amtrak_ldr_nums <- "https://en.wikipedia.org/wiki/Long-distance_Amtrak_routes#Routes" %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[2]") %>%
  html_table() %>%
  bind_rows()  %>%
  filter(Name != "Auto Train[note 5]") %>%
  mutate(Numbers = strsplit(as.character(Numbers), ", ")) %>% 
  unnest(Numbers) %>%
  filter(Numbers != 421 & Numbers != 422) %>%
  pull(Numbers)

amtrak_stations <- st_read("Data//Amtrak Stations//Amtrak_Stations.shp")

juckins_scrape <- function(train_numbers, start_date, end_date) {
  start_month <- format(as.Date(start_date, "%m/%d/%Y"), "%m")
  start_day <- format(as.Date(start_date, "%m/%d/%Y"), "%d")
  start_year <- format(as.Date(start_date, "%m/%d/%Y"), "%Y")
  end_month <- format(as.Date(end_date, "%m/%d/%Y"), "%m")
  end_day <- format(as.Date(end_date, "%m/%d/%Y"), "%d")
  end_year <- format(as.Date(end_date, "%m/%d/%Y"), "%Y")
  
  data <- data.frame()
  
  for (i in train_numbers) {
    url <- paste("https://juckins.net/amtrak_status/archive/html/history.php?train_num=", 
    i, "&station=&date_start=", start_month, "%2F", start_day, "%2F", start_year,
    "&date_end=", end_month, "%2F", end_day, "%2F", end_year,
    "&df1=1&df2=1&df3=1&df4=1&df5=1&df6=1&df7=1&sort=schAr&sort_dir=DESC&co=gt&limit_mins=&dfon=1",
    sep = "")
    
    df <- url %>%
      read_html() %>%
      html_nodes(xpath = "/html/body/div[1]/div/table") %>%
      html_table() %>% 
      bind_rows()
    
    colnames(df) <- df[1, ]
    df <- df[-1, ]
    
    df$train_number <- i
    
    df <- df %>%
      rename(origin_date = "Origin Date",
             station = "Station",
             sch_ar = "Sch Ar",
             act_ar = "Act Ar",
             comments = "Comments",
             service_disrupt = "Service Disruption",
             cancellations = "Cancellations") %>%
      mutate(origin_date = str_sub(origin_date, start = 1L, end = 10),
             sch_ar_date = str_sub(sch_ar, start = 1L, end = 10),
             sch_ar_time = str_sub(sch_ar, start = 12, end = -1L),
             sch_ar_time = substr(sch_ar_time, 1, nchar(sch_ar_time) - 5),
             sch_ar_time = ifelse(nchar(sch_ar_time) == 7, paste(0, sch_ar_time, sep = ""), sch_ar_time)) %>%
      mutate(sch_ar = paste(sch_ar_date, sch_ar_time, sep = " "),
             sch_ar = as.POSIXct(sch_ar, format = "%m/%d/%Y %I:%M %p")) %>%
      filter(act_ar != "") %>%
      select(-cancellations, -service_disrupt, -sch_ar_date, -sch_ar_time) %>%
      mutate("late_check" = ifelse(grepl("LATE", comments), "late",
                            ifelse(grepl("late", comments), "late", "not_late"))) %>%
      mutate(comments = gsub(x = comments, pattern = "\\..*", replacement = ""),
             comments = gsub(x = comments, pattern = "\\|.*", replacement = ""),
             comments = gsub(x = comments, pattern = "LATE\\:.*", replacement = ""),
             comments = gsub(x = comments, pattern = " HR ", ","),
             comments = gsub("[^0-9,-]", "", comments),
             hour_diff = ifelse(grepl(",", comments), as.numeric(gsub("\\,.*", "", comments)), 0),
             min_diff = ifelse(grepl(",", comments), as.numeric(gsub(".*\\,", "", comments)), comments),
             min_diff = ifelse(is.na(min_diff), 0, as.numeric(min_diff)),
             sec_diff = (hour_diff * 60 * 60) + (min_diff * 60),
             sec_diff = ifelse(late_check == "late", sec_diff * 1, sec_diff * -1),
             act_ar_datetime = sch_ar + sec_diff) %>% 
      drop_na(sch_ar)
    
    data <- rbind(data, df)
  }
  data
}

amtrak <- juckins_scrape(amtrak_ldr_nums, "01/01/2022", "12/31/2024")
amtrak_delays_2022_23 <- amtrak %>%
  filter(sch_ar < as.Date("2024-01-01 00:00:00"))

amtrak %>%
  drop_na(sec_diff) %>%
  group_by(station) %>%
  reframe("avg_delay" = mean(sec_diff),
          "obsv" = n()) %>%
  merge(., amtrak_stations, by.x = "station", by.y = "Code") %>%
  st_as_sf() %>%
  st_write(., "Data//Amtrak Station Delay Data//station_delays.shp", append = FALSE)

amtrak_stations_df <- amtrak_stations %>%
  as.data.frame() %>%
  select(-geometry)

ridership_scrape <- function(station_code) {
  city <- amtrak_stations_df %>%
    filter(Code == station_code) %>%
    pull(City)
  state <- amtrak_stations_df %>%
    filter(Code == station_code) %>%
    pull(State)
  
  slug <- paste(str_to_lower(city), "-", str_to_lower(state), "-", str_to_lower(station_code), sep = "")
  
  tryCatch({
    url <- paste("https://www.greatamericanstations.com/stations/", station_code, "/", sep = "")
    ridership <- url %>%
      read_html() %>%
      html_node(xpath = "/html/body/div[4]/div/div[1]/div[2]/div[1]/div") %>%
      html_text() %>%
      gsub(x = ., ".*Annual Station Ridership \\(FY 2024\\)", "") %>%
      parse_number()},
    error = function(condition) {
      message("Error in retrieving ", station_code)
      message(conditionMessage(condition))
      ridership <<- NA},
    warning = function(condition) {
      message("Warning in retrieving ", station_code)
      message(conditionMessage(condition))
      ridership <<- NA},
    finally = function(condition) {
      message(paste("Processed ", station_code))})
  
  data.frame("Code" = station_code,
             "ridership_24" = ridership)
}

amtrak_ldr_stations <- amtrak_gtfs_feed$stops$stop_id

amtrak_ridership_24 <- NULL

for (i in amtrak_ldr_stations) {
  df <- ridership_scrape(i)
  amtrak_ridership_24 <- rbind(amtrak_ridership_24, df)
  rm(df, i)
}

amtrak_ridership_24 <- amtrak_ridership_24 %>%
  mutate(ridership_24 = ifelse(Code == "ACD", 1233,
                        ifelse(Code == "LKL", 22861,
                        ifelse(Code == "MKS", 4977, ridership_24))))

write_rds(amtrak_ridership_24, "Data//station_ridership_24.Rds")

amtrak_ridership_24 <- read_rds("Data//station_ridership_24.Rds")

amtrak_stations %>%
  merge(., amtrak_ridership_24, by = "Code") %>%
  ggplot() +
  geom_sf(aes(size = ridership_24))

amtrak_gtfs_feed$stop_times %>% 
  group_by(stop_id) %>%
  summarise("count" = n()) %>% View()

