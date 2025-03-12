library(tidyverse)

read_arrow <- function(filename) {
  date <- as.Date(substring(gsub(".txt", "", filename), 
                            nchar(filename) - 11), "%Y%m%d")
  train_no <- gsub("\\/.*", "", gsub("Data//StatusMap Delay Data//2024 Unzipped Data/", "", filename))
  data <- filename
  data_lines <- readLines(data)
  data_lines <- data_lines[-c(1)]
  v_line_index <- which(grepl("^\\* V", data_lines))
  
  data_col_name_lines <- data_lines[1:(v_line_index - 1)]
  col_names <- gsub("[\\* \\+\\|-]", "", data_col_name_lines)
  col_names <- c("star", col_names)
  
  v_line_index_chars <- strsplit(data_lines[v_line_index], "")[[1]]
  v_line_v_indexes <- which(v_line_index_chars %in% c("*", "V"))
  widths <- c(diff(v_line_v_indexes), 100)
  
  df <- read.fwf(data,
                 widths = widths,
                 header = FALSE,
                 skip = 9,
                 col.names = col_names) %>% 
    mutate("star" = gsub(" ", "", star)) %>% 
    filter(star == "*") %>%
    select(-star)
  df <- df[-c(1),]
  df <- df %>%
    mutate(ScheduleDepartureTime = gsub(" ", "", ScheduleDepartureTime)) %>%
    mutate(ScheduleDepartureTime = ifelse(nchar(ScheduleDepartureTime) <= 4, 
                                          paste("0", ScheduleDepartureTime, "M", sep = ""),
                                          paste(ScheduleDepartureTime, "M", sep = ""))) %>%
    mutate(ActualDepartureTime = gsub(" ", "", ActualDepartureTime)) %>%
    mutate(ActualDepartureTime = ifelse(nchar(ActualDepartureTime) <= 4, 
                                        paste("0", ActualDepartureTime, "M", sep = ""),
                                        paste(ActualDepartureTime, "M", sep = ""))) %>%
    filter(!grepl("\\*", ScheduleDepartureTime) & ActualDepartureTime != "0M") %>%
    mutate("ScheduleDepartureDay" = gsub(" ", "", ScheduleDepartureDay),
           "adjusted_day" = as.numeric(ScheduleDepartureDay) - 1,
           "SchDepartureDate" = date + adjusted_day)
  
  stringi::stri_sub(df$ScheduleDepartureTime, 3, 2) <- ":"
  stringi::stri_sub(df$ActualDepartureTime, 3, 2) <- ":"
  
  df <- df %>%
    mutate(Comments = ifelse(grepl("Arrived", Comments), gsub(".*\\|", "", Comments), Comments),
           comments = gsub("[^0-9,-]", "", Comments)) %>%
    select(-Comments) %>%
    mutate(comments = as.numeric(comments))
  
  df$comments[is.na(df$comments)] <- 0
  
  df <- df %>%
    mutate(hour_diff = ifelse(grepl(",", comments), as.numeric(gsub("\\,.*", "", comments)), 0),
           min_diff = ifelse(grepl(",", comments), as.numeric(gsub(".*\\,", "", comments)), comments),
           min_diff = ifelse(is.na(min_diff), 0, as.numeric(min_diff)),
           sec_diff = (hour_diff * 60 * 60) + (min_diff * 60),
           sec_diff = ifelse(late_check == "late", sec_diff * 1, sec_diff * -1),
           sch_dep_dt = as.POSIXct(paste(SchDepartureDate, ScheduleDepartureTime, sep = " "), format = "%Y-%m-%d %I:%M%p"),
           act_dep_dt = as.POSIXct(sch_dep_dt + sec_diff, format = "%Y-%m-%d %I:%M%p"))
  
  df %>% 
    select(StationCode, sec_diff, sch_dep_dt, act_dep_dt) %>%
    mutate(train_number = train_no)
}
read_arrow("Data//StatusMap Delay Data//2024 Unzipped Data/665/665_20241102.txt")
