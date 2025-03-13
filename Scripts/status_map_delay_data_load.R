library(tidyverse)
library(plyr)

# zipped_folders <- list.files(path = "Data//StatusMap Delay Data//2024", pattern = "*.zip", full.names = TRUE)
# 
# ldply(.data = zipped_folders, .fun = unzip, exdir = "Data//StatusMap Delay Data//2024 Unzipped Data")
# 
# unlink("Data//StatusMap Delay Data//2024 Unzipped Data/logs", recursive = TRUE)

source("Scripts//read_arrow.R")

delay_data_folders_2024 <- list.dirs("Data//StatusMap Delay Data//2024 Unzipped Data")

delay_data <- NULL

for (i in delay_data_folders_2024) {
  files <- list.files(path = i, pattern = ".txt", full.names = TRUE)
  files <- files[-1]
  df <- ldply(.data = files, .fun = read_arrow_wrapper)
  delay_data <- rbind(delay_data, df)
  
  print(paste("Completed", i, sep = " "))
  rm(files, df)
}

station_freq <- table(delay_data$StationCode) %>%
  as.data.frame() %>%
  ### Stations with less than 160 obs are either state fair stations or code errors
  filter(Freq > 160)

delay_data <- delay_data %>%
  filter(StationCode %in% station_freq$Var1) %>%
  ### Manual checking of delays over 86,800 seconds (1+ day) revealed formatting
  ### discrepencies that caused errors in the code running as expected.
  filter(sec_diff < 86800)

write_rds(delay_data, "Data//SM_delay_data.rds")
