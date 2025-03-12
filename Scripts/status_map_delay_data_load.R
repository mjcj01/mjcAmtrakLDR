library(plyr)

# zipped_folders <- list.files(path = "Data//StatusMap Delay Data//2024", pattern = "*.zip", full.names = TRUE)
# 
# ldply(.data = zipped_folders, .fun = unzip, exdir = "Data//StatusMap Delay Data//2024 Unzipped Data")
# 
# unlink("Data//StatusMap Delay Data//2024 Unzipped Data/logs", recursive = TRUE)

source("Scripts//read_arrow.R")

delay_data_folders_2024 <- list.dirs("Data//StatusMap Delay Data//2024 Unzipped Data")

read_arrow_wrapper <- function(filename) {
  tryCatch(read_arrow(filename),
           error = function(e) NULL)
}

delay_data <- NULL
for (i in delay_data_folders_2024) {
  files <- list.files(path = i, pattern = ".txt", full.names = TRUE)
  files <- files[-1]
  df <- ldply(.data = files, .fun = read_arrow_wrapper)
  delay_data <- rbind(delay_data, df)
  
  print(paste("Completed", i, sep = " "))
}

