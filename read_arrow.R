library(tidyverse)

read_arrow <- function(filename) {
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
    select(StationCode, ScheduleDepartureDay, S)
  df <- df[-c(1),]
  df
}
read_arrow("test_file.txt")