library(tidyverse)

source("Scripts//data_load.R")
source("Scripts//amtrak_gtfs_ldr_filter.R")

delay_data_class <- delay_data %>%
  filter(StationCode %in% amtrak_gtfs_feed$stops$stop_id) %>%
  mutate("late_class" = ifelse(sec_diff == 0, "not_late",
                               ifelse(sec_diff > 0 & sec_diff <= 600, "min_late", "late")))

delay_data_pct <- delay_data_class %>%
  group_by(StationCode, late_class) %>%
  reframe("count" = n()) %>%
  group_by(StationCode) %>%
  reframe("pct" = count / sum(count),
          "late_class" = late_class)

delay_data_pct_train <- delay_data %>%
  filter(StationCode %in% amtrak_gtfs_feed$stops$stop_id) %>%
  mutate("late_class" = ifelse(sec_diff == 0, "not_late",
                               ifelse(sec_diff > 0 & sec_diff <= 600, "min_late", "late"))) %>%
  group_by(train_number, late_class, StationCode) %>%
  reframe("count" = n()) %>%
  group_by(train_number, StationCode) %>%
  reframe("pct" = (count / sum(count)),
          "late_class" = late_class,
          "obs" = n())

delay_data_pct_train$pct[is.na(delay_data_pct_train$pct)] <- 0