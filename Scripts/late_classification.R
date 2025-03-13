library(tidyverse)

delay_data %>%
  filter(StationCode %in% amtrak_gtfs_feed$stops$stop_id) %>%
  mutate("late_class" = ifelse(sec_diff == 0, "not_late",
                        ifelse(sec_diff > 0 & sec_diff <= 600, "min_late", "late"))) %>%
  group_by(StationCode, late_class) %>%
  reframe("count" = n()) %>%
  group_by(StationCode) %>%
  reframe("pct" = count / sum(count),
          "late_class" = late_class) %>%
  #pivot_wider(names_from = late_class, values_from = pct) %>%
  ggplot(aes(x = pct, fill = late_class)) + 
  geom_density(alpha = 0.3)