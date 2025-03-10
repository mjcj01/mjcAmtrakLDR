library(tidyverse)
library(sf)

amtrak_delays_2022_23$sec_diff[is.na(amtrak_delays_2022_23$sec_diff)] <- 0

amtrak_delays_2022_23 %>%
  group_by(station) %>%
  reframe("avg_delay" = (mean(sec_diff)) / 60,
          "obs" = n()) %>%
  filter(obs > 30) %>%
  ggplot(aes(x = obs, y = avg_delay)) +
  geom_point()
