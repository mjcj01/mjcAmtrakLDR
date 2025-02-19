library(tidyverse)
library(sf)

ggplot() +
  geom_sf(data = night_routes %>%
            filter(one_route_only == TRUE),
          aes(color = as.factor(pct_night_stops))) +
  scale_color_brewer()
