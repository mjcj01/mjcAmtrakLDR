library(tidyverse)
library(sf)

w_isochrone_bg_centroids_fix <- w_isochrone_bg_centroids %>%
  select(-id, -time) %>%
  st_join(., walking_isochrone) %>%
  drop_na(id)

d_isochrone_bg_centroids_fix <- d_isochrone_bg_centroids %>%
  select(-id, -time) %>%
  st_join(., driving_isochrone) %>%
  drop_na(id)
