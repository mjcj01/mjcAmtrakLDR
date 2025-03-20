library(tidyverse)
library(sf)
library(geosphere)

station_coords <- amtrak_stations %>%
  mutate(amtrak_coords.lon = st_coordinates(geometry)[,1],
         amtrak_coords.lat = st_coordinates(geometry)[,2]) %>%
  as.data.frame() %>%
  select(Code, amtrak_coords.lon, amtrak_coords.lat)

w_isochrones_bg_coords <- w_isochrone_bg_centroids_fix %>%
  mutate(bg_coords.lon = st_coordinates(geometry)[,1],
         bg_coords.lat = st_coordinates(geometry)[,2]) %>%
  as.data.frame() %>%
  select(GEOID, id, P2_001N, bg_coords.lon, bg_coords.lat)

d_isochrones_bg_coords <- d_isochrone_bg_centroids_fix %>%
  mutate(bg_coords.lon = st_coordinates(geometry)[,1],
         bg_coords.lat = st_coordinates(geometry)[,2]) %>%
  as.data.frame() %>%
  select(GEOID, id, P2_001N, bg_coords.lon, bg_coords.lat)

station_bg_coord_merge <- merge(station_coords,
                                d_isochrones_bg_coords,
                                by.x = "Code",
                                by.y = "id") %>%
  mutate("lon_distance" = amtrak_coords.lon - bg_coords.lon,
         "lat_distance" = amtrak_coords.lat - bg_coords.lat,
         "deg_distance" = sqrt((lon_distance^2) + (lat_distance^2)),
         "mi_distance" = deg_distance * 69.4) %>%
  select(-contains("amtrak")) %>%
  st_as_sf(coords = c("bg_coords.lon", "bg_coords.lat")) %>%
  filter(mi_distance <= 3) %>%
  merge(., station_data %>% as.data.frame() %>% select(-geometry),
        by.x = "Code", by.y = "id") %>%
  mutate("cont_rdrs" = (rdrs_24/count)/T_POP)

station_bg_coord_merge %>%
  filter(on_rt_n == 1) %>%
  group_by(Code) %>%
  slice(which.max(P2_001N)) %>%
  ggplot(aes(y = cont_rdrs, x = mi_distance)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)


