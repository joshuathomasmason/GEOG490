# Making contour map of Pittsburgh MSA

library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)
library(tidycensus)
library(crsuggest)
library(showtext)
#library(segregation)
#library(tmap)
options(tigris_use_cache = TRUE)

pa_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  st_transform(6565)

library(elevatr)
library(terra)

# Transform to WGS84 for elevatr
pa_metro_ll <- st_transform(pa_metro, 4326)

dem <- get_elev_raster(
  locations = pa_metro_ll,
  z = 8,
  clip = "locations"
)

dem <- rast(dem)
dem <- project(dem, "EPSG:6565")

dem_min <- global(dem, "min", na.rm = TRUE)[[1]]
dem_max <- global(dem, "max", na.rm = TRUE)[[1]]

contours <- as.contour(
  dem,
  levels = seq(
    floor(dem_min),
    ceiling(dem_max),
    by = 30
  )
)

contours_sf <- st_as_sf(contours)

ggplot() +
  geom_sf(
    data = contours_sf,
    color = "black",
    linewidth = 0.15
  ) +
  geom_sf(
    data = pa_metro,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  theme_void() +
  coord_sf()

library(tigris)
library(sf)

pittsburgh <- places("PA", year = 2020) %>%
  filter(NAME == "Pittsburgh") %>%
  st_transform(6565)

st_bbox(pittsburgh)

bbox <- st_bbox(pittsburgh)

ggplot() +
  geom_sf(
    data = contours_sf,
    color = "white",
    linewidth = 0.15
  ) +
  geom_sf(
    data = pa_metro,
    fill = NA,
    color = "white",
    linewidth = 0.5
  ) +
  coord_sf(
    xlim = c(bbox["xmin"] - 10000, bbox["xmax"] + 10000),
    ylim = c(bbox["ymin"] - 10000, bbox["ymax"] + 10000)
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = "grey85",
      color = NA
    ),
    plot.background = element_rect(
      fill = "grey85",
      color = NA
    )
  )