# Saturday, May 2, 2026
# Lab 5: Spatial Analysis & Modelling in R.
# Student: Joshua Mason
# Notes:

# Libraries
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
library(crsuggest)
library(spdep)
options(tigris_use_cache = TRUE)

#-------------------------------------------------------------------------------

# Acquiring Washington-Arlington-Alexandria metropolitan area
# Tracts in DC, VA, MD, WV

# CRS used: NAD83(2011) / UTM Zone 18N (For good multi-state coverage)
dc_va_md_wv_tracts <- map_dfr(c("DC", "VA", "MD", "WV"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(6347)

dc_va_md_wv_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Washington-Arlington-Alexandria")) %>%
  st_transform(6347)

# Plotting with ggplot2
ggplot() +
  geom_sf(data = dc_va_md_wv_tracts, fill = "white", color = "grey") +
  geom_sf(data = dc_va_md_wv_metro, fill = NA, color = "blue") +
  theme_void()

# Spatial sub-setting, uses extent of first data set + extracts features
# Keyword: Uses co-location defined by a SPATIAL PREDICATE
# Uses default spatial predicate, st_intersects().
dc_va_md_wv_tracts <- dc_va_md_wv_tracts[dc_va_md_wv_metro, ]

# Subsetting returns all tracts that intersect the extent of Kansas City metro area
ggplot() +
  geom_sf(data = dc_va_md_wv_tracts, fill = "white", color = "grey") +
  geom_sf(data = dc_va_md_wv_metro, fill = NA, color = "blue") +
  theme_void()

# Similar process using st_filter() in sf to more cleanly subset + st_within(),
# the preferred predicate for most Census analysts
dc_va_md_wv_tracts_within <- dc_va_md_wv_tracts %>%
  st_filter(dc_va_md_wv_metro, .predicate = st_within)

# Final map of Washington-Arlington-Alexandria metropolitan area (1,486 total tracts within)
ggplot() +
  geom_sf(data = dc_va_md_wv_tracts_within, fill = "white", color = "grey") +
  geom_sf(data = dc_va_md_wv_metro, fill = NA, color = "blue") +
  theme_void()

#-------------------------------------------------------------------------------

# Replicating erase_water() workflow for 

# Using suggest_crs()
mt_counties <- counties("MT", cb = TRUE)
st_crs(mt_counties)

mt_crs <- suggest_crs(mt_counties)

# Erasing areas from Census polygons

# CRS used: NAD83(2011) / Montana (ft)
cascade_county <- mt_counties %>%
  filter(str_detect(NAME, "Cascade")) %>% 
  st_transform(6515)

# Without water erased
ggplot(cascade_county) +
  geom_sf() +
  theme_void()

# With water erased
cascade_erase <- erase_water(cascade_county)

ggplot(cascade_erase) +
  geom_sf() +
  theme_void()

#-------------------------------------------------------------------------------

# Reproducing Getis-Ord hotspot analysis for San Francisco-Oakland-Berkeley metropolitan area

# Using suggest_crs()
ca_counties <- counties("CA", cb = TRUE)
ca_crs <- suggest_crs(ca_counties)

# CRS: NAD83(2011) / California Albers
sf_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "San Francisco")) %>%
  st_transform(6414)

# Getting ACS Median Houshold Income
sf_tracts <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CA",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(6414) %>%
  st_filter(sf_metro, .predicate = st_within) %>%
  na.omit()

# Plotting Median Houshold Income 
ggplot(sf_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  theme_void()

# Identifying neighbors
neighbors <- poly2nb(sf_tracts, queen = TRUE)
summary(neighbors)

sf_coords <- sf_tracts %>%
  st_centroid() %>%
  st_coordinates()
plot(sf_tracts$geometry)
plot(neighbors,
     coords = sf_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)

# Getting the row indices of the neighbors of the Census tract at row index 1
neighbors[[1]]

# Converting the neighbors list object into spatial weights
weights <- nb2listw(neighbors, style = "W")

weights$weights[[1]] # Tract at row index 1 has 4 neighbors, so each was given weight 0.25

# Creating new column in sf_tracts, lag_estimate, representing average income for neighbors
# of each Census tract in San Francisco-Oakland-Berkeley metropolitan area
sf_tracts$lag_estimate <- lag.listw(weights, sf_tracts$estimate)

# Scatter plot of income in metro area
ggplot(sf_tracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(title = "Median income by Census tract, San Francisco-Oakland-Berkeley CA",
       x = "Median income",
       y = "Spatial lag, Median income",
       caption = "Data source: 2016-2020 ACS via the tidycensus R package.")

# Moran test (reject null hypothesis of spatial randomness)
# Small p-value also suggests data are spatially clustered
moran.test(sf_tracts$estimate, weights)

# Getis-Ord local G statistic
# For Gi*, re-compute the weights with`include.self()`
localg_weights <- nb2listw(include.self(neighbors))

# This was a bug in the Walker chapter code. It has to be numeric to plot
sf_tracts$localG <- localG(sf_tracts$estimate, localg_weights)

# Converting to numeric
numeric_g <- as.numeric(localG(sf_tracts$estimate, localg_weights))

ggplot(sf_tracts) +
  geom_sf(aes(fill = numeric_g), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void() +
  labs(fill = "Local Gi* statistic")

# Setting hot spot thresholds
sf_tracts <- sf_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.56 ~ "High cluster",
    localG <= -2.56 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

# Plotting high and low clusters (Final Map)
ggplot(sf_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("orange", "blue", "grey")) +
  theme_void()