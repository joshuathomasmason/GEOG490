# Wednesday, April 29, 2026
# Lab 5: Spatial Analysis & Modelling in R.
# Student: Joshua Mason
# Notes:

# Libraries

#-------------------------------------------------------------------------------

# Acquiring Kansas City metropolitan area, tracts in both Kansas and Missouri
library(tigris)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)

# CRS used: NAD83(2011) Kansas Regional Coordinate System
# Zone 11 (For Kansas City)
ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)

kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)

# Plotting with ggplot2
ggplot() +
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

# Spatial sub-setting, uses extent of first data set + extracts features
# Keyword: Uses co-location defined by a SPATIAL PREDICATE
# Uses default spatial predicate, st_intersects().
kc_tracts <- ks_mo_tracts[kc_metro, ]

# Subsetting returns all tracts that intersect the extent of Kansas City metro area
ggplot() +
  geom_sf(data = kc_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

# Similar process using st_filter() in sf to more cleanly subset + st_within(),
# the preferred predicate for most Census analysts
kc_tracts_within <- ks_mo_tracts %>%
  st_filter(kc_metro, .predicate = st_within)

# Equivalent syntax:
# kc_metro2 <- kc_tracts[kc_metro, op = st_within]
ggplot() +
  geom_sf(data = kc_tracts_within, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

#-------------------------------------------------------------------------------

# Hypothetical health data analysis for Gainesville, FL, percentage of residents 65 and up
# who are lacking health insurance in patients' neighborhoods

# patients w/ patient ID and lat lon data
library(tidycensus)
library(mapview)

gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.38177, -82.259461,
                -82.361748, -82.367436, -82.374377, -82.404031,
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576,
               29.677201, 29.674923, 29.71099, 29.711587,
               29.648227, 29.624037)
)

# Preparing as spatial data set

# CRS: NAD83(2011) / Florida North
gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440)

# Map with mapview
mapview(
  gainesville_sf,
  col.regions = "red",
  legend = FALSE
)

# Getting ACS data on health insurance
alachua_insurance <- get_acs(
  geography = "tract",
  variables = "DP03_0096P",
  state = "FL",
  county = "Alachua",
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate,
         pct_insured_moe = moe) %>%
  st_transform(6440)

# Mapping with mapview
mapview(
  alachua_insurance,
  zcol = "pct_insured",
  layer.name = "% with health<br/>insurance"
) +
  mapview(
    gainesville_sf,
    col.regions = "red",
    legend = FALSE
  )

# Spatial join fo patient data, look at the tibble and how it has changed
patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance
)

#-------------------------------------------------------------------------------

# Analyzing distributions of neighborhoods (Census tracts) by Hispanic population in Texas

# CRS: NAD83(2011) / Texas Centric Albers Equal Area
tx_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)

# Data is all inside state of Texas, so we can obtain data on percent Hispanic by tract from ACS
pct_hispanic <- get_acs(
  geography = "tract",
  variables = "DP05_0071P",
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)

# Spatial join
hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE
)

# Plotting data
hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
                                  ggplot() +
                                    geom_density(aes(x = estimate_tracts), color = "navy", fill = "navy",
                                                 alpha = 0.4) +
                                    theme_minimal() +
                                    facet_wrap(~NAME_metro) +
                                    labs(title = "Distribution of Hispanic/Latino population by Census tract",
                                         subtitle = "Largest metropolitan areas in Texas",
                                         y = "Kernel density estimate",
                                         x = "Percent Hispanic/Latino in Census tract")

# "Rolling-up" data to larger geography with group-wise data analysis,
# finding median value of 4 distributions in previous plot
median_by_metro <- hispanic_by_metro %>%
  group_by(NAME_metro) %>%
  summarize(median_hispanic = median(estimate_tracts, na.rm = TRUE))

# Plotting, this is the extent of the given metro area, this is a very useful workflow
plot(median_by_metro[1,]$geometry)

#-------------------------------------------------------------------------------

# Experimenting with areal interpolation, fixing temporal constrains of Census tracts

# CRS: NAD 83 / Arizona Central
wfh_15 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2015,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  select(estimate) %>%
  st_transform(26949)

wfh_20 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2020,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  st_transform(26949)

# Area-weighted interpolation with sf st_interpolate_aw() function
# Uses overlap of geometries as interpolation weights, estimating 2020 Gilbert Census tract
wfh_interpolate_aw <- st_interpolate_aw(
  wfh_15,
  wfh_20,
  extensive = TRUE
) %>%
  mutate(GEOID = wfh_20$GEOID)

# Population-weighted interpolation using interpolate_pw() function
# Maricopa, AZ, blocks data
maricopa_blocks <- blocks(
  state = "AZ",
  county = "Maricopa",
  year = 2020
)

wfh_interpolate_pw <- interpolate_pw(
  wfh_15,
  wfh_20,
  to_id = "GEOID",
  extensive = TRUE,
  weights = maricopa_blocks,
  weight_column = "POP20",
  crs = 26949
)

# Mapping
library(mapboxapi)

wfh_shift <- wfh_20 %>%
  left_join(st_drop_geometry(wfh_interpolate_pw),
            by = "GEOID",
            suffix = c("_2020", "_2015")) %>%
  mutate(wfh_shift = estimate_2020 - estimate_2015)

maricopa_basemap <- layer_static_mapbox(
  location = wfh_shift,
  style_id = "dark-v9",
  username = "mapbox"
)

ggplot() +
  maricopa_basemap +
  geom_sf(data = wfh_shift, aes(fill = wfh_shift), color = NA,
          alpha = 0.8) +
  scale_fill_distiller(palette = "PuOr", direction = -1) +
  labs(fill = "Shift, 2011-2015 to\n2016-2020 ACS",
       title = "Change in work-from-home population",
       subtitle = "Maricopa County, Arizona") +
  theme_void()

#-------------------------------------------------------------------------------
# 7.4 Distance and proximity analysis (SKIPPING THIS SECTION)
#-------------------------------------------------------------------------------

# 7.5 Spatial overlay
ny <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  year = 2020,
  geometry = TRUE
)
ggplot(ny) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")

# 7.5.1 Erasing areas from Census polygons
ny2 <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  geometry = TRUE,
  year = 2020,
  cb = FALSE
) %>%
  st_transform(6538)

ny_erase <- erase_water(ny2)

ggplot(ny_erase) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")

#-------------------------------------------------------------------------------

# 7.6 Spatial neighborhoods and spatial weights matrices
library(spdep)

# CRS: NAD83 / Texas North Central
dfw <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Dallas")) %>%
  st_transform(32138)
dfw_tracts <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(32138) %>%
  st_filter(dfw, .predicate = st_within) %>%
  na.omit()

# Map of median age estimates in dfw
ggplot(dfw_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  theme_void()

# Producing neighbors list (on average, tracts in dfw have roughly 6.43 neighbors)
neighbors <- poly2nb(dfw_tracts, queen = TRUE)
summary(neighbors)

# Blue line connects each polygon w/ neighbors here
dfw_coords <- dfw_tracts %>%
  st_centroid() %>%
  st_coordinates()
plot(dfw_tracts$geometry)
plot(neighbors,
     coords = dfw_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)

# Get the row indices of the neighbors of the Census tract at row index 1
neighbors[[1]]

# 7.6.2 Generating the spatial weights matrix
weights <- nb2listw(neighbors, style = "W")
weights$weights[[1]]

#-------------------------------------------------------------------------------

# 7.7 Global and local spatial autocorrelation
# This is the main rule of geography, everythign is related to everything else,
# but near things are more closely related than far things

# 7.7.1
dfw_tracts$lag_estimate <- lag.listw(weights, dfw_tracts$estimate)

ggplot(dfw_tracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  theme_minimal() +
  labs(title = "Median age by Census tract, Dallas-Fort Worth TX",
       x = "Median age",
       y = "Spatial lag, median age",
       caption = "Data source: 2016-2020 ACS via the tidycensus R package.")

moran.test(dfw_tracts$estimate, weights)

# 7.7.2 Local spatial autocorrelation (BROKEN)

# For Gi*, re-compute the weights with`include.self()`
localg_weights <- nb2listw(include.self(neighbors))

dfw_tracts$localG <- localG(dfw_tracts$estimate, localg_weights)

#ggplot(dfw_tracts) +
 # geom_sf(aes(fill = localG), color = NA) +
 # scale_fill_distiller(palette = "RdYlBu") +
 # theme_void() +
 # labs(fill = "Local Gi* statistic")

dfw_tracts <- dfw_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.56 ~ "High cluster",
    localG <= -2.56 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

ggplot(dfw_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  theme_void()

# 7.7.3 Identifying clusters and spatial outliers with local indicators of spatial association (LISA)
set.seed(1983)

dfw_tracts$scaled_estimate <- as.numeric(scale(dfw_tracts$estimate))

dfw_lisa <- localmoran_perm(
  dfw_tracts$scaled_estimate,
  weights,
  nsim = 999L,
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))

dfw_lisa_df <- dfw_tracts %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(dfw_lisa)

dfw_lisa_clusters <- dfw_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"
  ))

# LISA quadrant plot
color_values <- c(`High-high`
                  = "red",
                  `High-low`
                  = "pink",
                  `Low-low`
                  = "blue",
                  `Low-high`
                  = "lightblue",
                  `Not significant`
                  = "white")
ggplot(dfw_lisa_clusters, aes(x = scaled_estimate,
                              y = lagged_estimate,
                              fill = lisa_cluster)) +
  geom_point(color = "black", shape = 21, size = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = color_values) +
  labs(x = "Median age (z-score)",
       y = "Spatial lag of median age (z-score)",
       fill = "Cluster type")

ggplot(dfw_lisa_clusters, aes(fill = lisa_cluster)) +
  geom_sf(size = 0.1) +
  theme_void() +
  scale_fill_manual(values = color_values) +
  labs(fill = "Cluster type")