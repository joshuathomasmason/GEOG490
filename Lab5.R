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