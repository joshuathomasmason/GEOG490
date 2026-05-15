# Wednesday, May 13, 2026
# Lab 7: Segregation, Diversity & Modelling in R.
# Student: Joshua Mason
# Notes:

# Libraries
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
library(segregation)
# library(crsuggest)
# library(spdep)
options(tigris_use_cache = TRUE)

#-------------------------------------------------------------------------------

# Getting California tract data for race/ethnicity
ca_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ),
  state = "CA",
  geometry = TRUE,
  year = 2019
)

# Using tidycensus to get urbanized areas by pop with geometry
# Filter for those with populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,
                                    fixed(", CA Urbanized Area (2010)")))

# Computing inner spatial join between CA tracts + urbanized areas, returning
# tracts in largest CA urban areas with urban_name column appended
ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()

# Computing dissimilarity index between non-Hispanic white and Hispanic popul-
# ations for the SF/Oakland urbanized area
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "San Francisco--Oakland") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )

# Filtering data for non-Hispannic white and Hispanic populations by Census tra-
# ct, then grouping data by values in urban_name column.
# Uses group_modify() to calculate dissimalarity index by group, urban areas
ca_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>%
  arrange(desc(est))

#-------------------------------------------------------------------------------
# MULTI-GROUP SEGREGATION INDICES
# Computing Mutual Information Index (M) and Theil's Entropy Index (H)
# using mutual_within()
mutual_within(
  data = ca_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)

# Using mutual_local() to devcompose M into unit-level segregation scores (ls)
# Examining segregation patterns across LA
la_local_seg <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )

# Joining data to dataset of Census tracts from tigris with inner_join() to 
# retain tracts for only LA area
la_tracts_seg <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_local_seg, by = "GEOID")

la_tracts_seg %>%
  ggplot(aes(fill = ls)) +
  geom_sf(color = NA) +
  coord_sf(crs = 26946) +
  scale_fill_viridis_c(option = "inferno") +
  theme_void() +
  labs(fill = "Local\nsegregation index")

# Calculating entropy for each Census tract
la_entropy <- ca_urban_data %>%
  filter(urban_name == "Los Angeles--Long Beach--Anaheim") %>%
  group_by(GEOID) %>%
  group_modify(~data.frame(entropy = entropy(
    data = .x,
    group = "variable",
    weight = "estimate",
    base = 4)))

la_entropy_geo <- tracts("CA", cb = TRUE, year = 2019) %>%
  inner_join(la_entropy, by = "GEOID")

# Using mapbox for travel times from LA core
library(mapboxapi)

la_city_hall <- mb_geocode("City Hall, Los Angeles CA")

minutes_to_downtown <- mb_matrix(la_entropy_geo, la_city_hall)
# Plotting
la_entropy_geo$minutes <- as.numeric(minutes_to_downtown)

ggplot(la_entropy_geo, aes(x = minutes_to_downtown, y = entropy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 80)) +
  labs(title = "Diversity gradient, Los Angeles urbanized area",
       x = "Travel-time to downtown Los Angeles in minutes, Census tracts",
       y = "Entropy index")