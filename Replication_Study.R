# Wednesday, May 6, 2026
# Replication Study
# Student: Joshua Mason
# Notes:

# Libraries
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
library(crsuggest)
# library(spdep)
options(tigris_use_cache = TRUE)

#-------------------------------------------------------------------------------

# Acquiring Austin-Round Rock-Georgetown metropolitan area
# Tracts in TX
# Using suggest_crs() to find appropriate projection
tx_counties <- counties("TX", cb = TRUE)

tx_crs <- suggest_crs(tx_counties)

# CRS used: NAD83(2011) / Texas Centric Lambert Conformal
tx_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Austin-Round Rock-Georgetown")) %>%
  st_transform(6580)

# Tracts with total population from 2020 decennial Census
# Also uses spatial sub-setting with spatial predicate st_within()
# ALAND column uses square meters as measurement
tx_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "TX",
  year = 2020,
  geometry = TRUE,
  keep_geo_vars = TRUE
) %>%
  st_transform(6580) %>%
  st_filter(tx_metro, .predicate = st_within) %>%
  na.omit()

# Map of Austin-Round Rock-Georgetown metropolitan area (503 total tracts within)
ggplot() +
  geom_sf(data = tx_tracts, fill = "white", color = "grey") +
  geom_sf(data = tx_metro, fill = NA, color = "blue") +
  theme_void()

# Population density, people per square meter
tx_tracts <- tx_tracts %>%
  mutate(pop_dens = (tx_tracts$value) / (tx_tracts$ALAND))

# Tabulating new groups with Hanberry thresholds
tx_tracts_recode <- tx_tracts %>%
  mutate(pop_class = case_when(
    between(value, 0, 250) ~ "Exurban",
    between(value, 250, 550) ~ "SuburbanLow",
    between(value, 550, 800) ~ "SuburbanHight",
    between(value, 800, 1900) ~ "UrbanLow",
    TRUE ~ "UrbanHigh"
  ))