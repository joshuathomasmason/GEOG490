# Thursday, May 13, 2026
# Lab 7 Submission: Segregation, Diversity & Modelling in R.
# Student: Joshua Mason
# Notes:

# Libraries
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
library(crsuggest)
library(segregation)
library(tmap)
options(tigris_use_cache = TRUE)

#-------------------------------------------------------------------------------
# MULTI-GROUP SEGREGATION INDICES FOR PITTSBURGH, PA

# Using suggest_crs()
pa_counties <- counties("PA", cb = TRUE)
pa_crs <- suggest_crs(pa_counties)

# CRS: NAD83(2011) / Pennsylvania South
pa_metro <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  st_transform(6565)

# Bring in 2019-2023 census tract data using the Census API
pa_tracts <- get_acs(geography = "tract",
                     year = 2023,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "PA",
                     survey = "acs5",
                     geometry = TRUE) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

# Computing dissimilarity index between non-Hispanic white and Hispanic popul-
# ations for the SF/Oakland urbanized area
pa_tracts %>%
  filter(variable %in% c("white", "hisp")) %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )

# Computing Mutual Information Index (M) and Theil's Entropy Index (H)
# using mutual_within()
seg_data <- pa_tracts %>%
  filter(variable %in% c("white", "black", "asian", "hisp"))

mutual_within(
  data = seg_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "NAME",
  wide = TRUE
)

# Using mutual_local() to devcompose M into unit-level segregation scores (ls)
# Examining segregation patterns across Pittsburgh MSA
pa_local_seg <- seg_data %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
  )

joined_local_seg <- seg_data %>%
  left_join(pa_local_seg, by = "GEOID")

# Note: If erase water is slow, try larger area threshold value
seg_erase_water <- erase_water(joined_local_seg)

# Multi-group segregation index for Pittsburgh, PA
ggplot(seg_erase_water) +
  geom_sf(aes(fill = ls), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void() +
  labs(fill = "Local\nsegregation index")

#-------------------------------------------------------------------------------
# LOCATION QUOTIENT FOR PITTSBURGH, PA

# Location Quotient
pa_tracts_wide <- get_acs(geography = "tract",
                     year = 2023,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "PA",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

pa_tracts_wide <- pa_tracts_wide %>%
  mutate(whitec = sum(whiteE), asianc = sum(asianE),
         blackc = sum(blackE), hispc = sum(hispE),
         tpopc = sum(tpopE))

pittsburgh_quotient <- pa_tracts_wide %>%
  mutate(blklq = (blackE/tpopE)/(blackc/tpopc),
         asnlq = (asianE/tpopE)/(asianc/tpopc),
         hisplq = (hispE/tpopE)/(hispc/tpopc),
         whitelq = (whiteE/tpopE)/(whitec/tpopc))

pittsburgh_quotient %>%
  ggplot() +
  geom_histogram(mapping = aes(x=asnlq), na.rm=TRUE) +
  xlab("Asian Location Quotient")

# Map of Asian Location Quotient in Pittsburgh, PA
tmap_mode("view")

tm_shape(pittsburgh_quotient, unit = "mi") +
  tm_polygons(fill = "asnlq",
              fill.scale = tm_scale(style = "quantile",
                                    values = "brewer.blues"),
              fill.legend = tm_legend(title = "Asian Location Quotient"))

# Map with toggles
tmap_mode("view")

tm_shape(pittsburgh_quotient, unit = "mi") +
  
  tm_polygons(
    fill = "asnlq",
    group = "Asian LQ",
    fill.scale = tm_scale(
      style = "quantile",
      values = "brewer.blues"
    ),
    fill.legend = tm_legend(title = "Asian Location Quotient")
  ) +
  
  tm_polygons(
    fill = "blklq",
    group = "Black LQ",
    fill.scale = tm_scale(
      style = "quantile",
      values = "brewer.reds"
    ),
    fill.legend = tm_legend(title = "Black Location Quotient")
  ) +
  
  tm_polygons(
    fill = "whitelq",
    group = "White LQ",
    fill.scale = tm_scale(
      style = "quantile",
      values = "brewer.greys"
    ),
    fill.legend = tm_legend(title = "White Location Quotient")
  ) +
  
  tm_polygons(
    fill = "hisplq",
    group = "Hispanic LQ",
    fill.scale = tm_scale(
      style = "quantile",
      values = "brewer.greens"
    ),
    fill.legend = tm_legend(title = "Hispanic Location Quotient")
  )
