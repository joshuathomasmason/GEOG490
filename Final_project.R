# Wednesday, May 27, 2026
# Final Project
# Student: Joshua Mason
# Notes: This final project examines the Pittsburgh, Pennsylvania MSA

# Libraries
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
library(crsuggest)
library(showtext)
library(segregation)
library(tmap)
options(tigris_use_cache = TRUE)
font_add_google("IBM Plex Mono", "plexmono")
showtext_auto()

#-------------------------------------------------------------------------------

# ACQUIRING PITTSBURGH, PA METROPOLITAN STATISTICAL AREA BOUNDARY

# Using suggest_crs()
pa_counties <- counties("PA", cb = TRUE, year = 2020)
pa_crs <- suggest_crs(pa_counties)

# CRS: NAD83(2011) / Pennsylvania South
pa_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  st_transform(6565)

pa_counties <- pa_counties %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

county_labels <- pa_counties %>%
  st_point_on_surface()

pa_tracts <- tracts("PA", cb = TRUE, year = 2020) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

# Produce simple map of counties within Pittsburgh MSA (7 counties within)
ggplot() +
  geom_sf(data = pa_counties, fill = "white", color = "grey") +
  geom_sf(data = pa_metro, fill = NA, color = "blue") +
  geom_sf_text(data = county_labels,
               aes(label = NAME),
               family = "plexmono",
               size = 3) +
  theme_void()

# Produce simple map of tracts within Pittsburgh MSA (724 tracts within)
ggplot() +
  geom_sf(data = pa_tracts, fill = "white", color = "grey") +
  geom_sf(data = pa_metro, fill = NA, color = "blue") +
  theme_void()

#-------------------------------------------------------------------------------

# MAPPING 2020 DECENNIAL POPULATION DATA FOR COUNTIES AND TRACTS IN MSA

# Mapping county population 
county_pop <- get_decennial(geography = "county",
                            year = 2020,
                            variables = "P1_001N",
                            state = "PA",
                            geometry = TRUE,
                            keep_geo_vars = TRUE
                            ) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

ggplot(county_pop) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "2020\nCounty Population")
  
# Mapping tract population
tract_pop <- get_decennial(geography = "tract",
                            year = 2020,
                            variables = "P1_001N",
                            state = "PA",
                            geometry = TRUE,
                            keep_geo_vars = TRUE
) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

ggplot(tract_pop) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "2020\nTract Population")

# How many people live in Pittsburgh MSA? Answer: 2,370,930
sum(tract_pop$value)

# Making map of tracts in Pittsburgh City proper
pittsburgh <- places("PA", cb = TRUE, year = 2020) %>%
  filter(NAME == "Pittsburgh") %>%
  st_transform(6565)

ggplot() +
  geom_sf(data = tract_pop,
          aes(fill = value),
          color = NA) +
  geom_sf(
    data = pittsburgh,
    fill = NA,
    color = "orange",
    linewidth = 0.6
  ) + scale_fill_distiller(palette = "Blues",
                           direction = 1,
                           guide = "none"
  ) + theme_void(base_family = "plexmono")

# Plain map showing Pittsburgh city inside MSA
ggplot() +
  geom_sf(data = pa_metro,
          fill = "lightgray",
          color = "lightgray") +
  geom_sf(
    data = pittsburgh,
    fill = NA,
    color = "white",
    linewidth = 0.6
    ) + 
  theme_void() +
  theme(legend.position = "none")

# Examining tracts only within Pittsburgh proper
proper_tracts <- tract_pop %>%
  st_filter(pittsburgh, .predicate = st_within)

ggplot(proper_tracts) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "2020\nTract Population")

#-------------------------------------------------------------------------------

# MAPPING REDLINED AREAS IN PITTSBURGH, PA

# Redlining data from local file
# downloaded from https://dsl.richmond.edu/panorama/redlining/data/PA-Pittsburgh
# redlining_data <- st_layers("mappinginequality.gpkg")

#-------------------------------------------------------------------------------

# MAPPING MEDIAN HOUSHOLD INCOME IN THE LAST 12 MONTHS IN PITTSBURGH MSA

# Getting ACS Median Household Income for tracts
pa_median_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "PA",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

# Creating bins for income
pa_income_groups <- pa_median_income %>%
  mutate(
    income_group = case_when(
      between(estimate, 0, 10000) ~ "0 to 10,000",
      between(estimate, 10000, 25000) ~ "10,000 to 25,000",
      between(estimate, 25000, 50000) ~ "25,000 to 50,000",
      between(estimate, 50000, 75000) ~ "50,000 to 75,000",
      between(estimate, 75000, 100000) ~ "75,000 to 100,000",
      between(estimate, 100000, 125000) ~ "100,000 to 125,000",
      between(estimate, 125000, 150000) ~ "125,000 to 150,000",
      between(estimate, 150000, 200000) ~ "150,000 to 200,000",
      between(estimate, 200000, 250000) ~ "200,000 to 250,000",
      estimate > 250000 ~ "> 250,000"
    )
  )

# Plotting income on bar chart
ggplot(pa_income_groups, aes(x = estimate, y = income_group)) +
  geom_col()

# Mapping median household income
ggplot(pa_median_income) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "Median Household\nIncome Estimate")

#-------------------------------------------------------------------------------

# RACE/ETHNICITY AND SEGREGATION IN PITTSBURGH MSA

# Acquiring data on race/ethinicty from 5-year ACS
pa_race <- get_acs(geography = "tract",
                     year = 2020,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "PA",
                     survey = "acs5",
                     geometry = TRUE) %>%
  st_transform(6565) %>%
  st_filter(pa_metro, .predicate = st_within) %>%
  na.omit()

# Computing Mutual Information Index (M) and Theil's Entropy Index (H)
# using mutual_within()
seg_data <- pa_race %>%
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
#seg_erase_water <- erase_water(joined_local_seg)

# Multi-group segregation index for Pittsburgh, PA MSA
ggplot(joined_local_seg) +
  geom_sf(aes(fill = ls), color = NA) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "Local\nsegregation index")

#-------------------------------------------------------------------------------

# RACE/ETHNICITY LOCATION QUOTIENTS FOR PITTSBURGH, PA MSA

# Location Quotient
pa_tracts_wide <- get_acs(geography = "tract",
                          year = 2020,
                          variables = c(tpop = "B03002_001",
                                        white = "B03002_003",
                                        black = "B03002_004",
                                        asian = "B03002_006",
                                        hisp = "B03002_012"),
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

# Map of all LQ's with toggles in Pittsburgh MSA
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

#-------------------------------------------------------------------------------

# DEFINING URBAN SPACE WITH HANBERRY THRESHOLDS

# Population density, people per square kilometer
# ALAND / 1e6 because ALAND default values are in square meters
pa_tracts_dens <- tract_pop %>%
  mutate(pop_dens = (tract_pop$value) / (tract_pop$ALAND / 1e6))

# Tabulating new groups with Hanberry LandScan thresholds
pa_tracts_recode <- pa_tracts_dens %>%
  mutate(pop_class = case_when(
    between(pop_dens, 0, 550) ~ "Exurban",
    between(pop_dens, 550, 1000) ~ "SuburbanLow",
    between(pop_dens, 1000, 1900) ~ "SuburbanHigh",
    between(pop_dens, 1900, 4500) ~ "UrbanLow",
    pop_dens > 4500 ~ "UrbanHigh"
  ),
  pop_class = factor(
    pop_class,
    levels = c(
      "Exurban",
      "SuburbanLow",
      "SuburbanHigh",
      "UrbanLow",
      "UrbanHigh"
    ),
    ordered = TRUE # Fixes issue with ggplot putting UrbanHigh before UrbanLow
  ))

# Map of Hanberry Threshold subgeographies
ggplot(pa_tracts_recode) +
  geom_sf(aes(fill = pop_class), color = NA) +
  scale_fill_brewer(
    palette = "Blues",
    direction = 1
  ) +
  theme_void(base_family = "plexmono") +
  labs(fill = "Hanberry\nPopulation Thresholds")