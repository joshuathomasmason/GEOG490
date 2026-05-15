# Wednesday, May 13, 2026
# Brazil Lab 8a
# Student: Joshua Mason
# Notes:

# Libraries
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(rmapshaper)
library(flextable)
options(tigris_use_cache = TRUE)

#-------------------------------------------------------------------------------

# Bring in 2019-2023 census tract data using the Census API
or.tracts <- get_acs(geography = "tract",
                     year = 2023,
                     variables = c(tpop = "B03002_001",
                                   white = "B03002_003", black = "B03002_004",
                                   asian = "B03002_006", hisp = "B03002_012"),
                     state = "OR",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)

# Calculate, rename and keep essential vars.
or.tracts <- or.tracts %>%
  mutate(pwhite = 100*(whiteE/tpopE), pasian = 100*(asianE/tpopE),
         pblack = 100*(blackE/tpopE), phisp = 100*(hispE/tpopE)) %>%
  rename(white = whiteE, asian = asianE, black = blackE,
         hisp = hispE, tpop = tpopE) %>%
  select(GEOID,tpop, pwhite, pasian, pblack, phisp,
         white, asian, black, hisp)

# Bring in city boundaries
pl <- places(state = "OR", year = 2023, cb = TRUE)

# Keep four large cities in OR
large.cities <- pl %>%
  filter(NAME == "Portland" |
           NAME == "Eugene" | NAME == "Salem" |
           NAME == "Hillsboro")

#Clip tracts in large cities
large.tracts <- ms_clip(target = or.tracts,
                        clip = large.cities,
                        remove_slivers = TRUE)

# Examine results
glimpse(large.tracts)

# Join variables from large.cities to object large.tracts
sf_use_s2(FALSE) # Disabling s2 geometry because it is too strict for data

large.tracts <- large.tracts %>%
  st_join(large.cities)

names(large.tracts)

# Mapping Percent Hispanic
large.tracts %>%
  filter(NAME == "Hillsboro") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "phisp",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent Hispanic in Hillsboro City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)

# Mapping percent white
large.tracts %>%
  filter(NAME == "Hillsboro") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = "pwhite",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_title("Percent White in Hillsboro City Tracts, 2019-2023") +
  tm_layout(scale = 0.6, frame = FALSE)

# Comparison
large.tracts %>%
  filter(NAME == "Hillsboro") %>%
  tm_shape(unit = "mi") +
  tm_polygons(fill = c("phisp", "pwhite"),
              fill.scale = tm_scale(style = "quantile",
                                    values = "brewer.reds"),
              fill.legend = tm_legend(title = ""),
              col_alpha = 0) +
  tm_scalebar(breaks = c(0, 1, 2),
              text.size = 0.75,
              position = tm_pos_in("right", "bottom")) +
  tm_compass(type = "4star", position = tm_pos_in("right", "top")) +
  tm_layout(scale = 0.6, frame = FALSE)

# Dissimilarity Index
large.tracts <- large.tracts %>%
  group_by(NAME) %>%
  mutate(whitec = sum(white), asianc = sum(asian),
         blackc = sum(black), hispc = sum(hisp),
         tpopc = sum(tpop))

large.tracts <- large.tracts %>%
  mutate(d.wb = abs(black/blackc-white/whitec),
         d.wa = abs(asian/asianc-white/whitec),
         d.wh = abs(hisp/hispc-white/whitec))

large.tracts %>%
  summarize(BWD = 0.5*sum(d.wb, na.rm=TRUE), AWD = 0.5*sum(d.wa, na.rm=TRUE),
            HWD = 0.5*sum(d.wh, na.rm=TRUE))

dis.table <- large.tracts %>%
  summarize(BWD = 0.5*sum(d.wb, na.rm=TRUE), AWD = 0.5*sum(d.wa, na.rm=TRUE),
            HWD = 0.5*sum(d.wh, na.rm=TRUE)) %>%
  st_drop_geometry() %>%
  flextable()
dis.table %>%
  colformat_double(j = c("BWD", "AWD", "HWD"), digits = 3)

#-------------------------------------------------------------------------------
# Location Quotient
hillsboro.tracts <- large.tracts %>%
  filter(NAME == "Hillsboro") %>%
  mutate(blklq = (black/tpop)/(blackc/tpopc),
         asnlq = (asian/tpop)/(asianc/tpopc),
         hisplq = (hisp/tpop)/(hispc/tpopc),
         whitelq = (white/tpop)/(whitec/tpopc))

hillsboro.tracts %>%
  ggplot() +
  geom_histogram(mapping = aes(x=blklq), na.rm=TRUE) +
  xlab("Black Location Quotient")

tmap_mode("view")

tm_shape(hillsboro.tracts, unit = "mi") +
  tm_polygons(fill = "blklq",
              fill.scale = tm_scale(style = "quantile",
                                    values = "reds"),
              fill.legend = tm_legend(title = "Black Location Quotient"))
