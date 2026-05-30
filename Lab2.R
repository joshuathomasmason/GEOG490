# Wednesday, April 8, 2026
# Lab 2: A Comparison of NHGIS and Tidycensus Workflows in R
# Student: Joshua Mason
# Note to self: Can run selected code blocks with control + return

# libraries
library(ggplot2)
library(tidycensus)
library(tidyverse)

#-----------------------------------------------------------------------

# Load total population data
TotalPopulation_2020 <- read_csv("Data/total_population_2020/nhgis0001_ds258_2020_state.csv")

# Making quick bar chart with population data
ggplot() +
  geom_col(data = TotalPopulation_2020,
           aes(x = reorder(STATE, U7H001), y = U7H001),
           fill = "darkred") + 
  coord_flip() + 
  labs(x = "State", y = "Total population", title = "Total Population by State")

# Now loading total population data using tidycensus
TotalPopulation_2020_TC <- get_decennial(geography = "state",
                                         variables = "P1_001N",
                                         year = 2020,
                                         geometry = FALSE)

# Making bar chart with tidycensus object
ggplot() +
  geom_col(data = TotalPopulation_2020_TC,
           aes(x = reorder(NAME, value), y = value),
           fill = "lightblue") + 
  coord_flip() + 
  labs(x = "State", y = "Total population", title = "Total Population by State")

#------------------------------------------------------------------------------------
# Lab 2 Submission Requirements Section: STUDY AREA — STATE OF OREGON COUNTIES

# Loading county data for Oregon
bachelors_25_over <- get_acs(
  geography = "county",
  variables = "DP02_0068P",
  state = "OR",
  year = 2019
)

# 1. Finding county with lowest percentage in Oregon NOTE: Morrow County, 9%
arrange(bachelors_25_over, estimate)

# 2. Finding county with highest percentage in Oregon NOTE: Benton County, 54.1%
arrange(bachelors_25_over, desc(estimate))

# 3. Finding median percentage for counties in Oregon NOTE: 20.9%
median(bachelors_25_over$estimate)

# 4. Using different variable, Total: income in the past 12 months below poverty level
# Data from 2019 ACS for analysis of Oregon counties
total_poverty <- get_acs(
  geography = "county",
  variables = c(poverty_count = "B17001_002"),
  state = "OR",
  year = 2019,
  survey = "acs5"
) %>%
  mutate(NAME = str_remove(NAME, " County, Oregon"))

# Examine margins of error
total_poverty %>%
  arrange(desc(moe))

# Plotting margin of error
ggplot(total_poverty, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbar(
    aes(xmin = estimate - moe,
        xmax = estimate + moe),
                orientation = "y") +
  geom_point(size = 3, color = "tomato") +
  theme_minimal(base_size = 12.5) +
  labs(title = "Total population living below poverty line",
       subtitle = "Counties in Oregon",
       x = "2015-2019 ACS estimate",
       y = "")

# Plotting histogram of different variable, total households for Oregon counties 
# NOTE: modal median of 20,000 households
options(scipen = 999)
ggplot(total_poverty, aes(x = estimate)) +
  geom_histogram(fill = "tomato")

# Adding bins to view differences
ggplot(total_poverty, aes(x = estimate)) +
  geom_histogram(fill = "tomato", bins = 60)

# Whisker plot
ggplot(total_poverty, aes(y = estimate)) +
  geom_boxplot()

# Pyramid plot getting estimate data from census population estimates API
oregon <- get_estimates(
  geography = "state",
  state = "OR",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)

# Filtering data
oregon_filtered <- filter(oregon, str_detect(AGEGROUP, "Age"),
                          SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

# Plotting on Pyramid Plot
ggplot(oregon_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Male" = "blue",
    "Female" = "orange"
  ))
