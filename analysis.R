library(tidyverse)

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# introduction and summary information
# share at least 5 relevant values of interest, how these measure of incarceration vary by race

# average value of aapi jail population rate across all the counties in 2018
mean_aapi_jail_rate_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(aapi_jail_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE)) %>%
  pull(aapi_jail_pop_rate)

# average value of aapi jail population rate across all the counties in 2008
mean_aapi_jail_rate_2008 <- incarceration %>%
  filter(year == 2008) %>%
  summarize(aapi_jail_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE)) %>%
  pull(aapi_jail_pop_rate)

# trend of average value of aapi jail population rate across all the counties from 2008 to 2018
trend_mean_aapi_jail_rate <- incarceration %>%
  filter(year >= 2008 & year <= 2018) %>%
  group_by(year) %>%
  summarize(aapi_jail_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  select(year, aapi_jail_pop_rate)

# location(s) and value where highest value of aapi jail incarceration rate in 2018
highest_aapi_jail_rate <- incarceration %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop_rate == max(aapi_jail_pop_rate, na.rm = TRUE)) %>%
  select(county_name, state, aapi_jail_pop_rate)

# location(s) and value of highest value of aapi jail incarceration population in 2018
highest_aapi_jail_pop <- incarceration %>%
  filter(year == 2018) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>%
  select(county_name, state, aapi_jail_pop)

# trends over time chart
# the trend of your measure over time



# variable comparison chart

# map