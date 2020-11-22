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
trends_chart <- ggplot(trend_mean_aapi_jail_rate) +
  geom_point(mapping = aes(x = year, y = aapi_jail_pop_rate)) +
  geom_line(mapping = aes(x = year, y = aapi_jail_pop_rate)) +
  scale_x_continuous(breaks = seq(2008, 2018, by = 1)) +

  labs(
    title = "Average Jail Incarceration Rate of Asian American and Pacific Islanders Across All Counties from 2008 to 2018",
    x = "year",
    y = "jail incarceration rate per 100,000 residents"
  )

# variable comparison chart
comparison_chart <- ggplot(incarceration) +
  geom_point(mapping = aes(x = aapi_jail_pop, y = black_jail_pop, color = year)) +
  scale_x_continuous() +
  scale_y_continuous() +

  labs(
    title = "Comparison of Asian American and Pacific Islander Jail Incarceration Population to Black Jail Incarceration Population",
    x = "Asian Americans and Pacific Islanders Jail Incarceration Population",
    y = "Black Jail Incarceration Population"
  )


# map