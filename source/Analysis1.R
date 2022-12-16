library(ggplot2)
library(dplyr)
library(leaflet)
library(stringr)
library(tidyr)
library(lintr)

incarceration_inc <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Black population rate in jail 
black_pop <- incarceration_trends %>%
  select(year, black_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(black_average_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE))