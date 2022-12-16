library(tidyverse)
library(ggplot2)
library(leaflet)
library(stringr)
library(tidyr)
library(lintr)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration_inc <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

# White population rate in jail 

white_pop_jail <- incarceration_inc %>%
  select(year,white_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(white_average = mean(white_jail_pop_rate, na.rm = TRUE))

# White population rate in 2018 

white_pop_jail_2018 <- white_pop_jail %>%
  filter(year == "2018") %>%
  pull(white_average)

# Native American population rate in jail 

native_pop_jail <- incarceration_inc %>%
  select(year, native_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(native_average = mean(native_jail_pop_rate, na.rm = TRUE))

# Black population rate in jail 

black_pop_jail <- incarceration_inc %>%
  select(year, black_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(black_average = mean(black_jail_pop_rate, na.rm = TRUE))

# Black population rate in 2018 

black_pop_jail_2018 <- black_pop_jail %>%
  filter(year == "2018") %>%
  pull(black_average)

# Latinx population in jail 

latinx_pop_jail <- incarceration_inc %>%
  select(year, latinx_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(latinx_average = mean(latinx_jail_pop_rate, na.rm = TRUE))

# Asian American population in jail 

asain_pop_jail <- incarceration_inc %>%
  select(year, aapi_jail_pop_rate) %>%
  group_by(year) %>%
  summarise(asain_average = mean(aapi_jail_pop_rate, na.rm = TRUE))
  
# combine the dataframe

grouped_population_jail <- left_join(white_pop_jail, black_pop_jail, by = "year") %>%
  left_join(latinx_pop_jail, by = "year") %>%
  left_join(native_pop_jail, by = "year") %>%
  left_join(asain_pop_jail, by = "year") %>%
  filter(year >= "1970") %>%
  filter(year <= "2018")
  
# Population rate in jail 

asain_pop <- incarceration_inc %>%
  select(year, aapi_prison_pop_rate) %>%
  group_by(year) %>%
  summarise(asain_average = mean(aapi_prision_pop_rate, na.rm = TRUE))


black_pop <- incarceration_inc %>%
  select(year, black_prison_pop_rate) %>%
  group_by(year) %>%
  summarise(black_average = mean(black_prison_pop_rate, na.rm = TRUE))
  

native_pop <- incarceration_inc %>%
  select(year, native_prison_pop_rate) %>%
  group_by(year) %>%
  summarise(native_average = mean(native_prison_pop_rate, na.rm =TRUE))


latinx_pop <- incarceration_inc %>%
  select(year, latinx_prison_pop_rate) %>%
  group_by(year) %>%
  summarise(latinx_average = mean(latinx_prison_pop_rate, na.rm = TRUE))


white_pop <- incarceration_inc %>%
  select(year, white_prison_pop_rate) %>%
  group_by(year) %>%
  summarise(white_average = mean(white_prison_pop_rate, na.rm = TRUE))
  

# Combine the dataset

grouped_population <- left_join(asain_pop, black_pop, by = "year") %>%
  left_join(native_pop, by = "year") %>%
  left_join(latinx_pop, by = "year") %>%
  left_join(white_pop, by = "year") %>%
  filter(year >= "1970") %>%
  filter(year <= "2018") %>%
  





#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# Population rate changes in jail 

pop_jail_graph <- ggplot(data = grouped_population_jail) +
  geom_line(mapping = aes(x = year, y = asain_average_pop_rate, color = "Asain American")) +
  geom_line(mapping = aes(x = year, y = white_average_pop_rate, color = "White")) +
  geom_line(mapping = aes(x = year, y = latinx_average_pop_rate, color = "Latinx")) +
  geom_line(mapping = aes(x = year, y = black_average_pop_rate, color = "Black")) +
  geom_line(mapping = aes(x = year, y = native_average_pop_rate, color = "Native American")) +
  labs(title = "Racial differences in Jail (1970-2018)", x = "year", y = "Population rate", color = "Race")





# Gender rate changes in jail 









#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


