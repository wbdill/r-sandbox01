# DC Course data manipulation with dplyr
# https://campus.datacamp.com/courses/data-manipulation-with-dplyr/transforming-data-with-dplyr?ex=1
# 2020-07-22
library(tidyverse)

counties <- readRDS("data/counties.rds")

#----- Ch 1 -----
glimpse(counties)

counties %>%
  select(state, county, population, poverty) %>% 
  arrange(desc(population)) %>% 
  filter(state == "New York")

counties_selected <- counties %>%
  select(state, county, population, public_work)

counties_selected %>%
  mutate(public_workers = population * public_work / 100)  

counties_selected <- counties %>%
  select(region, state, population, citizens)

counties_selected %>%  
count(state, wt = citizens, sort = TRUE)

counties %>%
  select(region, state, population, walk) %>% 
  mutate(population_walk = population * walk / 100) %>%  
  count(state, wt = population_walk, sort = TRUE)

#----- Ch2 - Aggregating Data -----
counties %>%
  select(county, population, income, unemployment) %>% 
  summarize(min_population = min(population),
            max_unemployment = max(unemployment),
            average_income = mean(income))

counties %>%
  select(state, county, population, land_area) %>% 
  group_by(state) %>% 
  summarize(total_area = sum(land_area),
            total_population = sum(population),
            density = total_population / total_area) %>% 
arrange(desc(density))

counties %>%
  select(region, state, county, population) %>% 
  group_by(region, state) %>% 
  summarize(total_pop = sum(population))

counties %>%
  select(region, state, county, population) %>% 
  group_by(region, state) %>% 
  summarize(total_pop = sum(population)) %>% 
  summarize(average_pop = mean(total_pop),
            median_pop = median(total_pop))

counties %>%
  select(region, state, county, population, income) %>% 
  group_by(region, state) %>%
  # Calculate average income
  summarize(average_income = mean(income)) %>%
  # Find the highest income state in each region
  top_n(1, average_income)

counties %>%
  select(state, metro, population) %>% 
  group_by(state, metro) %>%
  summarize(total_pop = sum(population))

#----- Ch 3 - Selecting and Transforming Data -----
counties %>%
  # Select state, county, population, and industry-related columns
  select(state, county, population, professional:production) %>%
  # Arrange service in descending order 
  arrange(desc(service))

counties %>%
  # Select the state, county, population, and those ending with "work"
  select(state, county, population, ends_with("work")) %>%
  # Filter for counties that have at least 50% of people engaged in public work
  filter(public_work > 50)

counties %>%
  count(state) %>%
  rename(num_counties = n)

#                      limit cols | keep all cols
#                     ===========================
# don't change vals  | select     |  rename
# does  change vals  | transmute  |  mutate
#

counties %>%
  # Keep the state, county, and populations columns, and add a density column
  transmute(state, county, population, density = population / land_area) %>%
  # Filter for counties with a population greater than one million 
  filter(population > 1000000) %>%
  # Sort density in ascending order 
  arrange(density)

#----- Ch 4 - Case Study: The babynames dataset -----
babynames <- readRDS("data/babynames.rds")
library(tidyverse)


babynames %>% 
  filter(name %in% c('Karen', 'Chad', 'Amy')) %>% 
  filter(year > 1920) %>% 
  ggplot(aes(year, number, color = name, group = name)) +
  geom_line(size=1) +
  labs(title = "Baby Names Born in Year",
       caption = "source: babynames R package")
#ggsave("output/karen_babyname.png", units = "in", dpi = 72, height = 3, width = 4)
ggsave(filename = "output/karen_babyname.png", width = 16, height = 10, units = "cm")  

babynames %>% 
  filter(year == 1970, number > 20000)


