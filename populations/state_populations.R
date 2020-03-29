

#Table 1. Annual Estimates of the Resident Population for the United States, Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2019 (NST-EST2019-01)													
#Source: U.S. Census Bureau, Population Division - Release Date: December 2019													

library(tidyverse)

states_pop <- read_csv("data/state_population_census_2019.csv")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#----- US pop change 2010 - 2019 -----
states_pop %>%
  summarize(pop_2010 = sum(pop_2010),
            pop_2019 = sum(pop_2019),
            pct_chg = (pop_2019 - pop_2010) / pop_2010 * 100)

#----- Top 10 most growing states -----
states_pop %>%
  mutate(pct_chg = round( (pop_2019 - pop_2010) / pop_2010 * 100, digits = 2) ) %>%
  select(state, pop_2010, pop_2019, pct_chg) %>%
  #top_n(10, pct_chg) %>%
  arrange(desc(pct_chg)) %>%
  write_csv(path = "output/us_states_pct_chg_pop.csv")

  View()

states_pop %>%
  gather(key = year, value = pop, starts_with("pop_")) %>%
  select(state, state_abbrev, year, pop) %>%
  mutate(year = substrRight(year, 4)) %>%
  ggplot(aes(year, pop, group = state)) +
  geom_line(aes(color = state))



