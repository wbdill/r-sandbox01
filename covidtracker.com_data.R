# https://covidtracking.com/api/
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
library(tidyverse)
library(lubridate)

#----- state populations -----
state_pop <- read_csv("state_populations_2019.csv")
state_pop %>% arrange(desc(population))

#----- states daily -----
statesdaily <- read_csv("http://covidtracking.com/api/states/daily.csv")

statesdaily$date <- ymd(ds$date)

statesdaily2 <- left_join(statesdaily, state_pop, by = c("state" = "state_abbrev"))

statesdaily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL")) %>%
  ggplot(aes(date, total, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Tests",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "covidtracker.com_southeast_states_tests.png")


statesdaily2 %>%
  filter(state %in% c("TN", "NY", "CA", "LA", "WA", "IL")) %>%
  ggplot(aes(date, Positive, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Cases",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "covidtracker.com_southeast_states_cases_NY.png")

# factoring in population.
statesdaily2 %>%
  filter(state %in% c("TN", "NY", "CA", "LA", "WA", "IL")) %>%
  mutate(CasesPerM = positive / (population / 1000000) ) %>%
  ggplot(aes(date, CasesPerM, color = state)) +
  geom_line() + 
  #scale_y_log10() +
  labs(title = "Total Cumulative covid19 Cases / Million Pop",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "covidtracker.com_southeast_states_cases_NY_per_mill.png")


statesdaily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL")) %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Confirmed Cases",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "covidtracker.com_southeast_states_cases.png")



#----- states current -----
states <- read_csv("https://covidtracking.com/api/states.csv")

states %>% filter(state %in% c("TN", "KY", "MS", "LA", "AL"))

