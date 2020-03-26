# https://covidtracking.com/api/
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
library(tidyverse)
library(lubridate)

#----- state populations -----
# state_populations_2019.csv derrived from 2019 column in:
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
# Download directly here: https://pastebin.com/UAhAPiYB

#state_pop <- read_csv("data/state_populations_2019.csv")
state_pop <- read_csv("https://pastebin.com/raw/UAhAPiYB")
#state_pop %>% arrange(desc(population))

#----- states daily -----
statesdaily <- read_csv("http://covidtracking.com/api/states/daily.csv")

statesdaily$date <- ymd(statesdaily$date)

#----- Southeast states TESTS -----
statesdaily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL", "GA")) %>%
  ggplot(aes(date, total, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Tests",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "covidtracker.com_southeast_states_tests.png", path = "output")

#----- Southeast states CASES -----
statesdaily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL", "GA")) %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Confirmed Cases",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_southeast_states_cases.png")

#----- top states - TESTING -----
statesdaily %>%
  filter(state %in% c("NY", "WA", "CA", "MA", "FL")) %>%
  ggplot(aes(date, total, color = state)) +
  geom_line() +
  labs(title = "covid19 Tests - Top 5 States",
       y = "Total Cumulative Tests",
       caption = "Source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "output/covidtracker.com_top5_testing_states.png")


#----- top states - CASES -----
statesdaily %>%
  filter(state %in% c("TN", "NY", "CA", "LA", "WA", "IL")) %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Cases",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_southeast_states_cases_NY.png")


#----- factoring in population. -----
statesdaily_withpop <- left_join(statesdaily, state_pop, by = c("state" = "state_abbrev"))

#----- Big states cases per population. -----
statesdaily_withpop %>%
  filter(state %in% c("TN", "NY", "CA", "LA", "WA", "IL")) %>%
  mutate(CasesPerM = positive / (population / 1000000) ) %>%
  ggplot(aes(date, CasesPerM, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Cases / Million Pop",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "output/covidtracker.com_southeast_states_cases_NY_per_mill.png")


#statesdaily %>% filter(state == "TN")


#----- states current dataset -----
states <- read_csv("https://covidtracking.com/api/states.csv")

states %>% filter(state %in% c("TN", "KY", "MS", "LA", "AL"))





