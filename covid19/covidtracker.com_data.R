# https://covidtracking.com/api/
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
library(tidyverse)
library(lubridate)

#----- state populations -----
# state_populations_2019.csv derrived from 2019 column in:
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
# Download directly here: https://pastebin.com/UAhAPiYB

#----- state populations -----
state_pop <- read_csv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/state_populations_2019.csv")

#----- states current dataset -----
states_curr <- read_csv("https://covidtracking.com/api/states.csv")

#----- states daily -----
states_daily <- read_csv("http://covidtracking.com/api/states/daily.csv")

states_daily$date <- ymd(states_daily$date)

#----- factoring in population. -----
states_daily_withpop <- left_join(states_daily, state_pop, by = c("state" = "state_abbrev"))
states_curr_withpop <- left_join(states_curr, state_pop,   by = c("state" = "state_abbrev"))


#----- Southeast states TESTS -----
states_daily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL", "GA")) %>%
  ggplot(aes(date, totalTestResults, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Tests",
       subtitle = "Southeastern States",
       y = "Cumulative Tests",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "covidtracker.com_southeast_states_tests.png", path = "output")

#----- Southeast states CASES -----
states_daily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL", "GA")) %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Confirmed Cases",
       subtitle = "Southeastern States",
       y = "Cumulative Cases",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_southeast_states_cases.png")


#----- top states - TESTING -----
top5_states_tests <- states_curr %>%
  arrange(desc(totalTestResults)) %>%
  select(state, totalTestResults) %>%
  top_n(5, totalTestResults)

states_daily %>%
  filter(state %in% pull(top5_states_tests, state)) %>%
  ggplot(aes(date, totalTestResults, color = state)) +
  geom_line() +
  labs(title = "Cumulative covid19 Tests",
       subtitle = "Top 5 States",
       y = "Cumulative Tests",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "output/covidtracker.com_top5_testing_states.png")


#----- top states - CASES -----
top5_states_cases <- states_curr %>%
  arrange(desc(positive)) %>%
  select(state, positive, positiveScore, datagrade = grade) %>%
  top_n(5, positive)
  
states_daily %>%
  filter(state %in% pull(top5_states_cases, state)) %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line(size = 1) + 
  labs(title = "Cumulative Cases covid19",
       subtitle = "Top 5 states",
       y = "Cumulative Cases",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_top5_states_cases_.png")



#----- top states CasesPerM -----
top5_state_cases_per_pop <- states_curr_withpop %>%
  mutate(CasesPerM = positive / (population / 1000000)) %>%
  select(state, positive, CasesPerM, population) %>%
  top_n(5, CasesPerM) %>%
  arrange(desc(CasesPerM))

states_daily_withpop %>%
  filter(state %in% pull(top5_state_cases_per_pop, state)) %>%
  mutate(CasesPerM = positive / (population / 1000000) ) %>%
  ggplot(aes(date, CasesPerM, color = state)) +
  geom_line(size = .5) + 
  labs(title = "Cumulative covid19 Cases per Million Pop",
       y = "Cumulative Cases per Million People",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_top5_states_cases_per_mill.png")


#----- top states TESTS PerM -----
top5_state_tests_per_pop <- states_curr_withpop %>%
  mutate(TestsPerM = totalTestResults / (population / 1000000)) %>%
  select(state, totalTestResults, TestsPerM, population) %>%
  top_n(5, TestsPerM) %>%
  arrange(desc(TestsPerM))

states_daily_withpop %>%
  filter(state %in% pull(top5_state_tests_per_pop, state)) %>%
  mutate(TestsPerM = totalTestResults / (population / 1000000) ) %>%
  ggplot(aes(date, TestsPerM, color = state)) +
  geom_line(size = .5) + 
  labs(title = "Cumulative covid19 Tests per Million Pop",
       y = "Cumulative Cases per Million People",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_top5_states_tests_per_mill.png")


#----- spreadsheet output -----
states_curr_withpop %>%
  select(state, region, population, cases = positive, tests = totalTestResults, deaths = death) %>%
  mutate(CasesPerM = round(cases / (population / 1000000), digits = 1),
         DeathsPerM = round(deaths / (population / 1000000), digits = 1),
         TestsPerM = round(tests / (population / 1000000), digits = 0) ) %>%
  filter(!is.na(region)) %>%
  arrange(desc(CasesPerM)) %>%
  write_csv(path = "output/covid19_states_tests_per_pop.csv")


states_curr_withpop %>%
  select(state, region, population, cases = positive, tests = totalTestResults, deaths = death) %>%
  View()
