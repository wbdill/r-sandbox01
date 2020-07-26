# https://covidtracking.com/api/
# https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx
rm(list = ls())
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


#----- Southeast states TESTS per M -----
states_daily_withpop %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL", "GA", "FL", "TX")) %>%
  filter(date > "2020-03-14") %>%
  mutate(TestsPerM = totalTestResults / (population / 1000000) ) %>%
  #View()
  ggplot(aes(date, TestsPerM, color = state)) +
  geom_line(size = .5) + 
  labs(title = "Tests per Million covid19",
       subtitle = "Southeastern States",
       y = "Cumulative Tests per Million",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv"
       )

ggsave(filename = "covidtracker.com_southeast_states_tests_per_pop.png", path = "output")

#----- Southeast states CASES per M -----
states_daily_withpop %>%
  filter(state %in% c("TN", "SC", "MS", "AZ", "GA", "FL", "TX", "LA")) %>%
  filter(date > "2020-06-14") %>%
  mutate(CasesPerM = positive / (population / 1000000) ) %>%
  #filter(CasesPerM > 15000) %>% 
  #View()
  ggplot(aes(date, CasesPerM, color = state)) +
  geom_line(size = 1) + 
  geom_point(size = 1) +
  expand_limits(y = 0) +
  #scale_y_log10() +
  labs(title = "Cumulative Cases per Million - covid19",
       subtitle = "Southeastern States",
       y = "Cumulative Cases per Millions",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv"
       )

ggsave(filename = "output/covidtracker.com_southeast_states_cases_per_pop.png")

#----- Southeast states NEW CASES -----
states_daily %>%
  filter(state %in% c("TN", "KY", "LA", "AL", "GA", "MS", "FL", "TX", "AR")) %>%
  ggplot(aes(date, positiveIncrease, color = state)) +
  geom_smooth(se = FALSE, size = 1) + 
  #geom_line(size = 1) + 
  labs(title = "covid19 Confirmed New Cases",
       subtitle = "Southeastern States",
       y = "New Cases",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_southeast_states_new_cases.png")


#----- Misc states NEW CASES  -----
states_daily_withpop %>%
  filter(state %in% c("GA", "SC", "AZ", "FL", "TX", "CA", "NC")) %>%
  ggplot(aes(date, positiveIncrease, color = state)) +
  geom_smooth(se = TRUE, size = 1) + 
  #geom_line(size = 1) + 
  labs(title = "covid19 Confirmed New Cases",
       subtitle = "Misc States",
       y = "New Cases",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_misc_states_new_cases.png")


#----- Misc states NEW CASES per M -----
states_daily_withpop %>%
  filter(state %in% c("GA", "SC", "AZ", "FL", "TX", "CA", "MS", "TN")) %>%
#  select(state, positiveIncrease , population) %>%
  mutate(new_per_pop = positiveIncrease / (population) * 1000000) %>%
  ggplot(aes(date, new_per_pop, color = state)) +
  geom_smooth(se = FALSE, size = 1) + 
  #geom_line(size = 1) + 
  labs(title = "covid19 Confirmed New Cases Per Million",
       subtitle = "Misc States",
       y = "New Cases per Mill",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_misc_states_new_cases_per_m.png")



#----- top states - TESTING -----
top5_states_tests <- states_curr %>%
  arrange(desc(totalTestResults)) %>%
  select(state, totalTestResults) %>%
  top_n(5, totalTestResults)

states_daily %>%
  filter(state %in% pull(top5_states_tests, state)) %>%
  filter(date > "2020-03-14") %>%
  ggplot(aes(date, totalTestResults, color = state)) +
  geom_line(size = 1) +
  labs(title = "Cumulative covid19 Tests",
       subtitle = "Top 5 States",
       y = "Cumulative Tests",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "output/covidtracker.com_top5_testing_states.png")


#----- top states - CASES -----
top5_states_cases <- states_curr %>%
  arrange(desc(positive)) %>%
  select(state, positive, positiveScore, datagrade = grade) %>%
  top_n(7, positive)
  
states_daily %>%
  filter(state %in% pull(top5_states_cases, state)) %>%
  filter(date > "2020-03-14") %>%
  ggplot(aes(date, positive, color = state)) +
  geom_line(size = 1) + 
  labs(title = "Cumulative Cases covid19",
       subtitle = "Top 5 states",
       y = "Cumulative Cases",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_top5_states_cases.png")


#----- top states CasesPerM -----
top5_state_cases_per_pop <- states_curr_withpop %>%
  mutate(CasesPerM = positive / (population / 1000000)) %>%
  select(state, positive, CasesPerM, population) %>%
  top_n(7, CasesPerM) %>%
  arrange(desc(CasesPerM))

states_daily_withpop %>%
  filter(state %in% pull(top5_state_cases_per_pop, state)) %>%
  filter(date > "2020-03-14") %>%
  mutate(CasesPerM = positive / (population / 1000000) ) %>%
  ggplot(aes(date, CasesPerM, color = state)) +
  geom_line(size = 1) + 
  labs(title = "Cumulative covid19 Cases per Capita",
       subtitle = "Top 5 States",
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
  filter(date > "2020-03-14") %>%
  mutate(TestsPerM = totalTestResults / (population / 1000000) ) %>%
  ggplot(aes(date, TestsPerM, color = state)) +
  geom_line(size = 1) + 
  labs(title = "Cumulative covid19 Tests per Capita",
       subtitle = "Top 5 States",
       y = "Cumulative Cases per Million People",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

ggsave(filename = "output/covidtracker.com_top5_states_tests_per_mill.png")


#----- spreadsheet output -----
str(states_curr_withpop)
states_curr_withpop %>%
  select(state, region = census_region, population, cases = positive, tests = totalTestResults, deaths = death) %>%
  mutate(CasesPerM = round(cases / (population / 1000000), digits = 1),
         DeathsPerM = round(deaths / (population / 1000000), digits = 1),
         TestsPerM = round(tests / (population / 1000000), digits = 0) ) %>%
  filter(!is.na(region)) %>%
  arrange(desc(CasesPerM)) %>%
  write_csv(path = "output/covid19_states_tests_per_pop.csv")


states_curr_withpop %>%
  select(state, region, population, cases = positive, tests = totalTestResults, deaths = death) %>%
  View()



#----- Misc states NEW CASES per M -----
MapState <- function(x) {
  state_name <- x$state
  state_abbrev <- x$state_abbrev
  
  gtitle = paste("New covid19 Cases Per Million - ", state_name)

  states_daily_withpop %>%
  filter(state == state_abbrev) %>%
  mutate(new_per_pop = positiveIncrease / (population) * 1000000) %>%
  ggplot(aes(date, new_per_pop, color = state)) +
  geom_smooth(se = TRUE, size = .5) + 
  labs(title = gtitle,
       y = "New Cases per Mill",
       caption = "graph: @bdill   data: http://covidtracking.com/api/states/daily.csv")

  filename <- paste("output/by_state/new_per_pop/covidtracker.com_misc_states_new_cases_per_m.png", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")
}
by(state_pop, 1:nrow(state_pop), MapState)
