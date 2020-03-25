# https://covidtracking.com/api/
library(tidyverse)
library(lubridate)

#----- states daily -----
statesdaily <- read_csv("http://covidtracking.com/api/states/daily.csv")

statesdaily$date <- ymd(ds$date)

statesdaily %>%
  filter(state %in% c("TN", "KY", "MS", "LA", "AL")) %>%
  ggplot(aes(date, total, color = state)) +
  geom_line() + 
  labs(title = "Total Cumulative covid19 Tests",
       y = "Total Tests",
       caption = "source: http://covidtracking.com/api/states/daily.csv")
ggsave(filename = "covidtracker.com_southeast_states_tests.png")

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

