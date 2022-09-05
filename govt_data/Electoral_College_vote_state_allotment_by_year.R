
library(tidyverse)
# https://docs.google.com/spreadsheets/d/1bwlPuu190YpOxuZUgcAMALHCxbJg6TkMCtMuNlXiCok/edit#gid=0

ec <- read_csv("https://pastebin.com/raw/BB0SaeGb")
ec <- pivot_longer(ec, cols = !starts_with("state"), names_to = "year", values_to = "votes")

ec %>% filter(!is.na(votes)) %>% 
  filter(!is.na(state_abbrev)) %>% 
  filter(state_abbrev %in% c("CA", "TX", "NY", "IL", "PA", "FL","OH")) %>% 
  ggplot(aes(x = year, y = votes, group = state_abbrev)) +
  geom_line(aes(color = state_abbrev), size = 1.5, alpha = 0.5) +
  #geom_smooth(aes(color = state_abbrev), se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Electoral College Votes Allocated",
       subtitle = "Top 7 States",
       x = "Year",
       y = "EC Votes",
       color = "State",
       caption = "@bdill\ndata: https://en.wikipedia.org/wiki/United_States_Electoral_College#Chronological_table")

