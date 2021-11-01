library(vroom)
library(tidyverse)
rm(list = ls())

#download.file("https://www.ssa.gov/oact/babynames/state/namesbystate.zip", "namesbystate.zip")
#unzip("namesbystate.zip", exdir = "namesbystate")

files <- dir("D:/opendata/SSA/babynames/", pattern = "*.TXT", full.names = T)
df <- vroom(files, col_names = c("state", "sex", "year", "name", "n"))
#spec(df)
#?vroom
str(df)

bn_nat <- df %>% group_by(name, year, sex) %>% summarise(n = sum(n))

bn_nat %>% filter(name == "Caitlyn", year == 1970, sex == "F")

bn_nat %>% filter(name == "Chris", sex == "M") %>% 
  ggplot(aes(x=year, y = n)) + geom_line()

df %>% filter(name == "Barbara", year == 1970, sex == "M") %>% summarize(n = sum(n))

df %>% filter(name %in% c("Brian", "Christian", "John") & state %in% c("MS", "CO", "TX") & sex == "M" & year > 1930) %>% 
  group_by(year, name, state) %>% summarize (n = sum(n)) %>% 
  ggplot(aes(x = year, y = n, group = state, color = name)) + 
  geom_line(size = 1, alpha = 0.5) +
  #scale_y_log10() +
  facet_wrap(~ state)

df %>% filter(name %in% c("Brian", "Christian", "John") & sex == "M" & state %in% c("MS", "CO", "AL", "CA") & year > 1930) %>% 
  group_by(year,) %>%  mutate(total = sum(n)) %>% ungroup() %>% 
  group_by(year, name, state) %>% summarize (n = sum(n), total = total) %>% 
  mutate(prop = n*100 / total) %>% 
  ggplot(aes(x = year, y = prop, group = name, color = name)) + 
  geom_line(size = 1.5) +
  #scale_y_log10() +
  facet_wrap(~ state)

df %>% 
  group_by(year, state) %>% mutate(total = sum(n))  %>% ungroup() %>% 
  filter(name %in% c("Brian", "Christian", "John") & sex == "M" & state %in% c("MS", "CO", "AL", "TN") & year > 1930) %>% 
  mutate(prop = n*100 / total) %>% 
  ggplot(aes(x = year, y = prop, group = name, color = name)) + 
  geom_line(size = 1.0, alpha = 0.7) +
  #scale_y_log10() +
  facet_wrap(~ state)
