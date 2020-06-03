
# Pipelines for Data Analysis by Hadley Wickham
# https://youtu.be/40tyOFMZUSM?t=1392
#----- babynames init -----
rm(list = ls())
install.packages("babynames")
library(tidyverse)
library(babynames)
babynames <- tbl_df(babynames)

babynames
babynames %>% select(-prop)

setwd("C:/GitHub/r-sandbox01")
#----- graphs of baby names over time -----
#----- bunch of graphs of baby names over time -----
ylbl = "Number of Births (thousands)"
var_name <- "Karen"
ctitle <- paste("Babies Born Named", var_name)

babynames %>% 
  filter(name == var_name, sex == "M", year > 1880) %>%
  ggplot(aes(year, n/1000)) + 
  geom_line() +
  labs(x = "Year", y = ylbl, title = ctitle) +
  scale_x_continuous(breaks = seq(1880, 2020, 10))


ggsave(filename = paste0("output/babies_named_",var_name,".png"), width = 6, height = 4, dpi = 120)

#Brian vs Ryan
babynames %>% 
  filter(name %in% c("Brian", "Ryan"), sex == "M", year > 1950) %>%
  ggplot(aes(year, n/1000)) + 
  geom_line() +
  labs(y = ylbl, title = "Births") +
  facet_grid(name~.)

#Richard, Hillary, Karen
babynames %>% 
  filter( (name == "Richard" & sex == "M") | (name == "Hillary" & sex == "F") | (name == "Karen" & sex == "F")  | (name == "Peyton" & sex == "M"), year > 1940) %>%
  ggplot(aes(year, n/1000, col = name)) + 
  geom_line(size = 1) +
  scale_y_log10() +
  labs(y = ylbl, title = "Baby Names", caption = "graph @bdill  source: babynames R package (https://bit.ly/3apr8AQ)")
ggsave(filename = paste0("output/babies_named_richard_hillary_karen.png"), width = 6, height = 4, dpi = 120)

#Brian vs Tina
babynames %>% 
  filter( (name == "Brian" & sex == "M") | (name == "Tina" & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n/1000, col = name)) + 
  geom_line() +
  labs(y = ylbl, title = "Births")

#Steve vs Beth
babynames %>% 
  filter( (name == "Steve" & sex == "M") | (name == "Beth" & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n/1000, col = name)) + 
  geom_line() +
  labs(y = ylbl, title = "Births")

babynames %>% filter( (name %in% c("Brian", "Steve") & sex == "M") | (name %in% c("Beth", "Tina") & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n/1000)) + 
  geom_line(aes(color = name)) +
  labs(x = "Year", y = ylbl, title = "Births")



babynames %>% 
  filter( (name %in% c("Brian","William","Robert") & sex == "M") | (name %in% c("Catherine","Caitlyn","Tina") & sex == "F"), year > 1940) %>%
  ggplot(aes(year, n/1000)) + 
  geom_line(aes(color=name)) +
  labs(y = ylbl, title = "Births")

babynames %>% 
  filter(name %in% c("Caitlyn","Tina"), sex == "F") %>%
  ggplot(aes(year, n/1000)) + 
  geom_line() +
  labs(y = ylbl, title = "Births") +
  facet_grid(name~.)


#----- Top male baby names by proportion -----
baby_prop <- babynames %>% 
  filter(sex == "M") %>%
  group_by(year) %>%
  arrange(desc(prop)) %>%
  top_n(1)

#----- Top male names by proportion -----
baby_prop <- babynames %>% 
  group_by(year, sex) %>%
  mutate(rank = min_rank(desc(prop))) %>%
  filter(rank == 1) %>%
  ungroup()

baby_prop %>% distinct(name)

ggplot(baby_prop, aes(x=year, y = prop)) +
  geom_point(aes(color = name))

#----- Top names by year -----
top_names_by_year <- babynames %>% 
  group_by(year, sex) %>%
  mutate(rank = min_rank(desc(n))) %>%
  filter(rank == 1) %>%
  arrange(year)
  

tail(top_names_by_year, 20)

top_names_by_year %>% 
  ggplot(aes(x=year, y = prop*100)) +
  geom_point(aes(color = name)) +
  facet_grid(sex~.) +
  ylim(0,10) +
  labs(y = "Percent", title="Top Birth Names By Percent (Proportion)")




#----- year each name was most popular -----
babynames %>% 
  group_by(name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank == 1) %>%
  arrange(desc(n))

#----- Top names of all time -----
top_all_time <- babynames %>% 
  group_by(name, sex) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(1000)

top_all_time

write_csv(top_all_time, path="C:/data/R/top_all_time.csv")



#----- Male/Female comparisons -----
mf <- babynames %>% group_by(year, sex) %>%
  summarize(number = sum(n))

tot_by_year <- mf %>% group_by(year) %>% summarize(total = sum(number))

mfpct <- mf %>% inner_join(tot_by_year, by = "year") %>%
  mutate(pct = number / total)

mfpct %>% ggplot( aes(x=year, y=pct, color=sex)) +
  geom_line()

tot_by_year %>% ggplot(aes(x=year, y=total/1000000)) + geom_line() +
  labs(y = "Births (Million)", title="Births By Year")

#----- Setting additional specific axis tick marks -----
#https://stackoverflow.com/questions/51019320/r-ggplot2-setting-additional-specific-axis-tick-marks
df <- data.frame(y = c(1,2,3), x=c(1,2,3))

pretty_br <- pretty(df$x)[abs(pretty(df$x) - 1.5) > 0.25]
ggplot(df, aes(x, y)) + 
  geom_line() +
  scale_x_continuous(breaks = c(pretty_br, 1.5), labels = c(pretty_br, 'hi'))
