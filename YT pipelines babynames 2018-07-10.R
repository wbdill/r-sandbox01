
# Pipelines for Data Analysis by Hadley Wickham
# https://youtu.be/40tyOFMZUSM?t=1392
#----- babynames init -----
rm(list = ls())
install.packages("babynames")
library(babynames)
babynames <- tbl_df(babynames)

babynames
babynames %>% select(-prop)

#----- bunch of graphs of baby names over time -----
var_name <- "Steve"
ctitle <- paste("Babies Born Named", var_name)
babynames %>% filter(name == var_name, sex == "M", year > 1800) %>%
  ggplot(aes(year, n)) + 
  geom_line() +
  labs(x = "Year", y = "Number of Births", title = ctitle)

#Brian vs Ryan
babynames %>% filter(name %in% c("Brian", "Ryan"), sex == "M", year > 1950) %>%
  ggplot(aes(year, n)) + 
  geom_line() +
  labs(y = "Number of Births", title = "Births") +
  facet_grid(name~.)

#Brian vs Tina
babynames %>% filter( (name == "Brian" & sex == "M") | (name == "Tina" & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n)) + 
  geom_line() +
  labs(y = "Number of Births", title = "Births") +
  facet_grid(name~.)

#Steve vs Beth
babynames %>% filter( (name == "Steve" & sex == "M") | (name == "Beth" & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n)) + 
  geom_line() +
  labs(y = "Number of Births", title = "Births") +
  facet_grid(name~.)

babynames %>% filter( (name %in% c("Brian", "Steve") & sex == "M") | (name %in% c("Beth", "Tina") & sex == "F"), year > 1920) %>%
  ggplot(aes(year, n)) + 
  geom_line(aes(color = name)) +
  labs(x = "Year", y = "Number of Births", title = "Births")



babynames %>% filter( (name %in% c("Brian","William","Robert") & sex == "M") | (name %in% c("Catherine","Caitlyn","Tina") & sex == "F"), year > 1940) %>%
  ggplot(aes(year, n)) + 
  geom_line(aes(color=name)) +
  labs(y = "Number of Births", title = "Births") +
  scale_y_log10()

babynames %>% filter(name %in% c("Caitlyn","Tina"), sex == "F") %>%
  ggplot(aes(year, n)) + 
  geom_line() +
  labs(y = "Number of Births", title = "Babies Born") +
  facet_grid(name~.)


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
  arrange(year) %>%
  

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

#----- foo -----
#----- bar -----