# https://www.adl.org/heat-map
library(tidyverse)
library(readr)
#library(lubridate)
library(stringr)
library(data.table)

dat <- read_csv("C:/data/R/data/extremism_adl_org.csv")
saveRDS(data, "C:/Data/R/data/extremism_adl_org.csv.rds")

# how many na's in certain cols?
sum(is.na(dat$image))
sum(is.na(dat$date))

#----- wrangling factors -----
dat$type <- as.factor(dat$type)
dat$state <- as.factor(dat$state)
dat$ideology <- as.factor(dat$ideology)
dat$group <- as.factor(dat$group)
glimpse(dat)
str(dat)

# get year from data
dat$date <- as.Date(dat$date)  # fails b/c not uniform dates
dat <- mutate(dat, year = str_sub(date, -4))

#----- wrangling bit field breakouts -----
dat[which(str_detect(dat$type, "Extremist Murder")), ]
dat$murder <- str_detect(dat$type, "Extremist Murder")
dat$terror <- str_detect(dat$type, "Terrorist Plot/Attack")
dat$shootout <- str_detect(dat$type, "Extremist/Police Shootout")

glimpse(dat)
table(dat$murder)
table(dat$murder, dat$terror)
table(dat$murder, dat$terror, dat$shootout)


#----- plot 1 -----
dat %>%
  filter(murder == TRUE & year >= 2008) %>%
  group_by(ideology) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot( aes(reorder(ideology, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Ideology", y = "Incidents", title="Murder Incidents by Ideology in the US (2008-2017)", caption = "source: www.adl.org/heat-map")

#----- plot 2 stack by year -----
dat %>%
  filter(murder == TRUE & year >= 2008) %>%
  group_by(ideology, year) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot( aes(x=year, y = n, fill=ideology)) +
  geom_col(position="stack") +
  labs(x = "Year", y = "Incidents",title="Murder Incidents by Ideology in the US (2008-2017)", caption = "source: www.adl.org/heat-map")


#----- data table by ideology only -----
dat %>%
  filter(murder == TRUE | terror == TRUE | shootout == TRUE) %>%
  group_by(ideology) %>%
  count() %>%
  arrange(desc(n))

#----- data table counts by combinations of ideology and murder/terror/shootout -----
dat %>%
  filter(murder == TRUE | terror == TRUE | shootout == TRUE) %>%
  group_by(ideology, murder, terror, shootout) %>%
  count() %>%
  arrange(desc(n)) %>%
  fwrite(file="C:/data/R/data/extremism_summary.csv")

#----- data table counts by combinations of murder/terror/shootout -----
dat %>%
  group_by(murder, terror, shootout) %>%
  count()
