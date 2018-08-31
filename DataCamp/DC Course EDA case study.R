# DC Course EDA case study
rm(list = ls())
library(dplyr)

votes <- readRDS("C:/GitHub/r-sandbox01/DataCamp/data/votes.rds")
descriptions <- readRDS("C:/GitHub/r-sandbox01/DataCamp/data/descriptions.rds")

str(votes)
head(votes)
str(descriptions)

install.packages("countrycode")
library(countrycode)
?countrycode
?codelist

# Session 1 is 1946, so add 1945 to session to get year
# get country name from countrycode function+
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945) %>%
  mutate(country = countrycode(ccode, "cown", "country.name"))
head(votes_processed)

votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

by_country %>% 
  filter(total > 100) %>%
  arrange(percent_yes)

by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

ggplot(by_year, aes(x = year, y = percent_yes)) +
  geom_point() +
  geom_smooth()


by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

by_year_country %>%
  filter(country %in% c("United States", "United Kingdom","France", "India")) %>%
  ggplot(aes(x = year, y = percent_yes, color = country)) + 
  geom_line()

countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")
by_year_country %>%
  filter(country %in% countries) %>%
  ggplot( aes(year, percent_yes)) +
         geom_line() +
         facet_wrap(~ country)  #, scale = "free_y"

#----- Chapter 3 Linear Regression -----
US_by_year <- by_year_country %>%
  filter(country == "United States")

US_fit <- lm(percent_yes ~ year, US_by_year)

summary(US_fit)
# slope: -0.0062393  p-value: 1.37e-07

library(broom)
US_by_year <- by_year_country %>%
  filter(country == "United States")
US_fit <- lm(percent_yes ~ year, US_by_year)

UK_by_year <- by_year_country %>%
  filter(country == "United Kingdom")
UK_fit <- lm(percent_yes ~ year, UK_by_year)

bind_rows(tidy(US_fit), tidy(UK_fit))

#----- nest/unnest -----
by_year_country <- by_year_country[complete.cases(by_year_country), ]

library(purrr)
library(tidyr)
library(broom)

# https://campus.datacamp.com/courses/exploratory-data-analysis-in-r-case-study/tidy-modeling-with-broom?ex=14
# this works
by_year_country %>%
  nest(-country)

#but this errors out
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, .)))
# Error in mutate_impl(.data, dots) : Evaluation error: variable lengths differ (found for 'year').

by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

#----- Chapter 4: Joining datasets -----
glimpse(votes_processed)
glimpse(descriptions)

votes_joined <- votes_processed %>%
  inner_join(descriptions, by = c("rcid", "session"))

#me: Palestinian conflict
#nu: Nuclear weapons and nuclear material
#di: Arms control and disarmament
#hr: Human rights
#co: Colonialism
#ec: Economic development

# Filter, then summarize by year: US_co_by_year
US_co_by_year <- votes_joined %>%
  filter(co == 1, country == "United States") %>%
  group_by(year) %>%
  summarize(percent_yes = mean(vote == 1))

ggplot(US_co_by_year, aes(year, percent_yes)) +
  geom_line()

votes_gathered <- votes_joined %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic == 1)

#--- use recode() to make codes more readable.
votes_tidied <- votes_gathered %>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and nuclear material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))
 
by_country_year_topic <- votes_tidied %>%
  group_by(country, year, topic) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1)) %>%
  ungroup()

by_country_year_topic %>%
  filter(country == "United States") %>%
  ggplot( aes(year, percent_yes)) + 
  geom_line() +
  facet_wrap(~ topic)

# build models of country/topic combos  (this errors in R-studio for unknown reason so next block won't work either)
country_topic_coefficients <- by_country_year_topic %>%
  nest(-country, -topic) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)) ,
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# filter to just the slope (term == year) and adjusted p.value < .05
country_topic_filtered <- country_topic_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)
