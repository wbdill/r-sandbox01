# DC Course EDA case study
rm(list = ls())
library(dplyr)

votes <- readRDS("C:/GitHub/r-sandbox01/DataCamp/data/votes.rds")
desc <- readRDS("C:/GitHub/r-sandbox01/DataCamp/data/descriptions.rds")

str(votes)
head(votes)
str(desc)

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