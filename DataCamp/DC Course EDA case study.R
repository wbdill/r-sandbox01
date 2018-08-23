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
votes2 <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945) %>%
  mutate(country = countrycode(ccode, "cown", "country.name"))
head(votes2)
