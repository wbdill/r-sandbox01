
# dataset: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
rm(list = ls())
library(readxl)
library(tidyverse)

path <- "C:/Users/brian.dill/Downloads/COVID-19-geographic-disbtribution-worldwide-2020-03-17.xlsx"
covid19 <- read_excel(path)
names(covid19) <- c("DateRep", "Day", "Month", "Year", "Cases", "Deaths", "Country", "GeoID")

covid19 %>%
  distinct(Country) %>%
  View()

plotdata <- covid19 %>%
  filter(Country %in% c("United_States_of_America","Iran","Italy", "China")) %>%
  filter(DateRep > '2020-02-14') %>%
  select(DateRep, Country, Cases, Deaths)

ggplot(plotdata, aes(x = DateRep, y = Cases, group = Country, color = Country))+
  geom_line() +
  labs(title = "Coronavirus Cases", 
       x = "Date Reported",
       y = "# of new confirmed cases",
       subtitle = "European Centre for Disease Prevention and Control",
       caption = "Source: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide")

ggsave(filename = "C:/Users/brian.dill/Downloads/covid19_2020-03-17.png", width = 10, height = 6, dpi = 120)
