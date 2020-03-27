
#----- Johns Hopkins CSSE data repository -----
# https://pastebin.com/g8BTSmwN
# https://github.com/CSSEGISandData/COVID-19
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-15-2020.csv
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# https://datahub.io/JohnSnowLabs/population-figures-by-country
# rm(list = ls())
#library(data.table)
library(tidyverse)
library(lubridate)
#setwd("C:/Users/brian.dill/Downloads/")


#----- Read in data -----
jhconfirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhdeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
jhrecovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

jhcountries <- jhconfirmed %>% select(`Province/State`, `Country/Region`, Lat, Long)
names(jhcountries) <- c("Province", "Country", "Lat", "Long")

#----- data wrangle -----
jhconfirmed2 <- select(jhconfirmed, -Lat, -Long) %>%
  gather(key = "date", value = "confirmed", -`Province/State`, -`Country/Region`)
jhconfirmed2$date <- mdy(jhconfirmed2$date)
names(jhconfirmed2) <- c("Province", "Country", "Date", "Confirmed")

jhdeaths2 <- select(jhdeaths, -Lat, -Long) %>%
  gather(key = "date", value = "deaths", -`Province/State`, -`Country/Region`)
jhdeaths2$date <- mdy(jhdeaths2$date)
names(jhdeaths2) <- c("Province", "Country", "Date", "Deaths")

jhrecovered2 <- select(jhrecovered, -Lat, -Long) %>%
  gather(key = "date", value = "recovered", -`Province/State`, -`Country/Region`)
jhrecovered2$date <- mdy(jhrecovered2$date)
names(jhrecovered2) <- c("Province", "Country", "Date", "Recovered")

jh <- inner_join(jhconfirmed2, jhdeaths2) %>%
  inner_join(jhrecovered2) %>%
  arrange(Country, Province, desc(Date))

head(jh)
jh_gis <- inner_join(jh, jhcountries)

jh_country <- jh %>%
  group_by(Country, Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths= sum(Deaths),
            Recovered = sum(Recovered)) %>%
  arrange(Country, desc(Date))

#----- Save csv files -----
#write_csv(jh, path = "output/covid19_jh_timeseries.csv")
#write_csv(jh_country, path = "output/covid19_jh_country_timeseries.csv")
#write_csv(jhcountries, path = "output/covid19_jh_country_lat_long.csv")
#write_csv(jh_gis, path = "output/covid19_jh_timeseris_with_latlong.csv")


#----- graphs -----

#----- China by province -----
jh %>%
  filter(Country %in% c("China")) %>%
  ggplot(aes(x = Date, y = Confirmed, color = Province)) +
  geom_line() +
  labs(title = "Confirmed covid-19 cases by Province in China",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = paste0(getwd(), "/output/covid19_by_china_province.png"), width = 10, height = 6, dpi = 120)


#----- Top countries -----
jh_country %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Germany", "China")) %>%
  ggplot(aes(x = Date, y = Confirmed, color = Country)) +
  geom_line() +
  scale_y_log10(limits = c(1, 100000)) +  
  labs(title = "covid-19 Cases by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = paste0(getwd(), "/output/covid19_cases_by_country.png"), width = 10, height = 6, dpi = 120)

jh_country %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Germany", "China")) %>%
  ggplot(aes(x = Date, y = Deaths, color = Country)) +
  geom_line() +
  #scale_y_log10(limits = c(10, 10000)) +  
  labs(title = "covid-19 Deaths by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = paste0(getwd(), "/output/covid19_deaths_by_country.png"), width = 10, height = 6, dpi = 120)


#jh_country %>% filter(Country %in% c("Italy", "Iran", "US", "Spain", "Germany", "China")) %>% arrange(desc(Date), Country)

#----- Country populations -----

country_pop <-read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")
country_pop2 <- country_pop %>%
  select(Country, pop = Year_2016) %>%
  mutate(Country = case_when(Country == "United States" ~ "US", 
                             Country == "Iran, Islamic Rep." ~ "Iran",
                             TRUE ~ Country))

#----- daily update v population -----

jh_daily <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-25-2020.csv")
                      
names(jh_daily) <- c("FIPS", "Admin2", "Province", "Country", "Date", "Lat", "Long", "Confirmed", "Deaths", "Recovered", "Active", "CombinedKey")

countries_of_interest <- c("US", "Italy", "China", "Canada", "Iran", "United Kingdom", "Germany", "France", "Spain", "Switzerland")

jh_daily %>%
  #filter(Country %in% countries_of_interest ) %>%
  group_by(Country) %>%
  summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths)) %>%
  inner_join(country_pop2) %>%
  filter(pop > 1000000) %>%
  mutate(PopMillions = round(pop / 1000000, digits = 1),
         ConfirmedPerMill = round(Confirmed / PopMillions, digits = 1),
         DeathsPerMill = round(Deaths / PopMillions, digits = 1),
         MortalityRate = round((Deaths / Confirmed)*100, digits = 1)) %>%
  select(Country, PopMillions, Confirmed, ConfirmedPerMill, Deaths, DeathsPerMill, MortalityRate) %>%
  arrange(desc(ConfirmedPerMill)) %>%
  top_n(150) %>%
  write_csv(path = "output/covid19_high_confirmed_per_pop.csv")
  
