
#----- Johns Hopkins CSSE data repository -----
# https://pastebin.com/g8BTSmwN
# https://github.com/CSSEGISandData/COVID-19
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-15-2020.csv
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# https://datahub.io/JohnSnowLabs/population-figures-by-country
rm(list = ls())
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

jh_cdr <- inner_join(jhconfirmed2, jhdeaths2) %>%
  inner_join(jhrecovered2) %>%
  arrange(Country, Province, desc(Date))

#jh_gis <- inner_join(jh_cdr, jhcountries)

jh_country <- jh_cdr %>%
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
jh_cdr %>%
  filter(Country %in% c("China")) %>%
  ggplot(aes(x = Date, y = Confirmed, color = Province)) +
  geom_line() +
  labs(title = "Confirmed covid-19 cases by Province in China",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = paste0(getwd(), "/output/covid19_by_china_province.png"), width = 10, height = 6, dpi = 120)


#----- Top countries CASES -----
jh_country %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Spain", "Germany", "China", "France")) %>%
  ggplot(aes(x = Date, y = Confirmed, color = Country)) +
  geom_line(size = .7) +
#  scale_y_log10() +  
  labs(title = "covid-19 Cases by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = "output/covid19_cases_by_country.png", width = 10, height = 6, dpi = 120)


#----- Top countries DEATHS -----
jh_country %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Spain", "Germany", "China", "France")) %>%
  filter(Date >= "2020-03-01") %>%
  ggplot(aes(x = Date, y = Deaths, color = Country)) +
  geom_line(size = 1) +
  #scale_y_log10(limits = c(10, 10000)) +  
  labs(title = "covid-19 Deaths by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = paste0(getwd(), "/output/covid19_deaths_by_country.png"), width = 10, height = 6, dpi = 120)


#jh_country %>% filter(Country %in% c("Italy", "Iran", "US", "Spain", "Germany", "China")) %>% arrange(desc(Date), Country)
jh_country %>% filter(Country %in% c("US")) %>% arrange(desc(Date), Country)

#----- Country populations -----

country_pop <-read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")
country_pop2 <- country_pop %>%
  select(Country, pop = Year_2016) %>%
  mutate(Country = case_when(Country == "United States" ~ "US", 
                             Country == "Iran, Islamic Rep." ~ "Iran",
                             TRUE ~ Country))

#----- top countries CASES per M -----
jh_country %>%
  inner_join(country_pop2) %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Spain", "Germany", "China", "France")) %>%
  filter(Date >= "2020-03-01") %>%
  mutate(CasesPerM = Confirmed / (pop / 1000000)) %>%
  ggplot(aes(x = Date, y = CasesPerM, color = Country)) +
  geom_line(size = 1) +
  #  scale_y_log10() +  
  labs(title = "covid-19 CASES Per Million by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = "output/covid19_country_per_m_cases.png", width = 10, height = 6, dpi = 120)

#----- top countries DEATHS per M -----
jh_country %>%
  inner_join(country_pop2) %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Spain", "Germany", "China", "France")) %>%
  filter(Date >= "2020-03-01") %>%
  mutate(DeathsPerM = Deaths / (pop / 1000000)) %>%
  ggplot(aes(x = Date, y = DeathsPerM, color = Country)) +
  geom_line(size = 1) +
  #  scale_y_log10() +  
  labs(title = "covid-19 DEATHS Per Million by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = paste0(getwd(), "/output/covid19_country_per_m_deaths.png"), width = 10, height = 6, dpi = 120)


#----- top countries New cases -----
jh_country %>%
  inner_join(country_pop2) %>%
  filter(Country %in% c("Italy", "Iran", "US", "Spain", "Spain", "Germany", "China", "France")) %>%
  filter(Date >= "2020-03-01") %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(prev = lag(Confirmed, n = 1),
         delta = Confirmed - prev,
         deltaPerM = delta / (pop / 1000000)) %>%
  #View()
  ggplot(aes(x = Date, y = deltaPerM, color = Country)) +
  #geom_line(size = 1) +
  geom_smooth(size = 1) +
  #  scale_y_log10() +  
  labs(title = "covid-19 NEW CASES per capita by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       y = "New Cases Per Million",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = paste0(getwd(), "/output/covid19_country_per_m_new_cases.png"), width = 10, height = 6, dpi = 120)

group_by(county, state) %>%
  arrange(county, state, date) %>%
mutate(prev = lag(cases, n = 1),
       delta = cases - prev) %>%



#----- daily update v population -----

jh_daily <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-02-2020.csv")
                      
names(jh_daily) <- c("FIPS", "Admin2", "Province", "Country", "Date", "Lat", "Long", "Confirmed", "Deaths", "Recovered", "Active", "CombinedKey")

countries_of_interest <- c("US", "Italy", "China", "Canada", "Iran", "Switzerland", "Germany", "France", "Spain", "Switzerland")

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
  write_csv(path = "output/covid19_countries_high_confirmed_per_pop.csv")
  

#----- JH State/County level data -----

counties_pop <- read_csv("data/counties_pop_census_2018.csv")
counties_pop2 <- counties_pop %>% select(county, state, pop = pop_2018)


# this dataset appears to be unreliable.  It has several instances of the cumulative cases dropping
# jh_county <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

jh_county2 <- jh_county %>%
  gather(key = date, value = cases, ends_with("20")) %>%
  select(county = Admin2, state = Province_State, date, cases)
jh_county2$date <- mdy(jh_county2$date)

jh_county2 %>%
  filter(state == "Tennessee" & county %in% c("Williamson", "Davidson", "Shelby")) %>%
  filter(date > '2020-03-01') %>%
  group_by(county, state) %>%
  arrange(county, state, date) %>%
  mutate(prev = lag(cases, n = 1),
         delta = cases - prev) %>%
  ungroup() %>%
  View()
  ggplot(aes(date, delta, group = county, color = county )) +
  #geom_smooth()
  geom_line()  
  
  ?geom_smooth


jh_county2 %>%
  inner_join(counties_pop2) %>%
  filter(state == "Tennessee" & county %in% c("Williamson", "Davidson", "Shelby")) %>%
  filter(date > '2020-03-05') %>%
  mutate(CasesPerM = cases / (pop / 1000000)) %>%
  ggplot(aes(date, CasesPerM, group = county, color = county )) +
  geom_line()
  View()

#----- Sweden, Denmark, Norway one-off -----
jh_country %>%
  inner_join(country_pop2) %>%
  filter(Country %in% c("Sweden", "Denmark", "Norway")) %>%
  filter(Date >= "2020-03-10") %>%
  mutate(DeathsPerThou = Deaths / (pop / 1000) * 100) %>%
  ggplot(aes(x = Date, y = DeathsPerThou, color = Country)) +
  geom_line(size = .7) +
  labs(title = "covid-19 Cases per Thousand Population by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = "output/covid19_deaths_per_pop_by_country_sweden.png", width = 8, height = 5, dpi = 120)

