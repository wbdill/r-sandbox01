
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

#----- Read in data -----
jhconfirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
jhdeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
jhrecovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

jhcountries <- jhconfirmed %>% select(`Province/State`, `Country/Region`, Lat, Long)
names(jhcountries) <- c("Province", "Country", "Lat", "Long")

#----- Edit date for jh_daily ----->>>>>
jh_daily <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-30-2020.csv")

names(jh_daily) <- c("FIPS", "Admin2", "Province", "Country", "Date", "Lat", "Long", "Confirmed", "Deaths", "Recovered", "Active", "CombinedKey")


#----- data wrangle -----
wrangle_data <- function(x, val_col){
  y <- select(x, -Lat, -Long) %>%
    gather(key = "date", value = val_col, -`Province/State`, -`Country/Region`)
  y$date <- mdy(y$date)
  names(y) <- c("Province", "Country", "Date", val_col)
  y
}
jhconfirmed2 <- wrangle_data(jhconfirmed, "confirmed")
jhdeaths2 <- wrangle_data(jhdeaths, "deaths")
jhrecovered2 <- wrangle_data(jhrecovered, "recovered")

jh_cdr <- inner_join(jhconfirmed2, jhdeaths2) %>%
  inner_join(jhrecovered2) %>%
  arrange(Country, Province, desc(Date))

jh_cdr_by_country <- jh_cdr %>%
  group_by(Country, Date) %>%
  summarize(Confirmed = sum(confirmed),
            Deaths= sum(deaths),
            Recovered = sum(recovered)) %>%
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
  ggplot(aes(x = Date, y = confirmed, color = Province)) +
  geom_line() +
  labs(title = "Confirmed covid-19 cases by Province in China",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

#ggsave(filename = paste0(getwd(), "/output/covid19_by_china_province.png"), width = 10, height = 6, dpi = 120)


#----- Top countries CASES -----

top7_countries_cases <- jh_cdr_by_country %>%
  group_by(Country) %>%
  summarize(confirmed = max(Confirmed)) %>%
  top_n(7, confirmed)

jh_cdr_by_country %>%
  filter(Country %in% pull(top7_countries_cases, Country)) %>%
  filter(Date >= "2020-03-01") %>%
  ggplot(aes(x = Date, y = Confirmed, color = Country)) +
  geom_line(size = 1) +
#  scale_y_log10() +  
  labs(title = "covid-19 Cases by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")

ggsave(filename = "output/covid19_country_cases.png", width = 10, height = 6, dpi = 120)


#----- Top countries DEATHS -----
top7_countries_deaths <- jh_cdr_by_country %>%
  group_by(Country) %>%
  summarize(deaths = max(Deaths)) %>%
  top_n(7, deaths)

jh_cdr_by_country %>%
  filter(Country %in% pull(top7_countries_deaths, Country)) %>%
  filter(Date >= "2020-03-01") %>%
  ggplot(aes(x = Date, y = Deaths, color = Country)) +
  geom_line(size = 1) +
  #scale_y_log10(limits = c(10, 10000)) +  
  labs(title = "covid-19 Deaths by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = paste0(getwd(), "/output/covid19_country_deaths.png"), width = 10, height = 6, dpi = 120)


#jh_country %>% filter(Country %in% c("Italy", "Iran", "US", "Spain", "Germany", "China")) %>% arrange(desc(Date), Country)
#jh_cdr_by_country %>% filter(Country %in% c("US")) %>% arrange(desc(Date), Country)

#----- Country populations -----

country_pop <-read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")
country_pop2 <- country_pop %>%
  select(Country, pop = Year_2016) %>%
  mutate(Country = case_when(Country == "United States" ~ "US", 
                             Country == "Iran, Islamic Rep." ~ "Iran",
                             TRUE ~ Country))

jh_cdr_by_country_per_pop <- jh_cdr_by_country %>%
  inner_join(country_pop2) %>%
  #filter(Country %in% c("Italy", "US", "Spain", "Germany", "France",  "Turkey")) %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  filter(Date >= "2020-03-01") %>%
  mutate(CasesPerM = round(Confirmed / (pop / 1000000), 2),
         DeathsPerM = round(Deaths / (pop / 1000000), 2),
         prev = lag(Confirmed, n = 1),
         delta = Confirmed - prev,
         deltaPerM = delta / (pop / 1000000)) 

#write_csv(jh_cdr_by_country_per_pop, "output/jh_covid19_countries_CDM_per_pop_raw.csv")
#View(jh_cdr_by_country_per_pop)

#----- top countries CASES per M -----
top7_countries_cases_pop <- jh_cdr_by_country_per_pop %>%
  group_by(Country) %>%
  filter(pop > 10000000) %>%
  summarize(confirmed = max(Confirmed),
            CasesPerM = confirmed / max(pop) * 100) %>%
  top_n(7, CasesPerM)

jh_cdr_by_country_per_pop %>%
  filter(Country %in% pull(top7_countries_cases_pop, Country)) %>%
  ggplot(aes(x = Date, y = CasesPerM, color = Country)) +
  geom_line(size = 1) +
  labs(title = "covid-19 CASES Per Million by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
ggsave(filename = "output/covid19_country_per_m_cases.png", width = 10, height = 6, dpi = 120)

#----- top countries DEATHS per M -----
top7_countries_deaths_pop <- jh_cdr_by_country_per_pop %>%
  group_by(Country) %>%
  filter(pop > 10000000) %>%
  summarize(deaths = max(Deaths),
            DeathsPerM = deaths / max(pop) * 100) %>%
  top_n(7, deaths)

jh_cdr_by_country_per_pop %>%
  filter(Country %in% pull(top7_countries_deaths_pop, Country)) %>%
  ggplot(aes(x = Date, y = DeathsPerM, color = Country)) +
  geom_line(size = 1) +
  labs(title = "covid-19 DEATHS Per Million by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"
       )

ggsave(filename = "output/covid19_country_per_m_deaths.png", width = 10, height = 6, dpi = 120)


#----- top countries New cases -----


jh_cdr_by_country_per_pop %>%
  filter(Country %in% c("Chile", "Peru", "Brazil", "Italy", "Spain", "United Kingdom", "US", "Belgium")) %>%
  ggplot(aes(x = Date, y = deltaPerM, color = Country)) +
  geom_smooth(size = 1, se = FALSE) +
  #geom_line() +
  labs(title = "covid-19 NEW CASES per capita by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       y = "New Cases Per Million",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"
       )

ggsave(filename = "output/covid19_country_per_m_new_cases_per_m.png", width = 10, height = 6, dpi = 120)

#----- top countries New cases -----
jh_cdr_by_country_per_pop %>%
  filter(Country %in% c("Brazil", "Chile", "Germany", "Peru", "United Kingdom", "US")) %>%
  ggplot(aes(x = Date, y = delta, color = Country)) +
  geom_smooth(size = 1) +
  #geom_line() +
  labs(title = "covid-19 NEW CASES  by Country",
       subtitle = "Data Repository by Johns Hopkins CSSE",
       y = "New Cases",
       caption = "Graph: @bdill  data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"
  )

ggsave(filename = "output/covid19_country_new_cases.png", width = 10, height = 6, dpi = 120)

#----- daily update v population -----


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
  write_csv(path = "output/jh_covid19_countries_CDM_per_pop.csv")
  

#----- JH State/County level data (not used) -----

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

