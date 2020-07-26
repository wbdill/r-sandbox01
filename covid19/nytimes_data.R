# New York Times covid19 data
# Brian Dill 2020-03-27
# https://github.com/nytimes/covid-19-data
# https://en.wikipedia.org/wiki/FIPS_county_code
# County populations: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
rm(list = ls())
#install.packages("zoo")
library(tidyverse)
library(zoo)

state_pop <- read_csv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/state_populations_2019.csv")
nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#----- US counties and populations -----

counties_pop <- read_csv("data/PEP_2018_PEPANNRES.csv", col_types = "ccciiiiiiiiiii")  # force col2 to be string to prevent stripping of leading zero
names(counties_pop) <- c("geo_id", "fips_code", "county", "census_2010", "est_base_2010", "pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", "pop_2015", "pop_2016", "pop_2017", "pop_2018" )
counties_pop <- separate(counties_pop, county, c("county", "state"), sep = ", ")
counties_pop$county <- str_replace(counties_pop$county, " County", "")
#write_csv(counties_pop, "data/counties_pop_census_2018.csv")


#----- Function to generate graph for each state -----

MapStateCounties <- function(x) {
  state_name <- x$state
  state_abbrev <- x$state_abbrev
  
  top7_counties <- nyt_counties %>%
    filter(state == state_name & date == "2020-03-28") %>%
    top_n(7, cases) %>%
    arrange(desc(cases))
  
  gtitle = paste("Cumulative covid19 Cases - ", state_name)
  nyt_counties %>%
    filter(state == state_name & county %in% pull(top7_counties, county)) %>%
    filter(date > '2020-03-07') %>%
    ggplot(aes(date, cases, color = county)) +
    geom_line(size = .7) +
    labs(title = gtitle,
         subtitle = "Top 7 Counties",
         y = "Cumulative Cases",
         caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
  
  filename <- paste("output/by_state/cumcases_top_counties/nyt_top7_counties_", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")
}

by(state_pop, 1:nrow(state_pop), MapStateCounties)


#----- New cases and New Cases per M
MapStateVarious <- function(x) {
  state_name <- x$state
  state_abbrev <- x$state_abbrev
  
  
  dat <- nyt_counties %>%
    filter(state == state_name ) %>%  #& county %in% pull(top7_counties, county)
    #filter(state == "state_name"Tennessee" ) %>%
    group_by(state, date) %>% 
    select(date, state, cases, deaths) %>% 
    mutate(cases = sum(cases),
           deaths = sum(deaths)) %>% 
    distinct() %>% 
    group_by(state) %>% 
    inner_join(state_pop, by = "state") %>% 
    mutate(cases_per_m = cases / (population / 1000000), 
           new_cases = cases - dplyr::lag(cases), 
           new_cases_avg7 = rollmean(new_cases, 7, fill = NA, align="right"),
           new_cases_per_m = new_cases / (population / 1000000),
           new_cases_per_m_avg7 = rollmean(new_cases_per_m, 7, fill = NA, align="right"),
           new_deaths = deaths - dplyr::lag(deaths), 
           new_deaths_per_m = new_deaths / (population / 1000000),
           new_deaths_per_m_avg7 = rollmean(new_deaths_per_m, 7, fill = NA, align="right") )
    
  # new cases
  gtitle = paste("COVID19: New Cases - ", state_name)
  ggplot(dat, aes(date, new_cases)) +
  geom_col(aes(date, new_cases), fill = "blue", alpha = 0.3) +
  geom_line(aes(date, new_cases_avg7), color = "red") +
  labs(title = gtitle,
       subtitle = "7 Day Moving Average in red",
       y = "New Cases",
       caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")

  filename <- paste0("output/by_state/new_cases/nyt_new_cases_", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")

  # new cases per M
  gtitle = paste("COVID19: New Cases per M - ", state_name)
  ggplot(dat ) +
    geom_col(aes(date, new_cases_per_m), fill = "blue", alpha = 0.3) +
    geom_line(aes(date, new_cases_per_m_avg7), color = "red") +
    labs(title = gtitle,
         subtitle = "7 Day Moving Average in red",
         y = "New Cases Per M",
         caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
  
  filename <- paste0("output/by_state/new_cases_per_pop/nyt_new_cases_per_m_", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")  

  # new deaths per M
  gtitle = paste("COVID19: New Deaths per M - ", state_name)
  ggplot(dat ) +
    geom_col(aes(date, new_deaths_per_m), fill = "blue", alpha = 0.3) +
    geom_line(aes(date, new_deaths_per_m_avg7), color = "red") +
    labs(title = gtitle,
         subtitle = "7 Day Moving Average in red",
         y = "New Deaths Per M",
         caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
  
  filename <- paste0("output/by_state/new_deaths_per_pop/nyt_new_deaths_per_m_", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")    
  
}

by(state_pop, 1:nrow(state_pop), MapStateVarious)


#--- Misc EDA
state_name <- "Tennessee"
nyt_counties %>%
  filter(state == state_name ) %>%  #& county %in% pull(top7_counties, county)
  group_by(state, date) %>% 
  select(date, state, cases, deaths) %>% 
  mutate(cases = sum(cases),
         deaths = sum(deaths)) %>% 
  distinct() %>% 
  group_by(state) %>% 
  inner_join(state_pop, by = "state") %>% 
  mutate(cases_per_m = cases / (population / 1000000), 
         new_cases = cases - dplyr::lag(cases), 
         new_cases_per_m = new_cases / (population / 1000000),
         new_cases_per_m_avg7 = rollmean(new_cases_per_m, 7, fill = NA, align="right"),         
         new_deaths = deaths - dplyr::lag(deaths), 
         new_deaths_per_m = new_deaths / (population / 1000000),
         new_deaths_per_m_avg7 = rollmean(new_deaths_per_m, 7, fill = NA, align="right") ) %>% 
  #View()
  ggplot(aes(date, new_deaths_per_m)) +
  geom_col( fill = "blue", alpha = .3) +
  geom_line(aes(date, new_deaths_per_m_avg7) ,size = 1, color = "red")


#----- New Cases by state -----
nyt_counties %>%
  filter(date > "2020-04-01") %>% 
  filter(state %in% c("California", "Arizona", "Texas", "Florida", "Georgia", "Oklahoma", "South Carolina")) %>% 
  group_by(state, date) %>% 
  select(date, state, cases, deaths) %>% 
  mutate(cases = sum(cases), deaths = sum(deaths)) %>% 
  distinct() %>% 
  group_by(state) %>% 
  inner_join(state_pop, by = "state") %>% 
  mutate(cases_per_m = cases / (population / 1000000), 
         new_cases = cases - dplyr::lag(cases), 
         new_cases_per_m = new_cases / (population / 1000000),
         new_deaths = deaths - lag(deaths),
         new_deaths_per_m = new_deaths / (population / 1000000)) %>% 
  ggplot(aes(date, new_deaths, color = state)) +
  geom_smooth(size = .7) +
  labs(title = "COVID19: New Deaths",
       subtitle = "Top States",
       y = "New Cases",
       caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_new_cases_states.png", width = 16, height = 10, units = "cm")

#----- New Cases per m by state -----
nyt_counties %>%
  filter(date > "2020-05-15") %>% 
  filter(state %in% c("California", "Arizona", "Texas", "Florida", "Georgia", "Idaho", "South Carolina", "Louisiana")) %>% 
  #filter(state %in% c("New York", "New Jersey", "Illinois", "Michigan", "Virginia", "Colorado")) %>% 
  group_by(state, date) %>% 
  select(date, state, cases) %>% 
  mutate(cases = sum(cases)) %>% 
  distinct() %>% 
  group_by(state) %>% 
  inner_join(state_pop, by = "state") %>% 
  mutate(cases_per_m = cases / (population / 1000000), 
         new_cases = cases - dplyr::lag(cases), 
         new_cases_per_m = new_cases / (population / 1000000)) %>% 
  #filter(new_cases_per_m > 300) %>% 
  #View()
  ggplot(aes(date, new_cases_per_m, color = state)) +
  geom_smooth(size = .7, se = FALSE) +
  labs(title = "COVID19: New Cases per M",
       subtitle = "States",
       y = "New Cases Per Mill",
       caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_new_cases_per_m_states.png", width = 16, height = 10, units = "cm")



#----- New Deaths per m by state -----
nyt_counties %>%
  filter(date > "2020-04-01", date < "2020-05-25") %>% 
  filter(state %in% c("California", "Arizona", "Texas", "Florida", "Georgia", "Oklahoma", "South Carolina")) %>% 
  #filter(state %in% c("New York", "New Jersey", "Illinois", "Michigan", "Virginia", "Colorado")) %>% 
  group_by(state, date) %>% 
  select(date, state, cases, deaths) %>% 
  mutate(cases = sum(cases), deaths = sum(deaths)) %>% 
  distinct() %>% 
  group_by(state) %>% 
  inner_join(state_pop, by = "state") %>% 
  mutate(cases_per_m = cases / (population / 1000000), 
         new_cases = cases - dplyr::lag(cases), 
         new_cases_per_m = new_cases / (population / 1000000),
         new_deaths = deaths - lag(deaths),
         new_deaths_per_m = new_deaths / (population / 1000000)) %>% 
#  arrange(state, date) %>% View()
  ggplot(aes(date, new_deaths_per_m, color = state)) +
  geom_smooth(size = .7, se = FALSE) +
  labs(title = "COVID19: New Deaths per Mill",
       subtitle = "States",
       y = "New Deaths Per Mill",
       caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_new_deaths_per_m_states.png", width = 16, height = 10, units = "cm")




#----- Manual EDA -----

nyt_counties %>%
  filter(state == "Tennessee" & county == "Davidson") %>%
  arrange(desc(date))

top7_counties <- nyt_counties %>%
  filter(state == "Tennessee" & date == "2020-06-20") %>%
  top_n(7, cases) %>%
  arrange(desc(cases))

pull(top7_counties, county)

nyt_counties %>%
  filter(state == "Tennessee" & county %in% pull(top7_counties, county)) %>%
  filter(date > "2020-04-01") %>% 
  ggplot(aes(date, cases, color = county)) +
  geom_line(size = .7) +
  labs(title = "Cumulative covid19 Cases - Tennessee",
      subtitle = "Top 7 Counties",
      y = "Cumulative Cases",
      caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_top7_tn_counties.png", width = 16, height = 10, units = "cm")



nyt_counties %>%
  filter(state %in% c("Mississippi") & county %in% c("Warren", "Lowndes", "Monroe", "Hinds") & date > "2020-03-14") %>%
  #top_n(7, cases) %>%
  arrange(desc(county, date)) %>%
  View()


#----- Mapping by county -----

library(urbnmapr)
library(urbnthemes)

library(RColorBrewer)
# display.brewer.all()
# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n).

nyt_counties %>%
  group_by(state, county, fips) %>%
  summarize(cases = max(cases),
            deaths = max(deaths)) %>%
  filter(state == "Tennessee" & fips == 47187)

#----- Single state -----

graph_state_heatmap <- function(p_state, p_measure) {
  dt = max(nyt_counties$date)
  subt = ""  
  if(p_measure == "mortality") {
    subt = "Mortality = deaths as % of cases. Cases < 20, mortality ratio unreliable (dark gray)"  
  }
  nyt_counties %>%
    group_by(state, county, fips) %>%
    filter(state == p_state) %>% 
    summarize(cases = max(cases),
              deaths = max(deaths)) %>% 
    mutate(mortality = case_when(cases > 20 ~ deaths / cases * 100,
                                   TRUE ~ NA_real_)) %>%
    left_join(counties, by = c("fips" = "county_fips")) %>% 
    ggplot(mapping = aes(long, lat, group = group )) +
    geom_polygon(color = "#000000", size = .25, aes_string(fill = p_measure)) +
    scale_fill_gradientn(colors = (brewer.pal(8, "YlOrRd")),
                         guide = guide_colorbar(title.position = "top")) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme(legend.title = element_text(),
          legend.key.width = unit(.25, "in"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.subtitle=element_text(size=7, hjust=0.0, color="black"),
          plot.caption=element_text(size=6, hjust=0.0, color="black")) +
    labs(fill = p_measure,
         title = paste0(p_state, " covid19 ", p_measure, " (",dt,")"),
         subtitle = subt,
         caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv") 
    #theme_urbn_map()
  filename <- paste0("output/by_state_maps/nyt_covid_", p_state, "_", p_measure, ".png")
  ggsave(filename = filename)  
  
}
graph_state_heatmaps <- function(p_state) {
  graph_state_heatmap(p_state, "cases")
  graph_state_heatmap(p_state, "deaths")
  graph_state_heatmap(p_state, "mortality")
}
graph_state_heatmaps("Tennessee")
graph_state_heatmaps("Georgia")
graph_state_heatmaps("Louisiana")
graph_state_heatmaps("Kentucky")
graph_state_heatmaps("Florida")


 nyt_counties %>%
  group_by(state, county, fips) %>%
  filter(state == "Tennessee") %>% 
  summarize(cases = max(cases),
            deaths = max(deaths),
            date = max(date)) %>% 
  mutate(mortality = case_when(cases > 20 ~ deaths / cases * 100,
                               TRUE ~ NA_real_)) %>%
  arrange(desc(mortality)) %>%
  write_csv("output/tn_mortality_massaged_bdill.csv")

nyt_counties %>%
  group_by(state, county, fips) %>%
  filter(state == p_state) %>% 
  summarize(cases = max(cases),
            deaths = max(deaths)) %>% 
  mutate(mortality = case_when(cases > 20 ~ deaths / cases * 100,
                               TRUE ~ NA_real_)) %>%
  left_join(counties, by = c("fips" = "county_fips"))