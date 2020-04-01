# New York Times covid19 data
# Brian Dill 2020-03-27
# https://github.com/nytimes/covid-19-data
# https://en.wikipedia.org/wiki/FIPS_county_code
# County populations: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
rm(list = ls())
library(tidyverse)

state_pop <- read_csv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/state_populations_2019.csv")
nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#----- US counties and populations -----

counties_pop <- read_csv("data/PEP_2018_PEPANNRES.csv", col_types = "ccciiiiiiiiiii")  # force col2 to be string to prevent stripping of leading zero
names(counties_pop) <- c("geo_id", "fips_code", "county", "census_2010", "est_base_2010", "pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", "pop_2015", "pop_2016", "pop_2017", "pop_2018" )
counties_pop <- separate(counties_pop, county, c("county", "state"), sep = ", ")
counties_pop$county <- str_replace(counties_pop$county, " County", "")
write_csv(counties_pop, "data/counties_pop_census_2018.csv")


#----- Function to generate graph for each state -----

MapState <- function(x) {
  state_name <- x$state_name
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
  
  filename <- paste("output/by_state/nyt_top7_counties_", state_abbrev, ".png")
  ggsave(filename = filename, width = 16, height = 10, units = "cm")
}

by(state_pop, 1:nrow(state_pop), MapState)



#----- Manual EDA -----

nyt_counties %>%
  filter(state == "Tennessee" & county == "Davidson") %>%
  arrange(desc(date))

top7_counties <- nyt_counties %>%
  filter(state == "Tennessee" & date == "2020-03-25") %>%
  top_n(7, cases) %>%
  arrange(desc(cases))

pull(top7_counties, county)

nyt_counties %>%
  filter(state == "Tennessee" & county %in% pull(top7_counties, county)) %>%
  ggplot(aes(date, cases, color = county)) +
  geom_line(size = .7) +
  labs(title = "Cumulative covid19 Cases - Tennessee",
      subtitle = "Top 7 Counties",
      y = "Cumulative Cases",
      caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_top7_tn_counties.png", width = 16, height = 10, units = "cm")


nyt_counties %>%
  filter(state == "Tennessee" & date == "2020-03-25") %>%
  #top_n(7, cases) %>%
  arrange(desc(cases)) %>%
  View()

