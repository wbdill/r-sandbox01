library(tidyverse)

# https://www.unitedstateszipcodes.org/
zips <- read_csv("C:/Users/brian.dill/Downloads/zip_code_database.csv")

# problems(zips) %>% View()

#zips %>% group_by(timezone) %>% count() %>% arrange(desc(n))

zips2 <- zips %>% filter(decommissioned == 0) %>% 
  select(zip, type, primary_city, state, county, country, timezone, latitude, longitude, pop = irs_estimated_population_2015, acceptable_cities, unacceptable_cities)

#zips2 %>% arrange(desc(pop))
#zips2 %>% filter(primary_city == "Brentwood")

#zips2 %>% select(state, county, primary_city) %>% count()
#zips2 %>% group_by(state, county, primary_city) %>% filter(!is.na(county)) %>% count(name = "num_zip_codes") %>% arrange(state, primary_city)
#zips2 %>% filter(state == "DC") %>% View()



cities <- zips2 %>% group_by(state, county, primary_city) %>% 
  filter(!is.na(county)) %>% 
  count(name = "num_zip_codes") %>% 
  arrange(state, county, primary_city)

write_csv(cities, "C:/Users/brian.dill/Downloads/us_cities.csv")
