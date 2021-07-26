library(tidyverse)

# https://www.unitedstateszipcodes.org/
zips <- read_csv("C:/Users/brian.dill/Downloads/zip_code_database.csv")

# problems(zips) %>% View()

#zips %>% group_by(timezone) %>% count() %>% arrange(desc(n))

fips_county <- read_csv("http://briandill.com/data/census_county_pop.csv")
View(fips_county)
zips_clean <- zips %>% filter(decommissioned == 0) %>% 
  left_join(fips_county, by = c("state" = "state_abbrev", "county")) %>%
  select(zip_code = zip, fips_state, fips_county, state_abbrev = state, state = state.y, county, city = primary_city, country, timezone, latitude, longitude, type,
         pop = irs_estimated_population_2015, acceptable_cities, unacceptable_cities) 
View(zips_clean)
#zips_clean %>% group_by(country) %>% count() %>% arrange(desc(n))

write_csv(zips_clean, "C:/Users/brian.dill/Downloads/zips_clean.csv", na =)
?write_csv

#zips2 %>% arrange(desc(pop))
#zips_clean %>% filter(city == "Brentwood")

#zips2 %>% select(state, county, primary_city) %>% count()
#zips2 %>% group_by(state, county, primary_city) %>% filter(!is.na(county)) %>% count(name = "num_zip_codes") %>% arrange(state, primary_city)
#zips2 %>% filter(state == "DC") %>% View()



cities <- zips2 %>% group_by(state_abbrev, county, primary_city) %>% 
  filter(!is.na(county)) %>% 
  count(name = "num_zip_codes") %>% 
  arrange(state_abbrev, county, primary_city) %>% 
  left_join(fips_county, by = c("state_abbrev", "county")) %>% 
  select(fips_state, fips_county, fips_county3, state_abbrev, state, county, city = primary_city, num_zip_codes)

write_csv(cities, "C:/Users/brian.dill/Downloads/us_cities.csv")
