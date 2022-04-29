#----- tidycensus API -----
library(tidyverse)
library(tidycensus)
rm(list = ls())

race_vars <- c(
  White    = "B03002_003",
  Black    = "B03002_004" ,
  Native   = "B03002_005" ,
  Asian    = "B03002_006" ,
  HIPI     = "B03002_007" ,
  Hispanic = "B03002_012"
)

st_race <- get_acs (
  geography = "county" ,
  #state = "MS" , 
  variables = race_vars,
  summary_var = "B03002_001",
  survey = "acs5",
  show_call = "TRUE"
)
st_race

st_race_percent <- st_race %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>% 
  select(GEOID, NAME, variable, percent, estimate, moe, summary_est)

#st_race_percent %>% filter(NAME %in% c('Davidson County, Tennessee', 'Shelby County, Tennessee', 'Haywood County, Tennessee'))
#st_race_percent %>% filter(variable == "Hispanic") %>% arrange(desc(percent))
#st_race_percent %>% filter(variable == "Black") %>% arrange(desc(percent))
#st_race_percent %>% filter(grepl("Louisiana", NAME), variable == "Black") %>% arrange(NAME) %>% View()


largest_group <- st_race_percent %>% 
  group_by(NAME) %>% 
  filter(percent == max(percent))
largest_group %>% arrange(desc(percent)) %>% View()


#===== mapping with sf pkg =====
# https://www.youtube.com/watch?v=GMi1ThlGFMo

library(sf)
options(scipen=999)

# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-county-and-equivalent-national-shapefile
us_county_map <- st_read("D:/opendata/TIGER/tl_2019_us_county.shp") # full US county level shapefile
#str(us_county_map)

st_race_percent2 <- st_race_percent %>% 
  separate(NAME, c("county", "state_name"), ", ") %>% 
  inner_join(tidycensus::fips_codes, by = c("state_name" = "state_name", "county" = "county")) %>%
  #inner_join(tidycensus::fips_codes, by = c("GEOID" = "GEOID")) %>% 
  select(state_code, county_code, state, state_name, county, variable, percent)

dat <- st_race_percent %>% 
  separate(NAME, c("county", "state_name"), ", ") %>% 
  inner_join(tidycensus::fips_codes, by = c("state_name" = "state_name", "county" = "county")) %>% 
  filter(variable == "Asian") %>%
  filter(state %in% c("TN"))

map_and_data <- inner_join(us_county_map, dat, by = c("GEOID" = "GEOID"))

ggplot(map_and_data) +
  geom_sf(aes(fill = percent)) +
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  scale_fill_gradient(low = "#99EEBB", high = "#007755") +
  labs(title = "Tennessee Counties",
       subtitle = "Percent Asian",
       fill = "Percent",
       caption = "@bdill\nData: US Census Bureau ACS 5-year 2019")

#===== mapping with tmap =====
library(tmap)
tm_shape(map_and_data) +
  tm_polygons("percent", id = "NAME", palette = "Greens")
  title(main = "Montana Counties",
        sub = "Percent Native")

tmap_mode("view")  # set to interactive
tmap_last()        # redraw the previous map

# save to html file
test_map <- tmap_last()
tmap_save(test_map, "C:/temp/test_map.html")
