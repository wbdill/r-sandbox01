#----- tidycensus API -----
library(tidyverse)
library(tidycensus)
rm(list = ls())

# 1) get your own free census API key: https://api.census.gov/data/key_signup.html
#census_api_key("your_key_here", install = TRUE)  # 2) install it to your R environment
# then you can run tidycensus functions like get_acs()
# https://walker-data.com/tidycensus/reference/index.html
#?get_acs

race_vars <- c(
  White    = "B03002_003",
  Black    = "B03002_004" ,
  Native   = "B03002_005" ,
  Asian    = "B03002_006" ,
  HIPI     = "B03002_007" ,
  Hispanic = "B03002_012"
)

#----- County level data -----
county_race <- get_acs (
  geography = "county" ,
  #state = "MS" , 
  variables = race_vars,
  summary_var = "B03002_001",
  survey = "acs5",
  show_call = "TRUE"
)
county_race

county_race_percent <- county_race %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>% 
  select(GEOID, NAME, variable, percent, estimate, moe, summary_est)

county_race_percent2 <- county_race_percent %>% 
  separate(NAME, c("county", "state_name"), ", ") %>% 
  inner_join(tidycensus::fips_codes, by = c("state_name" = "state_name", "county" = "county")) %>%
  #inner_join(tidycensus::fips_codes, by = c("GEOID" = "GEOID")) %>% 
  mutate(GEOID = paste0(state_code, county_code)) %>% 
  select(GEOID, state_code, county_code, state, state_name, county, variable, percent, estimate, moe, summary_est)

largest_group <- county_race_percent %>% 
  group_by(NAME) %>% 
  filter(percent == max(percent))
largest_group %>% arrange(desc(percent)) %>% View()
?get_acs

#----- State level data -----
state_race <- get_acs (
  geography = "state" ,
  variables = race_vars,
  summary_var = "B03002_001",
  survey = "acs5",
  #geometry = TRUE,
  cache_table = TRUE,
  show_call = "TRUE"
)
state_race 

state_race_percent <- state_race %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>% 
  select(GEOID, NAME, variable, percent, estimate, moe, summary_est, geometry)

states_fips <- tidycensus::fips_codes %>% select(state_code, state, state_name) %>% distinct()
state_race_percent2 <- state_race_percent %>% 
  inner_join(states_fips, by = c("NAME" = "state_name")) %>%
  select(GEOID, state_code, state, state_name = NAME, variable, percent, moe, summary_est)





#=======================================================================
#===== mapping with sf pkg =====
#=======================================================================
# https://www.youtube.com/watch?v=GMi1ThlGFMo

library(sf)
options(scipen=999)
?tigris::states
#----- State maps -----
us_states <- tigris::states(cb = TRUE, resolution = "20m")
us_states_map <- tigris::shift_geometry(us_states)   # Alaska, HI, PR shifted and scaled
#ggplot(us_states_map) + geom_sf() + theme_void()

dat <- state_race_percent2 %>% 
  filter(variable == "Black")
  
map_and_data <- inner_join(us_states_map, dat, by = c("GEOID" = "GEOID"))  # map object must be first to preserve sf geometry variable

ggplot(map_and_data) +
  geom_sf(aes(fill = percent)) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  #scale_fill_gradient(low = "#99EEBB", high = "#003311") +
  labs(title = "US States",
       subtitle = "Percent Black",
       fill = "Percent",
       caption = "@bdill\nData: US Census Bureau ACS 5-year 2019")

#----- County maps -----
# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-county-and-equivalent-national-shapefile
# high res (and slow to render) shape file of US counties
us_county_map <- st_read("D:/opendata/TIGER/tl_2019_us_county.shp") # full US county level shapefile

# https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#shifting-and-rescaling-geometry-for-national-us-mapping
# lower res shape file of US counties using Albers Equal Area CRS
# good for entire country, but eastern or western states in isolation will appear tilted.
us_counties <- tigris::counties(cb = TRUE, resolution = "20m")
us_county_map <- tigris::shift_geometry(us_counties)  # Alaska, HI, PR shifted and scaled

var_state <- "TN"
var_race <- "Black"
dat <- county_race_percent2 %>% 
  filter(state == var_state) %>% 
  filter(variable == var_race)

map_and_data <- inner_join(us_county_map, dat, by = c("GEOID" = "GEOID"))

ggplot(map_and_data) +
  geom_sf(aes(fill = percent)) +
  #scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  scale_fill_gradient(low = "#99EEBB", high = "#003311") +
  labs(title = paste(var_state, "Counties"),
       subtitle = paste("Percent",var_race),
       fill = "Percent",
       caption = "@bdill\nData: US Census Bureau ACS 5-year 2019")

MapStateCounties <- function(x) {
  var_race <- x$race
  var_state <- x$state_abbrev
  dat <- county_race_percent2 %>% 
      filter(state == var_state) %>% 
      filter(variable == var_race)
  
  map_and_data <- inner_join(us_county_map, dat, by = c("GEOID" = "GEOID"))
  
  ggplot(map_and_data) +
    geom_sf(aes(fill = percent)) +
    scale_fill_gradient(low = "#99EEBB", high = "#003311") +
    labs(title = paste(var_state, "Counties"),
         subtitle = paste("Percent",var_race),
         fill = "Percent",
         caption = "@bdill\nData: US Census Bureau ACS 5-year 2019")
  filname <- paste0(var_state, "_", var_race, ".png")
  ggsave(paste0("D:/tmp/R/maps/", filname), dpi=300)
}

races <- c("White", "Black", "Native", "Asian", "HIPI", "Hispanic")
state_abbrevs <- dplyr::pull(states_fips, state)
state_race <- expand.grid(state_abbrevs, races)
names(state_race) <- c("state_abbrevs", "races")
state_race <- state_race %>% filter(state_abbrevs %in% c("MS")) %>% arrange(state_abbrevs, races)
by(state_race, 1:nrow(state_race), MapStateCounties)


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
