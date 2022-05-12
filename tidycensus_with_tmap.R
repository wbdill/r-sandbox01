# Using tidycensus and tmap packages.

# https://www.youtube.com/watch?v=KMOTZij9qhU
install.packages("rosm")
library(tidyverse)
library(tidycensus)
library(tmap)
library(rosm)

tmap_mode("plot")
tmap_mode("view")

## 1) get your own free census API key: https://api.census.gov/data/key_signup.html
#census_api_key("your_key_here", install = TRUE)  # 2) install it to your R environment
## then you can run tidycensus functions like get_acs()
## https://walker-data.com/tidycensus/reference/index.html

#https://api.census.gov/data/2020/dec/pl/variables.html

county_race <- get_decennial(
  geography = "county",
  #state = "MT",
  #county = c("Lowndes County", "Clay", "Oktibbeha"),
  variables = c(
    Hispanic = "P2_002N",
    White    = "P2_005N",
    Black    = "P2_006N",
    Native   = "P2_007N",
    Asian    = "P2_008N"
    #NHOPI    = "P2_009N",
    #Other    = "P2_010N",
    #TwoOrMore = "P2_011N"    
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>% 
  mutate(percent = 100 * (value / summary_value))

fips <- tidycensus::fips_codes %>% mutate(GEOID = paste0(state_code, county_code))
county_race2 = county_race %>% inner_join(fips)
#county_race
county_asian <- filter(county_race, variable == "Asian")
county_black <- filter(county_race, variable == "Black")
county_white <- filter(county_race, variable == "White")
county_native <- filter(county_race, variable == "Native")

county_race2 <- county_race %>% select(GEOID, NAME, variable, value, summary_value, percent)
county_race3 <- county_race2 %>% pivot_wider(names_from = variable, values_from = value)
#tidycensus::load_variables(year = 2020, dataset = "sf1") %>% View()
#?load_variables
#tmaptools::palette_explorer()

# ----- basic map ----------------------------------------
tm_shape(county_black) + 
  tm_polygons(col = "percent")

# fancier map
tm_shape(county_native) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "2020 US Census",
              
              legend.hist = TRUE
            ) +
  tm_layout(title = "Percent Native",
            legend.outside = TRUE,
            frame = TRUE
            )

# ----- facet map  ----------------------------------------
#tmap_mode("view")
tmap_mode("plot")

dat <- county_race2 %>% filter(state != "AK", state != "HI", state != "PR") %>% 
  filter(state %in% c("AL", "MS", "LA", "TN", "GA", "FL", "AR", "SC")) %>%
  #filter(state %in% c("MT")) %>% 
  filter(variable %in% c("Hispanic", "Black", "White"))
facet_map <- tm_shape(dat) +
  tm_facets(by = "variable", scale.factor = 4) +
  tm_polygons(col = "percent",
              style = "fisher",
              n = 7,
              palette = "Blues") +
  tm_layout(title = "",
            #title.position = c(-.1, 0.4),
            #legend.position = c(-0.7, 0.1),
            legend.outside = TRUE
  )  
facet_map
?tm_polygons

##----- dot density map ----------------------------------
#remotes::install_github("walkerke/tidycensus")
#tmap_mode("view")
tmap_mode("plot")
county_race_fordots <- county_race2 %>% filter(state == "MS")
county_dots <- county_race_fordots %>% 
  as_dot_density(             #<< new function only in dev build
    value = "value",
    values_per_dot = 1000,
    group = "variable"
  )
county_base <- county_race_fordots %>% 
  distinct(GEOID, .keep_all = TRUE)

tm_shape(county_base) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(county_dots) +
  tm_dots(col = "variable",
          palette = "Set1",
          alpha = 0.5,
          size = 0.01,
          ) +
  tm_layout(legend.outside = TRUE,
            title = "Density\nby race")
tmap_save(tmap_last(), "D:/tmp/R/maps/MS_Col_WestPt_Stkvl.html")

## ---------------- raster background ------------------------------
library(rosm)
library(sf)

basemap <- osm.raster(
  st_bbox(county_asian),
  zoom = 10,
  type = "cartolight",
  crop = TRUE
)

tm_shape(basemap) +
  tm_rgb()

tm_shape(basemap) +
  tm_rgb() +
  tm_shape(county_asian) +
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "2020 US Census",
              alpha = 0.6) +
  tm_layout(title = "Percent Asian\nby Census tract",
            legend.outside = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top")) +
  tm_credits("Basemap (c) CARO, OSM",
             bg.color = "#eeeedd",
             position = c("right", "bottom"),
             bg.alpha = 0.5,
             align = "right")

