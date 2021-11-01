#install.packages("Rcpp")  # updating Rcpp fixed the issue of R hanging while trying to load library(sf)
#install.packages("sf")
#install.packages("tmap")
#install.packages("tmaptools")
#install.packages("leaflet")

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(leaflet)
rm(list = ls())

# Census shapefiles
# https://www.census.gov/geographies/mapping-files.html
# https://www2.census.gov/geo/tiger/TIGER2021/COUNTY/tl_2021_us_county.zip

# https://www.youtube.com/watch?v=wgFVmzSbaQc
# https://www.infoworld.com/video/series/8563/do-more-with-r
elecpath <- "D:/opendata/PresidentialElection2000_2020_county/countypres_2020_munged.csv"
sfpath <- "D:/opendata/census.gov/shapefiles/tl_2021_us_county/tl_2021_us_county.shp"

elec <- read_csv(elecpath)
map <- st_read(sfpath, stringsAsFactors = FALSE)

# zero pad the 5 digit FIPS.  Also split out 2 digit state and 3 digit county FIPS code for joining to census shapefile
elec <- elec %>% 
  mutate(county_fips5 = str_pad(county_fips, 5, pad = "0") ,
          county_fips3 = substr(county_fips5, 3, 5),
          state_fips = substr(county_fips5, 1, 2))

# put map as first table in inner_join to maintain the "sf" class.
data_and_map <- map %>% 
  inner_join(elec, by = c("COUNTYFP" = "county_fips3", "STATEFP" = "state_fips"))

#===== map using ggplot2 =====

data_and_map %>% filter(state %in% c("TN")) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_rep, geometry = geometry))

#===== map using tmap / tmaptools / leaflet =====

data_and_map %>% filter(state %in% c("TN")) %>% 
  tm_shape() +
  tm_polygons("pct_rep", id = "NAME", palette = "Greens")

tmap_mode("view")
tmap_save(tmap_last(), "TN_2020_elec.html")  # save off to single html file w/ embedded data.


