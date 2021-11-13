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
library(RColorBrewer)
library(scales)
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
  geom_sf(aes(fill = pct_dem, geometry = geometry)) +
  scale_fill_steps2(
    n.breaks = 7,
    low = muted("red"),
    mid = "white",
    high = muted("blue"),
    midpoint = 50,
    guide = "colorsteps"
  ) +
  labs(title = "2020 Presidential Election",
       fill = "% Vote Dem.",
       caption = "Chart: @bdill\nData: dataverse.harvard.edu")

#===== map using tmap / tmaptools / leaflet =====

data_and_map %>% filter(state %in% c("TN")) %>% 
  tm_shape() +
  tm_polygons("pct_rep", id = "NAME", palette = "Greens") 

tmap_mode("view")
tmap_save(tmap_last(), "TN_2020_elec.html")  # save off to single html file w/ embedded data.
getwd()

#===== How to Create Amazing Maps of India with ggmap, Google Maps and RStudio =====
# https://www.youtube.com/watch?v=ItaV0Y6l4ns
install.packages("ggmap")
library(ggmap)
library(tidyverse)


# manually signed up for a Google maps API: https://mapsplatform.google.com/
# saved my personal API key in .Renviron
ggmap::register_google(key = Sys.getenv("GOOGLEMAP_API_KEY"))

t <- "
state lat long size color city
TN 36.167870 -86.778160 3 blue Nashville
TN 36.033115 -86.782776 1 yellow Brentwood
TN 35.850098 -86.381950 1 blue Murfreesboro
TN 35.149760 -90.049250 3 green Memphis
TN 35.964668 -83.926453 2 orange Knoxville
"
df <- read.table(text = t, header = TRUE)
str(df)

get_map("Tennessee", zoom = 7) %>% 
  ggmap() +
  geom_point(data = df, aes(x = long, y = lat), color = df$color, size = df$size)
?ggmap
