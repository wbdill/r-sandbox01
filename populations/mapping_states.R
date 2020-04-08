# https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")
#install.packages("scales")
#devtools::install_github("UI-Research/urbnthemes")
install.packages("remotes")
remotes::install_github("UrbanInstitute/urbnthemes", build_vignettes = TRUE)

library(tidyverse)
library(urbnmapr)
library(urbnthemes)

library(RColorBrewer)
display.brewer.all()

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

#countydata
#counties

household_data <- left_join(countydata, counties, by = "county_fips") 

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")

#----- Single state -----
# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n).
countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="Tennessee") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::percent,
#                       colors = brewer.pal(n = 8, name = "YlOrRd"),
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.25, "in")) +
  labs(fill = "Homeownership rate",
       title = "Title") +
  theme_urbn_map()

#----- US county-level map -----
household_data %>%
  ggplot(aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = NA) +
  scale_fill_gradientn(labels = scales::percent,
  #                     colors = brewer.pal(n = 8, name = "YlOrRd"),
                       guide = guide_colorbar(title.position = "top")) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urbn_map()
