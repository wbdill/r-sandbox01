# https://www.census.gov/data/academy/courses/choroplethr.html

install.packages("choroplethr")
install.packages("choroplethrMaps")

library(choroplethr)
library(choroplethrMaps)

data(df_pop_state)
df_pop_state

state_choropleth(df_pop_state)


# customize map with title and legend
state_choropleth(df_pop_state,
                 title  = "2012 State Population Estimates",
                 legend = "Population")

# 2 colors shows which states are above / below median value
state_choropleth(df_pop_state, num_colors=2)

# 1 color uses a continuous scale - useful for seeing outliers
state_choropleth(df_pop_state, num_colors = 1)

# use the "zoom" parameter to zoom in on certain states
# remember: all choroplethr requires lower-case state names
# combine multiple states using ?c
state_choropleth(df_pop_state, 
                 zoom = c("california", "oregon", "nevada"))

# combine choropleth map and reference map by setting the "reference_map"
# parameter to TRUE
state_choropleth(df_pop_state, 
                 num_colors = 1,
                 zoom = c("california", "oregon", "washington"),
                 reference_map = TRUE)

pop <- read_csv("https://pastebin.com/raw/jQYXHM8e", skip = 7)

# need 2 cols named "region" and "value" for mapping
pop2 <- pop %>% 
  mutate(region = as.numeric(FIPS), value = pop_2018) %>% 
  select(geo_id, FIPS, county, state, region, value)
?county_choropleth
county_choropleth(pop2, state_zoom = c("alabama", "mississippi"),
                  title = "County Population",
                  legend = "Population")
?df_state_demographics

df_2020 <- get_state_demographics(2020, span = 1)
?acs.fetch

