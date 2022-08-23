# Education attainment by county
library(tidyverse)
library(tidycensus)

#vars <- load_variables(2020, "acs5/subject", cache = TRUE)
#View(vars)

# S1501_C01_006 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over
# S1501_C01_014 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate or higher
# S1501_C01_015 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher
# S1501_C02_006 Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over
# S1501_C02_015 Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher

#----- Census educational attainment data ---
edu <- get_acs(geography = "county",
               #state = "WY",
               variables = c(  bachelor_up_25_up = "S1501_C01_015"  # bachelor or higher age 25 or higher
                             , hs_up_25_up       = "S1501_C01_014"
                             , pop_25_up         = "S1501_C01_006"  # population age 25 or higher
                             , pct_bach_up       = "S1501_C02_015"
               ), 
               survey = "acs5",
               year = 2020,
               output = "wide",
               geometry = TRUE,
               cache_table = TRUE)

#edu <- edu %>% mutate(pct_bach_up = bachelor_up_25_upE  * 100 / pop_25_upE)

counties <- read_csv("https://pastebin.com/raw/swNsVEZA", skip = 7)

joined <- inner_join(edu, counties, by = c("GEOID" = "FIPS"))

joined %>% 
  #filter(state_abbrev %in% c("TN", "MS", "KY", "AL", "GA", "SC", "LA", "NC") )  %>%
  filter(state_abbrev %in% c("TN") )  %>% 
  ggplot() +
  geom_sf(aes(fill = pct_bach_up_C02_015E, geometry = geometry)) +
  #theme_void() +
  theme_minimal() +
  scale_fill_steps2(
    n.breaks = 7,
    low = scales::muted("red"),
    mid = "white",
    high = scales::muted("blue"),
    midpoint = 50,
    guide = "colorsteps"
  ) +
  labs(title = "Educational Attainment",
       subtitle = "% of population 25 and older with a Bachelor's degree or higher",
       fill = "% Bachelor's or higher",
       caption = "@bdill\nData: US Census Bureau ACS 5-yr 2020\nvariable: S1501_C02_015")  
