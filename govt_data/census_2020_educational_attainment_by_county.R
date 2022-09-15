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
# S1901_C02_012 Estimate!!Families!!Median income (dollars)

#----- Census educational attainment data ---
edu_geo <- get_acs(geography = "county",
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

edu <- get_acs(geography = "county",
                   variables = c(  bachelor_up_25_up = "S1501_C01_015"  # bachelor or higher age 25 or higher
                                   , hs_up_25_up       = "S1501_C01_014"
                                   , pop_25_up         = "S1501_C01_006"  # population age 25 or higher
                                   , pct_bach_up       = "S1501_C02_015"
                   ), 
                   survey = "acs5",
                   year = 2020,
                   output = "wide",
                   cache_table = TRUE)

edu_state <- get_acs(geography = "state",
               variables = c(  bachelor_up_25_up = "S1501_C01_015"  # bachelor or higher age 25 or higher
                               , hs_up_25_up       = "S1501_C01_014"
                               , pop_25_up         = "S1501_C01_006"  # population age 25 or higher
                               , pct_bach_up       = "S1501_C02_015"
               ), 
               survey = "acs5",
               year = 2020,
               output = "wide",
               cache_table = TRUE)

#----- Census medina household income ---
hhincome <- get_acs(geography = "county",
                    variables = c(  med_hh_income = "S1901_C02_012"), 
                    survey = "acs5",
                    year = 2020,
                    output = "wide",
                    cache_table = TRUE)
hhincome_us <- get_acs(geography = "us",
                    variables = c(  med_hh_income = "S1901_C02_012"), 
                    survey = "acs5",
                    year = 2020,
                    output = "wide",
                    cache_table = TRUE)
#----- Counties
counties <- read_csv("https://pastebin.com/raw/swNsVEZA", skip = 7)

joined <- inner_join(edu, counties, by = c("GEOID" = "FIPS"))
joined_geo <- inner_join(edu_geo, counties, by = c("GEOID" = "FIPS"))

# quick look at rankings
joined %>% filter(state_abbrev == "TN") %>% select(pct = pct_bach_upE, county) %>% arrange(desc(pct))
joined %>% select(pct = pct_bach_upE, state_abbrev, county) %>% arrange(desc(pct)) %>% View()   # nat'l county rankings
edu_state %>% select(pct = pct_bach_upE, NAME) %>% arrange(desc(pct)) %>% View()  # state rankings


#----- map of educational attainment -----
joined_geo %>% 
  filter(state_abbrev %in% c("TN", "MS", "KY", "AL", "GA", "SC", "LA", "NC", "AR", "FL") )  %>%
  #filter(state_abbrev %in% c("AL") )  %>% 
  ggplot() +
  geom_sf(aes(fill = pct_bach_upE, geometry = geometry)) +
  #theme_void() +
  theme_minimal() +
  scale_fill_steps2(
    n.breaks = 7,
    low = scales::muted("red"),
    mid = "white",
    high = scales::muted("blue"),
    midpoint = 30,
    guide = "colorsteps"
  ) +
  labs(title = "Educational Attainment",
       subtitle = "% of population 25 and older with a Bachelor's degree or higher",
       fill = "% Bachelor's or higher",
       caption = "@bdill\nData: US Census Bureau ACS 5-yr 2020\nvariable: S1501_C02_015")  

