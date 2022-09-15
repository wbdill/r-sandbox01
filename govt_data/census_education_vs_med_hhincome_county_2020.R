# Educational Attainment vs Median Household Income - ACS 5-year 2020
library(tidyverse)
library(tidycensus)

#----- State population -----
state_pop <- read_csv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/state_populations_2019.csv")

#----- Counties -----
counties <- read_csv("https://pastebin.com/raw/R7VVd55L", skip = 0)


#-- tidycensus API calls -----

# S1501_C01_006 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over
# S1501_C01_014 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate or higher
# S1501_C01_015 Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher
# S1501_C02_015 Estimate!!Percent!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree or higher

# S1901_C02_012 Estimate!!Families!!Median income (dollars)

# Census educational attainment
edu <- get_acs(geography = "county"
               , variables = c(  bachelor_up_25_up = "S1501_C01_015"  # bachelor or higher age 25 and up
                               , hs_up_25_up       = "S1501_C01_014"  # high school grad or higher age 25 and up
                               , pop_25_up         = "S1501_C01_006"  # population age 25 and up
                               , pct_bach_up       = "S1501_C02_015"  # % pop w/ bachelor's or higher
                              )              
               , survey = "acs5"
               , year = 2020
               , output = "wide"
               , cache_table = TRUE)

# Census median household income
hhincome <- get_acs(geography = "county"
                    , variables = c(  med_hh_income = "S1901_C02_012")
                    , survey = "acs5"
                    , year = 2020
                    , output = "wide"
                    , cache_table = TRUE)

#----- edu attainment vs median hh income table joins -----
edu_income <- inner_join(edu, hhincome) %>% 
  left_join(counties, by = c("GEOID" = "fips_county")) %>% 
  select(GEOID, fips_state, state_abbrev, state, county, NAME, pct_bach_up = pct_bach_upE, med_hh_income = med_hh_incomeE, pop = pop_2020) %>% 
  mutate( popK = (pop / 1000)) %>% 
  arrange(GEOID)

# save to CSV for Google sheet
write_csv(edu_income, "D:/R/output/by_state/edu_vs_hhincome/edu_vs_income_data.csv") 

#----- Graphing function -----
MapStateEAvsHHI <- function(x) {
  
  v_state_name <- x$state
  v_state_abbrev <- x$state_abbrev

  edu_income %>% 
    filter(state_abbrev == v_state_abbrev) %>% 
    ggplot(aes(x = pct_bach_up, y = med_hh_income)) +
    geom_point(aes(size = popK), alpha = 0.3) +
    geom_smooth(method = "lm") +
    geom_vline(xintercept = 32.9, color = "#ff9999", size = 1.2, alpha = .5) +
    geom_hline(yintercept = 80069, color = "#77bb33", size = 1.2, alpha = .5) +    
    scale_y_continuous(labels = comma) +
    theme_bw() +
    labs(title = paste0("Education vs Income by County: ", v_state_name),
         subtitle = "% of county with bachelor's or higher vs median houshold income",
         x = "% of county with bachelor's degree or higher",
         y = "Median Household Income",
         size = "pop (1000)",
         caption = "@bdill\nUS Census Bureau ACS 5-year 2020\nvars: S1501_C02_015, S1901_C02_012")
  
  filename <- paste0("D:/R/output/by_state/edu_vs_hhincome/edu_vs_income_", v_state_abbrev, ".png")
  ggsave(filename = filename, width = 6, height = 4, units = "cm", dpi = 200, scale = 3)
}

by(state_pop, 1:nrow(state_pop), MapStateEAvsHHI)


#----- single graph -----
v_state_name = "MI"
edu_income %>% 
  filter(state_abbrev == v_state_name) %>% 
  ggplot(aes(x = pct_bach_up, y = med_hh_income)) +
  geom_point(aes(size = popK), alpha = 0.3) +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 32.9, color = "#aa00aa", size = 1.2, alpha = .3) +
  geom_hline(yintercept = 80069, color = "#77bb33", size = 1.2, alpha = .5) +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(title = paste0("Education vs Income by County: ", v_state_name),
       subtitle = "% of county with bachelor's or higher vs median houshold income",
       x = "% of county with bachelor's degree or higher",
       y = "Median Household Income",
       size = "pop (1000)",
       caption = "@bdill\nUS Census Bureau ACS 5-year 2020\nvars: S1501_C02_015, S1901_C02_012")


