library(tidyverse)
library(scales)

# vote data: https://www.nytimes.com/interactive/2022/08/02/us/elections/results-kansas-abortion-amendment.html
vote <- read_csv("D:/opendata/ks_abortion_vote_2022-08-03.csv")
vote <- janitor::clean_names(vote)

#county <- read_csv("D:/opendata/census.gov/county_pop_totals_2010_2020/census_county_pop.csv")
county <- read_csv("https://pastebin.com/raw/R7VVd55L")
ks <- county %>% filter(state == "Kansas") %>% 
  select(fips_county, fips_county3, state, county, pop = pop_2020) %>% 
  mutate(county = str_replace(county, " County", ""))

join <- vote %>% inner_join(ks) %>% 
  mutate(turnout = total_votes * 100 / pop)

join %>% 
  ggplot(aes(x = pop, y = no)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  geom_smooth(method = lm) +
  labs(title = "KS Abortion Vote by County",
       x = "County Population (Log scale)",
       y = "Pct voting No",
       caption = "chart: @bdill\npopulation: US Census Bureau\nvote data: nytimes.com/interactive/2022/08/02/us/elections/results-kansas-abortion-amendment.html")

#----- linear model -----
mod <- lm(log(no) ~ pop, data = join)
summary(mod)


#--- No vote vs education attainment ---
#----- Census educational attainment data ---
edu <- get_acs(geography = "county",
               state = "KS",
               variables = c(bachelor_up_25_up = "S1501_C01_015",  # bachelor or higher age 25 or higher
                             hs_up_25_up       = "S1501_C01_014",
                             pop_25_up         = "S1501_C01_006"), # population age 25 or higher
               survey = "acs5",
               year = 2020,
               output = "wide",
               cache_table = TRUE)
edu <- edu %>% 
  mutate(pct_bach_up = bachelor_up_25_upE  * 100 / pop_25_upE,
         county = str_remove(NAME, " County, Kansas"))

vote_edu <- edu %>% 
  inner_join(vote, by = "county")

vote_edu %>% 
  ggplot(aes(x = pct_bach_up, y = no)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  geom_smooth(method = lm) +
  labs(title = "KS Abortion Vote by County",
       x = "% of county (age 25 or up) with a bachelor's degree or higher",
       y = "Pct voting No",
       caption = "chart: @bdill\nEducation: US Census Bureau ACS 5-year 2020\nvote data: nytimes.com/interactive/2022/08/02/us/elections/results-kansas-abortion-amendment.html")

mod <- lm(no ~ pct_bach_up, data = vote_edu)
summary(mod)
