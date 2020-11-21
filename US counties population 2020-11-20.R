

library(tidyverse)
rm(list = ls())
setwd("D:/opendata/county_demographics")

pop <- read_csv("cc-est2019-alldata.csv")  # 170MB
str(pop)
head(pop)


# https://nces.ed.gov/ipeds/report-your-data/race-ethnicity-definitions
# Hispanic or Latino
# Not Hispanic or Latino

# American Indian or Alaska Native
# Asian
# Black or African American
# Native Hawaiian or Other Pacific Islander
# White


# YEAR 1 = 2010 census, YEAR > 3 = 2011-2019 estimates
pop_simp <- pop %>% filter(AGEGRP == 0, (YEAR > 3 | YEAR == 1)) %>% 
  mutate(FIPS = paste0(STATE, COUNTY),
         STFIPS = STATE,
         COFIPS = COUNTY,
         state = STNAME,
         county = str_remove(CTYNAME, " County"), 
         year = case_when(YEAR == 1 ~ 2010, TRUE ~ YEAR + 2007), 
         pop = TOT_POP,
         white_pop = WA_MALE + WA_FEMALE,
         black_pop = BA_MALE + BA_FEMALE,
         indian_pop = IA_MALE + IA_FEMALE,
         asian_pop = AA_MALE + AA_FEMALE,
         pacific_pop = NA_MALE + NA_FEMALE,
         two_pop = TOM_MALE + TOM_FEMALE,
         not_hisp_pop = NH_MALE + NH_FEMALE,
         hisp_pop = H_MALE + H_FEMALE) %>% 
  select(FIPS, STFIPS, COFIPS, state, county, year, pop, white_pop, black_pop, indian_pop, pacific_pop, two_pop, not_hisp_pop, hisp_pop)

write_csv(pop_simp, "county_census_est_race_eth_2010_2019.csv")


tn_pop_delta <- pop_simp %>% 
  filter(STFIPS == 47) %>% 
  filter(year %in% c(2010, 2019)) %>% 
  select(FIPS, state, county, year, pop) %>% 
  pivot_wider(names_from = year, values_from = pop, names_prefix = "pop_") %>% 
  mutate(delta = pop_2019 - pop_2010,
         delta_pct = round(delta * 100 / pop_2010, 2) ) %>% 
  arrange(desc(delta_pct))

write_csv(tn_pop_delta, "TN_county_pop_delta_2010_2019.csv")
# saved to https://docs.google.com/spreadsheets/d/1ivOzLBZJzAgRmX8wZn9yXF7UETPCc8d8GZMFxENRqhw/edit#gid=0

tn_pop_delta %>% 
  pivot_longer(cols = c("pop_2010", "pop_2019"), names_prefix = "pop_", names_to = "year", values_to = "pop") %>% 
  arrange(desc(delta_pct), FIPS) %>% 
  head(10) %>% 
  ggplot(aes(year, pop, group = county)) +
  geom_line(aes(color = county), size = 1) +
  labs(title = "TN County Population Change",
       subtitle = "Top 5 Growth",
       x = "Year",
       y = "Population",
       caption = "@bdill  Data: US Census cc-est2019-alldata.csv")
ggsave("TN_county_growth_2010s_top.png", width = 7, height = 5, dpi = 150)

tn_pop_delta %>% 
  pivot_longer(cols = c("pop_2010", "pop_2019"), names_prefix = "pop_", names_to = "year", values_to = "pop") %>% 
  arrange(desc(delta_pct), FIPS) %>% 
  tail(10) %>% 
  ggplot(aes(year, pop, group = county)) +
  geom_line(aes(color = county), size = 1) +
  labs(title = "TN County Population Change",
       subtitle = "Bottom 5 Growth",
       x = "Year",
       y = "Population",
       caption = "@bdill  Data: US Census cc-est2019-alldata.csv")
ggsave("TN_county_growth_2010s_bottom.png", width = 7, height = 5, dpi = 150)

hist(tn_pop_delta$delta_pct)

?pivot_longer
vignette("pivot")
?round

  ggplot(aes(year, pop, group = county)) +
  geom_line(aes(color = county)) +
  scale_y_log10()

  ?pivot_wider
  