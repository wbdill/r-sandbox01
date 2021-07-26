library(tidyverse)
library(scales)
library(RColorBrewer)
rm(list = ls())


state_abbrev <- data.frame(
  stringsAsFactors = FALSE,
  state = c("Alabama", "Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
            "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
            "Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
            "New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma",
            "Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
            "Virginia","Washington","West Virginia","Wisconsin",
            "Wyoming"),
  state_abbrev = c("AL", "AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS",
                   "KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                   "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
)
#----- 2020 simple population estimates by county -----
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv # 4MB
pop_raw <- read_csv("D:/opendata/census.gov/county_pop_totals_2010_2020/co-est2020-alldata.csv")  
pop_clean <- pop_raw %>% filter(SUMLEV == "050") %>% 
  mutate(fips_county = paste0(STATE,COUNTY)) %>% 
  full_join(state_abbrev, by = c("STNAME" = "state")) %>% 
  select(region = REGION, division = DIVISION, fips_state = STATE, fips_county, fips_county3 = COUNTY, state = STNAME, state_abbrev, county = CTYNAME, 
         pop_2010_census = CENSUS2010POP, pop_2010 = POPESTIMATE2010, pop_2011 = POPESTIMATE2011, pop_2012 = POPESTIMATE2012,
         pop_2013 = POPESTIMATE2013, pop_2014 = POPESTIMATE2014, pop_2015 = POPESTIMATE2015, pop_2016 =POPESTIMATE2016, 
         pop_2017 = POPESTIMATE2017, pop_2018 = POPESTIMATE2018, pop_2019 = POPESTIMATE2019, pop_2020 = POPESTIMATE2020)

write_csv(pop_clean, "D:/opendata/census.gov/county_pop_totals_2010_2020/census_county_pop.csv")

#----- 2019 full demographics -----
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv

pop_raw <- read_csv("D:/opendata/county_demographics/cc-est2019-alldata.csv")  # 170MB
str(pop_raw)
head(pop_raw)

# https://nces.ed.gov/ipeds/report-your-data/race-ethnicity-definitions
# Hispanic or Latino
# Not Hispanic or Latino

# American Indian or Alaska Native
# Asian
# Black or African American
# Native Hawaiian or Other Pacific Islander
# White



# YEAR 1 = 2010 census, YEAR > 3 = 2011-2019 estimates
pop_race_simp <- pop_raw %>% filter(AGEGRP == 0, (YEAR > 3 | YEAR == 1)) %>% 
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
  inner_join(state_abbrev, by = "state") %>% 
  select(FIPS, STFIPS, COFIPS, state_abbrev, state, county, year, pop, white_pop, black_pop, asian_pop, indian_pop, pacific_pop, two_pop, not_hisp_pop, hisp_pop)

write_csv(pop_race_simp, "US_county_census_est_race_eth_2010_2019.csv")


#----- race proportion for every county in the state -----
PlotRaceProportion <- function(x) {
  state_name <- x$state
  state_abbrev <- x$state_abbrev
  gtitle = paste("County Race Proportions - ", state_name)
  
  pop_race_simp %>% 
    filter(state == state_name, year == 2019) %>%
    pivot_longer(cols = ends_with("_pop"), names_to = "race", values_to = "population") %>% 
    mutate(race = str_remove(race, "_pop")) %>% 
    filter(!race %in% c("hisp", "not_hisp")) %>% 
    ggplot(aes(county, population, group = "race")) +
    geom_col(aes(fill = race), position = "fill") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = brewer.pal(n = 6, name = "RdYlBu")) +
    labs(title = gtitle,
         subtitle = "2019 Census Estimate",
         caption = "@bdill Data: US Census cc-est2019-alldata.csv")
  
  filename <- paste0("output/census/county_race_proportion/county_race_proportion_", state_abbrev, ".png")
  ggsave(filename = filename, width = 9, height = 6, dpi = 250)  
}
by(state_abbrev, 1:nrow(state_abbrev), PlotRaceProportion)
?by




#----- TN counties 2010 vs 2019 pop change
tn_pop_delta <- pop_race_simp %>% 
  filter(STFIPS == 47) %>% 
  filter(year %in% c(2010, 2019)) %>% 
  select(FIPS, state, county, year, pop) %>% 
  pivot_wider(names_from = year, values_from = pop, names_prefix = "pop_") %>% 
  mutate(delta = pop_2019 - pop_2010,
         delta_pct = round(delta * 100 / pop_2010, 2) ) %>% 
  arrange(desc(delta_pct))

write_csv(tn_pop_delta, "TN_county_pop_delta_2010_2019.csv")
# saved to https://docs.google.com/spreadsheets/d/1ivOzLBZJzAgRmX8wZn9yXF7UETPCc8d8GZMFxENRqhw/edit#gid=0

#----- Top 5 growth TN counties -----
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

#----- Bottom 5 growth TN counties -----
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


#----- TN counties hispanic
pop_race_simp %>% 
  filter(STFIPS == 47, year == 2019) %>% 
  mutate(hisp_pct = hisp_pop * 100 / pop,
         black_pct = black_pop * 100 / pop,
         asian_pct = asian_pop * 100 / pop) %>% 
  arrange(desc(asian_pct)) %>% 
  select(FIPS, state, county, pop, black_pop, black_pct, asian_pop, asian_pct, hisp_pop, hisp_pct) %>% 
  ggplot(aes(reorder(county, desc(hisp_pct)), hisp_pct)) +
  geom_col()

#----- Counties per state
pop_race_simp %>% 
  filter(year == 2019) %>% 
  count(STFIPS, state) %>% 
  arrange(desc(n)) %>% 
  View()




