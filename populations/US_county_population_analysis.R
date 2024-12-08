# Brian Dill @bdill
# 2020-03-28
# County populations: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
rm(list = ls())
library(tidyverse)

#----- US counties and populations -----

#counties_pop <- read_csv("covid19/data/PEP_2018_PEPANNRES.csv", col_types = "ccciiiiiiiiiii")  # force col2 to be string to prevent stripping of leading zero
counties_pop <- read_csv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/PEP_2018_PEPANNRES.csv")
names(counties_pop) <- c("geo_id", "fips_code", "county", "census_2010", "est_base_2010", "pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", "pop_2015", "pop_2016", "pop_2017", "pop_2018" )
counties_pop <- separate(counties_pop, county, c("county", "state"), sep = ", ")
counties_pop$county <- str_replace(counties_pop$county, " County", "")

#write_csv(counties_pop, "data/counties_pop_census_2018.csv")

state_counties_count <- counties_pop %>%
  group_by(state) %>%
  count() %>%
  rename(num_counties = n)

#----- counties with > 5% growth or decline b/t 2010 - 2018 (and 2010 at least 1000 people)
counties_pop %>%
  mutate(pct_chg = round((pop_2018 - pop_2010) / pop_2010 * 100, digits = 2)) %>%
  select(county, state, pop_2010, pop_2018, pct_chg) %>%
  filter(abs(pct_chg) > 5 & pop_2010 > 1000) %>%
  arrange(desc(pct_chg)) %>%
  write_csv("data/counties_with_gt_5pct_chg_2010_2018.csv")

#----- count of counties with 5% up/down growth by state -----
counties_net_updown <- counties_pop %>%
  mutate(pct_chg = round((pop_2018 - pop_2010) / pop_2010 * 100, digits = 2),
         direction = case_when(pct_chg > 0 ~ "counties_up", 
                               pct_chg < 0 ~ "counties_down")) %>%
  filter(abs(pct_chg) > 5 & pop_2010 > 1000) %>%
  select(state, direction) %>%
  group_by(state, direction) %>%
  count() %>%
  spread(key = direction, value = n) %>%
  replace_na(list(counties_up = 0, counties_down = 0)) %>%  #https://tidyr.tidyverse.org/reference/replace_na.html
  mutate(counties_net = counties_up - counties_down) %>%
  arrange(desc(counties_net)) %>%
  ungroup()

counties_net_updown %>%
  top_n(7, counties_net) %>%
  arrange(desc(counties_net)) %>%
  gather(key = measure, value = count, -state) %>%
  mutate(measure = fct_relevel(measure, "counties_down", "counties_up", "counties_net")) %>%
  ggplot(aes(reorder(state, -count), count, group = measure, fill = measure)) +
  geom_col(position = "dodge", color = "#000000") +
  labs(title = "States with most net county increases 2010-2018",
       subtitle = "Counties with: > 5% change and > 1000 population",
       x = "",
       y = "Counties")


#----- TN Counties -----
counties_pop %>%
  filter(state == "Tennessee") %>%
  mutate(pct_chg = round((pop_2018 - pop_2010) / pop_2010 * 100, digits = 2)) %>%
  select(county, state, pop_2010, pop_2018, pct_chg) %>%
  arrange(desc(pct_chg)) %>%
  write_csv("output/tn_counties.csv")
