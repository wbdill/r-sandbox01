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
