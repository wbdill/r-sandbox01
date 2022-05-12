
library(tidyverse)
library(tidycensus)

# 1) get your own free census API key: https://api.census.gov/data/key_signup.html
#census_api_key("your_key_here", install = TRUE)  # 2) install it to your R environment
# then you can run tidycensus functions like get_acs()
# https://walker-data.com/tidycensus/reference/index.html

county_race_nogeo <- get_decennial(
  geography = "county",
  variables = c(
    Hispanic = "P2_002N",
    White    = "P2_005N",
    Black    = "P2_006N",
    Native   = "P2_007N",
    Asian    = "P2_008N",
    NHOPI    = "P2_009N",
    Other    = "P2_010N",
    TwoOrMore = "P2_011N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = FALSE
) %>% 
  mutate(percent = 100 * (value / summary_value)) %>% arrange(GEOID, variable)

fips <- tidycensus::fips_codes %>% mutate(GEOID = paste0(state_code, county_code))

county_race_nogeo2 <- county_race_nogeo %>% 
  select(GEOID, NAME, variable, total_pop = summary_value, value) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  inner_join(fips) %>% 
  select(GEOID, fips_state = state_code, fips_county = county_code, state, state_name, county, total_pop, 
         white_pop = White, black_pop = Black, asian_pop = Asian,
         native_pop = Native, nhopi_pop = NHOPI, hispanic_pop = Hispanic, other_pop = Other
         , twoormore_pop = TwoOrMore, NAME) %>% 
  mutate(pct_white = round(100 * white_pop / total_pop, digits = 4),
         pct_black = round(100 * black_pop / total_pop, digits = 4),
         pct_asian = round(100 * asian_pop / total_pop, digits = 4),
         pct_native = round(100 * native_pop / total_pop, digits = 4),
         pct_nhopi = round(100 * nhopi_pop / total_pop, digits = 4),
         pct_hispanic = round(100 * hispanic_pop / total_pop, digits = 4),
         pct_other = round(100 * other_pop / total_pop, digits = 4),
         pct_twoormore = round(100 * twoormore_pop / total_pop, digits = 4)
         ) %>% 
  arrange(GEOID)

county_race_nogeo2 %>% filter(state == "MS") %>% 
  select(county, total_pop, pct_white, pct_black, pct_asian, pct_hispanic, pct_twoormore) %>% 
  arrange(desc(pct_asian)) %>% 
  View()
write_csv(county_race_nogeo2, "D:/opendata/census2020_decennial_race_county.csv")


county_race_nogeo2 %>% filter(state == "MT") %>% 
  select(state, county, total_pop, white_pop, black_pop, pct_white, pct_black) %>% 
  arrange(desc(total_pop)) %>% 
  ggplot(aes(x = reorder(county, desc(total_pop)), y = total_pop)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "County", y = "Total Population")

county_race_nogeo2 %>% filter(state == "TN") %>% count()  # 95 counties
top10pop <- county_race_nogeo2 %>% filter(state == "TN") %>% 
  select(state, county, total_pop, white_pop, black_pop, pct_white, pct_black) %>% 
  top_n(10, total_pop) %>% 
  summarize(top10 = sum(total_pop))

bottom85pop <- county_race_nogeo2 %>% filter(state == "TN") %>% 
  select(state, county, total_pop, white_pop, black_pop, pct_white, pct_black) %>% 
  top_n(85, desc(total_pop)) %>% 
  summarize(top10 = sum(total_pop))
