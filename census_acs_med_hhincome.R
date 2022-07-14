library(tidycensus)
library(tidyverse)

# 1) get your own free census API key: https://api.census.gov/data/key_signup.html
#census_api_key("your_key_here", install = TRUE)  # 2) install it to your R environment
# then you can run tidycensus functions like get_acs()
# https://walker-data.com/tidycensus/reference/index.html
hhincome <- get_acs(geography = "county",   
                  variables = c(hhincome = "B19013_001"),   # Request median household income data
                  survey = "acs5",
                  year = 2019,
                  cache = T) 

county_fips <- fips_codes %>% 
  mutate(GEOID = paste0(state_code, county_code),
         county_clean = str_replace_all(county, c(" County"="", " Parish"="", " Borough" = "", " Census Area"="", " Municipio"="")) ) %>% 
  rename(state_abbrev = state, state = state_name, state_fips = state_code, county_fips = county_code) %>% 
  select(GEOID, state_fips, state_abbrev, state, county_fips, county, county_clean)
#head(county_fips)

hhincome2 <- hhincome  %>% inner_join(county_fips)
#head(hhincome2)

MapStateIncome <- function(x) {
  state_name <- x$state
  state_abbrev <- x$state_abbrev
  
  gtitle = paste("Median Household Income - ", state_name)
  hhincome2 %>%
    filter(state == state_name) %>%
    ggplot(aes(x = estimate, y = reorder(county_clean, estimate))) + 
    geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
    geom_point(size = 1.5, color = "darkgreen") + 
    theme_minimal(base_size = 5) + 
    labs(title = "Median household income (US Census 2019)", 
         subtitle = paste0("Counties in ", state_name), 
         x = "ACS estimate (bars represent margins of error)", 
         y = "",
         caption = "chart: @bdill\ndata: US Census Burea ACS 5-year 2019 (B19013_001)") + 
    scale_x_continuous(labels = scales::dollar)
  
  filename <- paste0("D:/tmp/R/output/med_hhincome/med_hhincome_", state_abbrev, ".png")
  ggsave(filename = filename, width = 20, height = 18, units = "cm")
}

#states_subset <- states %>% filter(state_abbrev %in% c("TN", "GA", "TX", "CA", "MT"))
#by(states_subset, 1:nrow(states_subset), MapStateIncome)

states <- county_fips %>% distinct(state_fips, state_abbrev, state) %>% filter(state_fips <= 56)
by(states, 1:nrow(states), MapStateIncome)



# Build a margin of error plot

hhincome2 %>% filter(state_abbrev == "TN") %>% 
  ggplot(aes(x = estimate, y = reorder(county_clean, estimate))) +
  #ggplot(aes(x = estimate, y = county_clean)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 2, color = "darkgreen") + 
  theme_minimal(base_size = 8) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Tennessee", 
       x = "ACS estimate (bars represent margins of error)", 
       y = "",
       caption = "chart: @bdill\ndata: US Census Burea ACS 5-year 2019 (B19013_001)") + 
  scale_x_continuous(labels = scales::dollar)
