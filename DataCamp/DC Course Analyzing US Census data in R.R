# https://campus.datacamp.com/courses/analyzing-us-census-data-in-r/census-data-in-r-with-tidycensus?ex=1
# 2022-05-29

library(tidycensus)
library(tigris)
library(tidyverse)

#----- 1 Census data in R with tidycensus  ----------------------------------------------------------------------
Sys.getenv("CENSUS_API_KEY")


state_pop <- get_decennial(geography = "state", variables = "P001001")  # Obtain and view state populations from the 2010 US Census
head(state_pop)

state_income <- get_acs(geography = "state", variables = "B19013_001")  # Obtain and view state median household income from the 2012-2016 American Community Survey
head(state_income)


travis_income2 <- get_acs(geography = "tract", 
                          variables = c(hhincome = "B19013_001"),  # Supply custom variable names
                          state = "TX",
                          county = "Travis")
head(travis_income2)


or_wide <- get_acs(geography = "county",   # Return county data in wide format
                   state = "OR",
                   variables = c(hhincome = "B19013_001", 
                                 medage = "B01002_001"), 
                   output = "wide")
head(or_wide)  # Compare output to the tidy format from previous exercises
plot(or_wide$hhincomeE, or_wide$medageE)  # Create a scatterplot

?load_variables
vars <- load_variables(year = 2016,
                      dataset = "acs5",
                      cache = TRUE)
filter(vars, str_detect(name, "B19001"))
filter(vars, str_detect(label, fixed("public transportation", ignore_case = TRUE)))


ne_income <- get_acs(geography = "state",
                 variables = "B19013_001", 
                 survey = "acs1", 
                 year = 2019,
                 state = c("ME", "NH", "VT", "MA", "RI", "CT", "NY"))
ggplot(ne_income, aes(x = estimate, y = NAME)) +
  geom_point()

ggplot(ne_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(color = "navy", size = 4) +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 18) +
  labs(x = "2016 ACS estimate",
       y = "",
       title = "Median household income by state")

#----- 2 Wrangling US Census Data  ----------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
race_vars = c(White = "B03002_003",
              Black = "B03002_004",
              Native = "B03002_005",
              Asian = "B03002_006",
              HIPI = "B03002_007",
              Hispanic = "B03002_012")
tx_race <- get_acs(geography = "county",
                   state = "TX",
                   variables = race_vars,
                   summary_var = "B03002_001")
tx_race_pct <- tx_race %>% 
  mutate(pct = 100 * (estimate / summary_est)) %>% 
  select(GEOID, NAME, variable, pct)

tx_largest <- tx_race %>%   # get largest estimate w/in each group
  group_by(GEOID) %>% 
  filter(estimate == max(estimate)) %>% 
  select(NAME, variable, estimate)
tx_largest %>% group_by(variable) %>% tally()

wa_income <- get_acs(geography = "county", # Download table "B19001"
                    state = "WA", 
                    table = "B19001")
head(wa_income)  # Check out the first few rows of wa_income

wa_grouped <- wa_income %>%
  filter(variable != "B19001_001") %>%
  mutate(incgroup = case_when(
    variable < "B19001_008" ~ "below35k", 
    variable < "B19001_013" ~ "35kto75k", 
    TRUE ~ "above75k"
  )) %>%
  group_by(NAME, incgroup) %>%
  summarize(group_est = sum(estimate))

wa_grouped
?map_df
tn_cities <- purrr::map_df(2010:2019, function(x) {     # map_df to apply the function over multiple years
  get_acs(geography = "place", 
          variables = c(totalpop = "B01003_001"),
          state = "TN",
          survey = "acs1", year = x) %>% 
    mutate(year = x)
})
tn_cities %>% arrange(NAME, year)

# looking at vars with high MOE
vt_eldpov <- get_acs(geography = "tract",   # Get data on elderly poverty by Census tract in Vermont
                     variables = c(eldpovm = "B17001_016", 
                                   eldpovf = "B17001_030"), 
                     state = "VT")
moe_check <- filter(vt_eldpov, moe > estimate)  # Identify rows with greater margins of error than their estimates
nrow(moe_check) / nrow(vt_eldpov)  # Check proportion of rows where the margin of error exceeds the estimate

# you can combine vars to reduce MOE 
# use moe_sum() moe_product(), moe_ratio(), moe_prop()
moe_sum(moe = c(55, 33, 44, 12, 4))  # Calculate a margin of error for a sum
moe_product(est1 = 55, est2 = 33, moe1 = 12, moe2 = 9) # Calculate a margin of error for a product
moe_ratio(num = 1000,  # Calculate a margin of error for a ratio
          denom = 950,
          moe_num = 200,
          moe_denom = 177)
moe_prop(num = 374,   # Calculate a margin of error for a proportion
         denom = 1200,
         moe_num = 122,
         moe_denom = 333)

# Group the dataset and calculate a derived margin of error
vt_eldpov2 <- vt_eldpov %>%
  group_by(GEOID) %>%
  summarize(
    estmf = sum(estimate), 
    moemf = moe_sum(moe = moe, estimate = estimate)
  )
moe_check2 <- filter(vt_eldpov2, moemf > estmf)  # Filter rows where newly-derived margin of error exceeds newly-derived estimate
nrow(moe_check2) / nrow(vt_eldpov2) # Check proportion of rows where margin of error exceeds estimate


# plotting with errorbars
maine_inc <- get_acs(geography = "county",   # Request median household income data
                    variables = c(hhincome = "B19013_001"), 
                    state = "ME") 
ggplot(maine_inc, aes(x = estimate, y = NAME)) +   # Generate horizontal error bars with dots
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point()


maine_inc2 <- maine_inc %>%  # Remove unnecessary content from the county's name
  mutate(NAME = str_replace(NAME, " County, Maine", ""))

# Build a margin of error plot incorporating your modifications
ggplot(maine_inc2, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "ACS estimate (bars represent margins of error)", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)

#----- 3 US Census geographic data in R  ----------------------------------------------------------------------
library(tigris)
co_counties <- counties(state = "CO")
plot(co_counties)

denver_tracts <- tracts(state = "CO", county = "Denver")
plot(denver_tracts)
#roads <- primary_secondary_roads(state = "NH")
#plot(roads)

lane_water <- area_water(state = "OR", county = "Lane")  # Plot area water features for Lane County, Oregon
plot(lane_water)

class(co_counties)  # By default, tigris returns objects of class Spatial*DataFrame from the sp package. (for me it was "sf"  "data.frame")
head(co_counties@data)  # Take a look at the information in the data 
co_counties@proj4string  # Check the coordinate system of the data




#----- 4 Mapping US Census Data  ----------------------------------------------------------------------




income <- get_acs(geography = "county",   # Request median household income data
                  variables = c(hhincome = "B19013_001"), 
                  survey = "acs5",
                  year = 2019) 
income %>% arrange(desc(estimate)) %>% select(estimate, moe, NAME) %>% head(20)




#options(scipen=0, digits=7)  #<<< default settings
options(scipen=999, digits=7)

tn_cities <- map_df(2010:2019, function(x) {
  get_acs(geography = "place", 
          variables = c(totalpop = "B01003_001"),
          state = "TN",
          survey = "acs1", year = x) %>% 
    mutate(year = x)
})
tn_cities$NAME <- str_replace(tn_cities$NAME, ", Tennessee", "")
tn_cities$NAME <- str_replace(tn_cities$NAME, fixed(" metropolitan government (balance)"), "")
tn_cities %>% filter(estimate > 70000) %>% 
  ggplot(aes(x = year, y = estimate, group = NAME, color = NAME)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks=2010:2019) +
  theme_light(base_size = 14) +
  labs(x = "Year", y = "Population",
       title = "Tennessee City Population over Time ",
       subtitle = "Cities with 70k + population",
       caption = "chart: @bdill\nData: US Census Bureau ACS 1 year (B01003_001)")


