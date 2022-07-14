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
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
getOption("tigris_use_cache")
options(sf_max.plot = 9)  # default is 9
#tigris_cache_dir("D:/tmp")

az_sf <- counties("AZ", cb = TRUE)  # sf can speed up load times significantly
class(az_sf)
st_geometry(az_sf)
plot(az_sf)

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


mi_tiger <- counties("MI")  # Get a counties dataset for Michigan
mi_cb <- counties("MI", cb = TRUE)  # Get the equivalent cartographic boundary shapefile
plot(mi_tiger$geometry)
plot(mi_cb$geometry, add = TRUE, border = "red")  # Overlay the two on a plot to make a comparison


colorado_sf <- counties("CO")
head(colorado_sf)
plot(colorado_sf$geometry)



williamson90 <- tracts(state = "TX", county = "Williamson",  # Get a historic Census tract shapefile from 1990 for Williamson County, Texas
                       cb = TRUE, year = 1990)
williamson16 <- tracts(state = "TX", county = "Williamson",  # Compare with a current dataset for 2016
                    cb = TRUE, year = 2016)
par(mfrow = c(1, 2))


plot(williamson90$geometry) # Plot the geometry to compare the results
plot(williamson16$geometry)
dev.off() # to reset par

tn_house <- state_legislative_districts(state = "TN", house ="lower", cb = TRUE)
plot(tn_house$geometry)


or_tracts <- tracts("OR", cb = TRUE) # Get Census tract boundaries for Oregon and Washington
wa_tracts <- tracts("WA", cb = TRUE)

attr(or_tracts, "tigris")# Check the tigris attributes of each object
attr(wa_tracts, "tigris")

or_wa_tracts <- rbind_tigris(or_tracts, wa_tracts)  # Combine the datasets then plot the result
plot(or_wa_tracts$geometry)


# get tracts for multiple states with map() and rbind them
new_england <- c("ME", "NH", "VT", "MA")  # Generate a vector of state codes and assign to new_england
ne_tracts <- map(new_england, function(x) {  # Iterate through the states and request tract data for state
  tracts(state = x, cb = TRUE)
}) %>%
  rbind_tigris()
plot(ne_tracts$geometry)

tx_members <- read_csv("D:/tmp/R/tx_house_members.csv")
tx_members$District <- as.character(tx_members$District)
tx_house <- state_legislative_districts(state = "TX", house = "lower", cb = TRUE) # Get boundaries for Texas and set the house parameter
tx_joined <- left_join(tx_house, tx_members, by = c("NAME" = "District")) # Merge data on legislators to their corresponding boundaries
head(tx_joined)

ggplot(tx_joined) +   # Plot the legislative district boundaries
  geom_sf()

ggplot(tx_joined, aes(fill = Party)) +   # Set fill aesthetic to map areas represented by Republicans and Democrats
  geom_sf()


ggplot(tx_joined, aes(fill = Party)) +   # Set values so that Republican areas are red and Democratic areas are blue
  geom_sf() + 
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue"))


ggplot(tx_joined, aes(fill = Party)) +   # Draw a ggplot without gridlines and with an informative title
  geom_sf() + 
  coord_sf(crs = 3083, datum = NA) +  # datum = NA removes gridlines
  scale_fill_manual(values = c("Republican" = "#ff9999", "Democratic" = "#9999ff")) + 
  theme_minimal(base_size = 16) + 
  labs(title = "State House Districts in Texas")

#----- 4 Mapping US Census Data  ----------------------------------------------------------------------
library(tigris)
library(tidycensus)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# use Tigris when geometries aren't avail in tidycensus (state, county, tract, block group, block, zcta)
var_state <- "MT"
sd_income_unif <- tidycensus::get_acs(state = var_state, geography = "school district (unified)", variables = "B19013_001") #median hh income
sd_income_elem <- tidycensus::get_acs(state = var_state, geography = "school district (elementary)", variables = "B19013_001") #median hh income
sd_income_all <- rbind(sd_income_unif, sd_income_elem)

sd_geo_unif <- tigris::school_districts(state = var_state, type = "unified", class="sf")
sd_geo_elem <- tigris::school_districts(state = var_state, type = "elementary", class="sf")
names(sd_geo_elem) <- names(sd_geo_unif)  # make the names match so we can rbind them
sd_geo_all <- rbind(sd_geo_unif, sd_geo_elem)

sd_joined <- dplyr::left_join(sd_geo_all, sd_income_all, by = "GEOID")

ggplot(sd_joined, aes(fill = estimate)) +
  geom_sf() +
  coord_sf(datum = NA) +  # crs = 3083, 
  theme_minimal(base_size = 16) +
  scale_fill_distiller(palette = "Greens", direction = 1, labels = scales::dollar) +
  #scale_color_distiller(palette = "Blues", direction = 1, guide = FALSE) +
  #scale_fill_viridis_c(labels = scales::dollar) +
  #scale_color_viridis_c(guide = FALSE) +
  labs(title = paste0("Median Household Income by School District: ",var_state),
       subtitle = "American Community Survey 2020",
       fill = "ACS Estimate",
       caption = "data: US Census Burea ACS 5-year 2020 (B19013_001)")



orange_value <- get_acs(geography = "tract", state = "CA",  # Get dataset with geometry set to TRUE
                        county = "Orange", 
                        variables = "B25077_001", 
                        geometry = TRUE)
plot(orange_value["estimate"])  # Plot the estimate to view a map of the data


# using viridis for chloropleths
idaho_income <- get_acs(geography = "school district (unified)",   # Get an income dataset for Idaho by school district
                        variables = "B19013_001", 
                        state = "ID")
idaho_school <- school_districts(state = "ID", type = "unified", class = "sf")  # Get a school district dataset for Idaho
id_school_joined <- left_join(idaho_school, idaho_income, by = "GEOID") # Join the income dataset to the boundaries dataset
plot(id_school_joined["estimate"])
ggplot(id_school_joined, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()




# graduated symbol maps
library(sf)
state_value <- get_acs(geography = "state", variables = "B25077_001", geometry = TRUE)
state_value <- tigris::shift_geometry(state_value)
centers <- st_centroid(state_value)
# Set size parameter and the size range
ggplot() + 
  geom_sf(data = state_value, fill = "white") + 
  geom_sf(data = centers, aes(fill  = estimate), shape = 21, 
      fill = "lightblue", alpha = 0.7, show.legend = "point") + 
  scale_size_continuous(range = c(1, 20))

# faceted maps
race_vars = c(White = "B03002_003",
              Black = "B03002_004",
              #Native = "B03002_005",
              Asian = "B03002_006",
              #HIPI = "B03002_007",
              Hispanic = "B03002_012")
dc_race <- get_acs(geography = "tract",
                   state = "DC",
                   variables = race_vars,
                   summary_var = "B03002_001",
                   geometry = TRUE) %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>% 
  rename(value = estimate, summary_value = summary_est)

ggplot(dc_race, aes(fill = percent, color = percent)) + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  facet_wrap(~variable)

# Mapview
install.packages("mapview")
library(mapview)

orange_value <- get_acs(geography = "tract", state = "CA",  # Get dataset with geometry set to TRUE
                        county = "Orange", 
                        variables = "B25077_001", 
                        geometry = TRUE)
m <- mapview(orange_value)
m@map

m <- mapview(orange_value, zcol = "estimate", legend = TRUE)
m@map

# dot density
library(RColorBrewer)
dc_roads <- roads("DC", "District of Columbia") %>% 
  filter(RTTYP %in% c("I", "S", "U"))
dc_water <- area_water("DC", "District of Columbia")
dc_boundary <- counties("DC", cb = TRUE)
#plot(dc_water$geometry, col = "lightblue")

# Generate dots, create a group column, and group by group column
dc_dots <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  dc_race %>%
    filter(variable == group) %>%
    st_sample(., size = .$value / 100) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()


ggplot() +
  geom_sf(data = dc_boundary, color = NA, fill = "white") +
  geom_sf(data = dc_dots, aes(color = group, fill = group), size = 0.1) +
  geom_sf(data = dc_water, color = "lightblue", fill = "lightblue") +
  geom_sf(data = dc_roads, color = "gray") +
  coord_sf(crs = 26918, datum = NA) +
  scale_color_brewer(palette = "Set1", guide = FALSE) + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC", 
       subtitle = "2010 decennial U.S. Census", 
       fill = "", 
       camption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")

ggplot() + 
  geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = dc_dots, aes(color = group, fill = group), size = 0.1) + 
  geom_sf(data = dc_water, color = "lightblue", fill = "lightblue") + 
  geom_sf(data = dc_roads, color = "grey") + 
  coord_sf(crs = 26918, datum = NA) + 
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC", 
       subtitle = "2010 decennial U.S. Census", 
       fill = "", 
       camption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")

#----- Extra stuff -------
#race by county
race_vars = c(White = "B03002_003",
              Black = "B03002_004",
              Native = "B03002_005",
              Asian = "B03002_006",
              Pacific_Islander = "B03002_007",
              Hispanic = "B03002_012")
acs_race <- get_acs(geography = "county",
                   state = "MT",
                   variables = race_vars,
                   summary_var = "B03002_001",
                   geometry = TRUE,
                   cache_table = TRUE) %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>% 
  rename(value = estimate, summary_value = summary_est)

ggplot(acs_race, aes(fill = percent)) + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  scale_fill_viridis_c() +
  scale_color_grey(guide = FALSE) +
  #scale_color_viridis_c(guide = FALSE) +
  theme_minimal(base_size = 16) +
  facet_wrap(~variable) +
  labs(title = "Percent Race by County",
       subtitle = "US Census Burea - American Community Survey 2020",
       fill = "ACS est. %",
       caption = "chart: @bdill\ndata: US Census Burea ACS 5-yr 2020 (B03002)")



library(tidyverse)
library(tidycensus)
library(RColorBrewer)

pep_vars <- load_variables(2020, )
county_pep <- get_estimates(geography = "county", variables = "POP", year = 2019, geometry = TRUE)
county_pep_shift <- tigris::shift_geometry(county_pep)

#write_csv(county_pep_shift, "D:/opendata/county_pep_shift.csv")
#write_rds(county_pep_shift, "D:/opendata/county_pep_shift.rds")

fips <- fips_codes %>% 
  mutate(GEOID = paste0(state_code, county_code)) %>% 
  select(GEOID, state_code, state_abbrev = state, state = state_name, county_code, county)

county_pop <- county_pep %>% full_join(fips)

county_pop %>% filter(state_abbrev == "TN") %>% 
ggplot(aes(fill = value)) +
  geom_sf()
?tigris
?shift_geometry

# https://nces.ed.gov/ccd/districtsearch/district_list.asp?Search=1&details=1&InstName=&DistrictID=&Address=&City=&State=47&Zip=&Miles=&County=&PhoneAreaCode=&Phone=&DistrictType=1&DistrictType=2&DistrictType=3&DistrictType=4&DistrictType=5&DistrictType=6&DistrictType=7&DistrictType=8&NumOfStudents=&NumOfStudentsRange=more&NumOfSchools=&NumOfSchoolsRange=more
tn_nces <- read_csv("D:/opendata/NCES/nces_tn_public_school_districts.csv")
tn_nces <- janitor::clean_names(tn_nces)
tn_nces$nces_district_id <- as.character(tn_nces$nces_district_id) #make it a char so it can be joined to GEOID
tn_nces$student_teacher_ratio <- as.numeric(tn_nces$student_teacher_ratio)

sd_geo_unif <- tigris::school_districts(state = "TN", type = "unified", class="sf")
sd_geo_elem <- tigris::school_districts(state = "TN", type = "elementary", class="sf")
names(sd_geo_elem) <- names(sd_geo_unif)  # make the names match so we can rbind them
sd_geo_all <- rbind(sd_geo_unif, sd_geo_elem)

sd_joined <- dplyr::left_join(sd_geo_all, tn_nces, by = c("GEOID" = "nces_district_id"))

#tn_unsd_shp <- sf::st_read("D:/opendata/TIGER/tl_2020_47_unsd.shp")
#RColorBrewer::display.brewer.all()
#RColorBrewer::brewer.pal(5,"Greens")

ggplot(sd_joined, aes(fill = student_teacher_ratio)) +
  geom_sf() +
  coord_sf(datum = NA) +  # crs = 3083, 
  theme_minimal(base_size = 16) +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  #scale_fill_stepsn(colors = RColorBrewer::brewer.pal(5,"Greens"))
  labs(title = "Student Teacher Ratio by School District: TN",
       fill = "NCES Student Teacher Ratio",
       caption = "chart: @bdill\ndata: nces.ed.gov\nshapefiles: US Census Bureau")

tn_nces %>% filter(type == "Regular School District") %>% 
ggplot(aes(x = reorder(district_name, student_teacher_ratio), y = student_teacher_ratio)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90) ) +
  labs(title = "Student Teacher Ratio by School District: TN",
       x = "",
       y = "Student / Teacher Ratio",
       caption = "chart: @bdill\ndata: nces.ed.gov")
         

library(tidycensus)
sch <- get_school_districts(state = "TN")
sch <- get_school_districts(state = "TN", cache = T)
head(sch)
??get_school_districts
write_csv(sch, "D:/opendata/TN_school_dist.csv")

plot(sch, max.plot = 2)
plot(sch, max.plot = 1, datum = NULL)
?coord_sf




vars <- load_variables(2019, "acs1", cache = T)
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

#----- TX House members tibble -----
tx_members <- tibble::tribble(
  ~District,          ~Name,       ~Party,        ~City,
         1,         "Gary VanDeaver", "Republican",      "New Boston",
         2,           "Bryan Slaton", "Republican",      "Royse City",
         3,         "Cecil Bell Jr.", "Republican",        "Magnolia",
         4,             "Keith Bell", "Republican",          "Forney",
         5,            "Cole Hefner", "Republican",  "Mount Pleasant",
         6,          "Matt Schaefer", "Republican",             "Arp",
         7,               "Jay Dean", "Republican",        "Longview",
         8,            "Cody Harris", "Republican",       "Palestine",
         9,                       NA,           NA,                NA,
        10,         "Brian Harrison", "Republican",      "Midlothian",
        11,          "Travis Clardy", "Republican",     "Nacogdoches",
        12,             "Kyle Kacal", "Republican", "College Station",
        13,              "Ben Leman", "Republican",            "Iola",
        14,          "John N. Raney", "Republican",           "Bryan",
        15,             "Steve Toth", "Republican",          "Conroe",
        16,           "Will Metcalf", "Republican",      "Montgomery",
        17,            "John Cyrier", "Republican",        "Lockhart",
        18,          "Ernest Bailes", "Republican",        "Shepherd",
        19,            "James White", "Republican",       "Hillister",
        20,           "Terry Wilson", "Republican",      "Georgetown",
        21,            "Dade Phelan", "Republican",        "Beaumont",
        22,           "Joe Deshotel", "Democratic",        "Beaumont",
        23,        "Mayes Middleton", "Republican",       "Galveston",
        24,            "Greg Bonnen", "Republican",     "Friendswood",
        25,             "Cody Vasut", "Republican",        "Angleton",
        26,           "Jacey Jetton", "Republican",        "Richmond",
        27,           "Ron Reynolds", "Democratic",   "Missouri City",
        28,             "Gary Gates", "Republican",        "Richmond",
        29,            "Ed Thompson", "Republican",        "Pearland",
        30,        "Geanie Morrison", "Republican",        "Victoria",
        31,           "Ryan Guillen", "Republican", "Rio Grande City",
        32,       "Todd Ames Hunter", "Republican",  "Corpus Christi",
        33,         "Justin Holland", "Republican",           "Heath",
        34,           "Abel Herrero", "Democratic",        "Robstown",
        35,         "Oscar Longoria", "Democratic",         "La Joya",
        36,           "Sergio Muñoz", "Democratic",        "Palmview",
        37,         "Alex Dominguez", "Democratic",     "Brownsville",
        38,             "Erin Gamez", "Democratic",     "Brownsville",
        39,       "Armando Martinez", "Democratic",         "Weslaco",
        40,          "Terry Canales", "Democratic",        "Edinburg",
        41,          "Robert Guerra", "Democratic",         "Mission",
        42,        "Richard Raymond", "Democratic",          "Laredo",
        43,           "J. M. Lozano", "Republican",      "Kingsville",
        44,           "John Kuempel", "Republican",          "Seguin",
        45,           "Erin Zwiener", "Democratic",       "Driftwood",
        46,            "Sheryl Cole", "Democratic",          "Austin",
        47,          "Vikki Goodwin", "Democratic",          "Austin",
        48,           "Donna Howard", "Democratic",          "Austin",
        49,          "Gina Hinojosa", "Democratic",          "Austin",
        50,           "Celia Israel", "Democratic",          "Austin",
        51,        "Eddie Rodriguez", "Democratic",          "Austin",
        52,         "James Talarico", "Democratic",      "Round Rock",
        53,            "Andrew Murr", "Republican",        "Junction",
        54,           "Brad Buckley", "Republican",          "Salado",
        55,             "Hugh Shine", "Republican",          "Temple",
        56,       "Charles Anderson", "Republican",          "Lorena",
        57,            "Trent Ashby", "Republican",          "Lufkin",
        58,          "DeWayne Burns", "Republican",        "Cleburne",
        59,         "Shelby Slawson", "Republican",    "Stephenville",
        60,           "Glenn Rogers", "Republican",         "Graford",
        61,              "Phil King", "Republican",     "Weatherford",
        62,           "Reggie Smith", "Republican",     "Van Alstyne",
        63,             "Tan Parker", "Republican",    "Flower Mound",
        64,            "Lynn Stucky", "Republican",          "Sanger",
        65,       "Michelle Beckley", "Democratic",      "Carrollton",
        66,           "Matt Shaheen", "Republican",           "Plano",
        67,             "Jeff Leach", "Republican",           "Allen",
        68,          "David Spiller", "Republican",       "Jacksboro",
        69,            "James Frank", "Republican",   "Wichita Falls",
        70,          "Scott Sanford", "Republican",        "McKinney",
        71,           "Stan Lambert", "Republican",         "Abilene",
        72,             "Drew Darby", "Republican",      "San Angelo",
        73,        "Kyle Biedermann", "Republican",  "Fredericksburg",
        74,          "Eddie Morales", "Democratic",      "Eagle Pass",
        75,          "Mary González", "Democratic",           "Clint",
        76,    "Claudia Ordaz Perez", "Democratic",         "El Paso",
        77,         "Evelina Ortega", "Democratic",         "El Paso",
        78,              "Joe Moody", "Democratic",         "El Paso",
        79,             "Art Fierro", "Democratic",         "El Paso",
        80,             "Tracy King", "Democratic",          "Uvalde",
        81,        "Brooks Landgraf", "Republican",          "Odessa",
        82,           "Tom Craddick", "Republican",         "Midland",
        83,         "Dustin Burrows", "Republican",         "Lubbock",
        84,            "John Frullo", "Republican",         "Lubbock",
        85,        "Phil Stephenson", "Republican",         "Wharton",
        86,        "John T. Smithee", "Republican",        "Amarillo",
        87,             "Four Price", "Republican",        "Amarillo",
        88,               "Ken King", "Republican",        "Canadian",
        89,            "Candy Noble", "Republican",           "Lucas",
        90,       "Ramon Romero Jr.", "Democratic",      "Fort Worth",
        91,        "Stephanie Klick", "Republican",      "Fort Worth",
        92,             "Jeff Cason", "Republican",         "Bedford",
        93,            "Matt Krause", "Republican",          "Haslet",
        94,        "Tony Tinderholt", "Republican",       "Arlington",
        95,         "Nicole Collier", "Democratic",      "Fort Worth",
        96,             "David Cook", "Republican",       "Mansfield",
        97,          "Craig Goldman", "Republican",      "Fort Worth",
        98,   "Giovanni Capriglione", "Republican",       "Southlake",
        99,          "Charlie Geren", "Republican",      "Fort Worth",
       100,       "Jasmine Crockett", "Democratic",          "Dallas",
       101,           "Chris Turner", "Democratic",   "Grand Prairie",
       102,        "Ana-Maria Ramos", "Democratic",      "Richardson",
       103,          "Rafael Anchia", "Democratic",          "Dallas",
       104,       "Jessica González", "Democratic",          "Dallas",
       105,             "Terry Meza", "Democratic",          "Irving",
       106,        "Jared Patterson", "Republican",          "Frisco",
       107,         "Victoria Neave", "Democratic",        "Mesquite",
       108,           "Morgan Meyer", "Republican",          "Dallas",
       109,           "Carl Sherman", "Democratic",          "DeSoto",
       110,              "Toni Rose", "Democratic",          "Dallas",
       111,           "Yvonne Davis", "Democratic",          "DeSoto",
       112,      "Angie Chen Button", "Republican",         "Garland",
       113,          "Rhetta Bowers", "Democratic",         "Rowlett",
       114,            "John Turner", "Democratic",          "Dallas",
       115,          "Julie Johnson", "Democratic",  "Farmers Branch",
       116,  "Trey Martinez Fischer", "Democratic",     "San Antonio",
       117,          "Philip Cortez", "Democratic",     "San Antonio",
       118,             "John Lujan", "Republican",     "San Antonio",
       119,       "Elizabeth Campos", "Democratic",     "San Antonio",
       120, "Barbara Gervin-Hawkins", "Democratic",     "San Antonio",
       121,          "Steve Allison", "Republican",     "San Antonio",
       122,            "Lyle Larson", "Republican",     "San Antonio",
       123,           "Diego Bernal", "Democratic",     "San Antonio",
       124,           "Ina Minjarez", "Democratic",     "San Antonio",
       125,              "Ray Lopez", "Democratic",     "San Antonio",
       126,            "Sam Harless", "Republican",          "Spring",
       127,            "Dan Huberty", "Republican",          "Humble",
       128,           "Briscoe Cain", "Republican",       "Deer Park",
       129,            "Dennis Paul", "Republican",         "Houston",
       130,          "Tom Oliverson", "Republican",         "Cypress",
       131,             "Alma Allen", "Democratic",         "Houston",
       132,         "Mike Schofield", "Republican",            "Katy",
       133,             "Jim Murphy", "Republican",         "Houston",
       134,            "Ann Johnson", "Democratic",         "Houston",
       135,          "Jon Rosenthal", "Democratic",         "Houston",
       136,          "John Bucy III", "Democratic",          "Austin",
       137,                "Gene Wu", "Democratic",         "Houston",
       138,             "Lacey Hull", "Republican",         "Houston",
       139,         "Jarvis Johnson", "Democratic",         "Houston",
       140,          "Armando Walle", "Democratic",         "Houston",
       141,     "Senfronia Thompson", "Democratic",         "Houston",
       142,      "Harold Dutton Jr.", "Democratic",         "Houston",
       143,          "Ana Hernandez", "Democratic",         "Houston",
       144,         "Mary Ann Perez", "Democratic",         "Houston",
       145,      "Christina Morales", "Democratic",         "Houston",
       146,          "Shawn Thierry", "Democratic",         "Houston",
       147,                       NA,           NA,                NA,
       148,     "Penny Morales Shaw", "Democratic",         "Houston",
       149,              "Hubert Vo", "Democratic",         "Houston",
       150,        "Valoree Swanson", "Republican",          "Spring"
  )
tx_members$District <- as.character(tx_members$District)