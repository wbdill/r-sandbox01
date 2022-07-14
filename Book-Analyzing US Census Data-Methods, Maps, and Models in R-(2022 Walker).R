# Book-Analyzing US Census Data-Methods, Maps, and Models in R-(2022 Walker)
# https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html

library(scales)
library(tidycensus)
library(tidyverse)
library(systemfonts)
#system_fonts() %>% View()
#windowsFonts()

#----- 4.2 Customizing ggplot2 visualizations -----
metros <-  get_acs(
  geography = "cbsa",  # largest metropolitan areas in the United States
  variables = "DP03_0021P",  # percent of commuters that take public transportation to work
  summary_var = "B01003_001",
  survey = "acs1",
  year = 2019
) %>%
  slice_max(summary_est, n = 10)  # top n

ggplot(metros, aes(x = NAME, y = estimate)) + 
  geom_col()

metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col()

metros %>%
  mutate(NAME = str_remove(NAME, "-.*$")) %>%
  mutate(NAME = str_remove(NAME, ",.*$")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col(color = "navy", fill = "navy", alpha = 0.6, width = 0.85) +  
  theme_minimal(base_size = 12) + # , base_family = "Tahoma"
  #theme(text=element_text(family="Tahoma")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) + 
  labs(title = "Public Transit Commute Share", 
       subtitle = "2019 ACS 1-year estimates", 
       y = "", 
       x = "ACS estimate", 
       caption = "Source: US Census Bureau ACS 1-yr 2019 (DP03_0021P)") 


#----- 4.3 Visualizing margins of error -----
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  year = 2020
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))

ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_minimal(base_size = 12.5) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "2016-2020 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::label_dollar())

#----- 4.4 Visualizing ACS estimates over time -----
years <- 2005:2019
names(years) <- years
f <- get_acs(
  geography = "county",
  variables = "B25077_001", # median home value
  state = "TN",
  #county = "Williamson",
  year = 2021,
  survey = "acs1"
)
med_home_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B25077_001", # median home value
    state = "TN",
    #county = "Williamson",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")
med_home_value2 <- med_home_value %>% 
  separate(NAME, c("county", "state"), ", ") %>% 
  mutate(county = str_remove(county, " County"))

ggplot(med_home_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()

counties <- c("Williamson", "Davidson", "Shelby", "Rutherford", "Maury")

med_home_value2 %>% 
  filter(county %in% counties) %>% 
ggplot(aes(x = year, y = estimate, group = county, color = county)) + 
  geom_ribbon(aes(ymax = estimate + moe, ymin = estimate - moe), alpha = 0.1, color = "gray", size = 0.1) + 
  geom_line(size = 1.25) + 
  geom_point(size = 2.5) + 
  theme_gray(base_size = 14) +
  #theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::label_dollar(scale = .001, suffix = "k")) + 
  #facet_wrap(~county) +
  labs(title = paste("Median home value in", med_home_value2$state[1]),
       subtitle = "US Census Bureau - American Community Survey",
       x = "Year",
       y = "Median Home Value (ACS estimate)",
       color = "County",
       fill = "",
       caption = "chart: @bdill\ndata: US Census Bureau ACS 1-year (B25077_001)\nbands are 90% confidence interval")

#----- 4.5.1 Preparing data from the Population Estimates API -----
age_sex <- get_estimates(
  geography = "state",
  state = c("MS", "AL", "TN", "LA", "FL", "GA"),
  #state = "TN",
  #county = c("Williamson", "Shelby", "Davidson", "Maury", "Rutherford", "Dyer"),
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019,
  cache = TRUE
) 

age_sex_2 <- filter(age_sex, str_detect(AGEGROUP, "^Age"), SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

#ggplot(age_sex_2, aes(x = value, y = AGEGROUP, fill = SEX)) + geom_col()

ggplot(age_sex_2, aes(x = value, y = AGEGROUP, fill = SEX)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_minimal(base_size = 12) + 
  scale_fill_manual(values = c("#ffb3de", "#7777ff")) + 
  scale_x_continuous(
    #limits = 250000 * c(-1,1) ,
    labels = ~scales::number_format(scale = .001, suffix = "k")(abs(.x)),
  ) + 
  scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  facet_wrap(~NAME, scales = "free") +
  labs(x = "Population", 
       y = "Age Group", 
       title = paste0("Population Age Tree"), 
       fill = "", 
       caption = "chart: @bdill\ndata: US Census Bureau population estimates 2019 (SEX & AGEGROUP)")


#----- 4.6 Visualizing group-wise comparisons -----
install.packages("tidyverse")
install.packages(c("ggplot2", "scales"))

sessionInfo()


#----- 5.1 Basic usage of tigris
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
la_tracts <- tracts("NM", "Los Alamos")
plot(la_tracts$geometry)

st <- states()
plot(st$geometry)

nm_counties <- counties("NM")
plot(nm_counties$geometry)

la_water <- area_water("NM", "Los Alamos")
plot(la_water$geometry)


