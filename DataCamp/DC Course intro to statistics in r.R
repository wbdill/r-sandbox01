
# https://campus.datacamp.com/courses/introduction-to-statistics-in-r
library(tidyverse)
rm(list = ls())
#----- Ch 1 Summary Statistics -----
food_consumption <- readRDS("data/food_consumption.rds")

quantile(food_consumption$co2_emission)  # default quartile
quantile(food_consumption$co2_emission, probs = seq(0,1,0.2)) #quintile
quantile(food_consumption$co2_emission, probs = seq(0,1,0.1)) #decile

# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emission),
            sd_co2 = sd(co2_emission))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption, aes(x = co2_emission)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

# Compute the first and third quantiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - (1.5*iqr)
upper <- q3 + (1.5*iqr)

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission < lower | total_emission > upper)

#----- Ch 2 Random Numbers and Probability -----
amir_deals <- readRDS("data/seller_1.rds")


amir_deals %>% 
  count(product) %>% 
  mutate(prob = n / sum(n))

set.seed(31)
amir_deals %>% 
  sample_n(5, replace = FALSE)

amir_deals %>% 
  sample_n(5, replace = TRUE)

die <- tribble(~n,
       1,
       2,
       3,
       4,
       5,
       6,)

sample_n(die, 1000, replace = TRUE) %>% 
  ggplot(aes(n)) + geom_histogram(bins = 6)



#----- Ch 3 More Distributions and the Central Limit Theorem -----




#----- Ch 4 Correlation and Experimental Design -----





sugar <- readRDS("data/world_happiness_sugar.rds")
