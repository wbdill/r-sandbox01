
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

# continuous uniform
# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(5, min, max, lower.tail = F)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20

# binomial - probability distribution of # of successes in a sequence of independent trials.
rbinom(1000, size = 10, prob = .5) %>% 
  hist()

?dbinom
dbinom(7, 10, .5)  # prob of getting 7 heads exactly in 10 flips of a 50/50 coin
pbinom(7, 10, .5)  # prob of getting 7 heads or less in 10 flips of a 50/50 coin
pbinom(7, 10, .5, lower.tail = F)  # prob of getting > 7 heads in 10 flips of a 50/50 coin


#----- Ch 3 More Distributions and the Central Limit Theorem -----
amir_deals <- readRDS("data/seller_1.rds")

# Histogram of amount with 10 bins
ggplot(amir_deals, aes(x=amount)) +
  geom_histogram(bins=10)

summary(amir_deals)
skimr::skim(amir_deals)

pnorm(7500, 5000, 2000)  # Probability of deal < 7500
pnorm(1000, 5000, 2000, lower.tail = F)  # Probability of deal > 1000
pnorm(7000, 5000, 2000) - pnorm(3000, 5000, 2000)# Probability of deal between 3000 and 7000
qnorm(.75, 5000, 2000, lower.tail = F) # Calculate amount that 75% of deals will be more than


pnorm(1000, 5000, 2000, lower.tail = F)
pnorm(1000, 6000, 2600, lower.tail = F)

hist(amir_deals$num_users)
ggplot(amir_deals, aes(x=num_users)) +
  geom_histogram()

sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

#Poisson distribution.  avg = lambda
ppois(5, lambda = 10)  # prob of getting <=5 given avg=10

# expnential distribution 
?pexp
pexp(1, rate = 2) # P(wait < 1min) given rate = 1 every 2 mins

# (Student's) t-distribution - parameter is degrees of freedom

# Log-normal distribution - variables whose log is normally distributed

#----- Ch 4 Correlation and Experimental Design -----

world_happiness <- readRDS("data/world_happiness_sugar.rds")
# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(life_exp, happiness_score)) + geom_point() +
  geom_smooth(method = "lm", se = F)
# Correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp)) + geom_point()
cor(world_happiness$life_exp, world_happiness$gdp_per_cap)

# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness, aes(gdp_per_cap, happiness_score)) + geom_point()
# Calculate correlation
cor(world_happiness$happiness_score, world_happiness$gdp_per_cap)

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))
# Scatterplot of log_gdp_per_cap vs. happiness_score
ggplot(world_happiness, aes(log_gdp_per_cap, happiness_score)) +
  geom_point() + geom_smooth(method = "lm")
# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)

# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness, aes(grams_sugar_per_day, happiness_score)) + geom_point()
# Correlation between grams_sugar_per_day and happiness_score
cor(world_happiness$grams_sugar_per_day, world_happiness$happiness_score)





