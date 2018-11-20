# Data Camp: Modeling with data in the tidyverse (Albert Kim)
# 1. Intro to modelling: theory and terminology
# 2. Basic regression
# 3. Multiple regression
# 4. Model assessment

rm(list = ls())


#----- Chapter 1: Intro to modelling: theory and terminology -----

library(tidyverse)
library(moderndive)

glimpse(house_prices)

# this dataset is included in moderndive pkg as "house_prices"
# https://www.kaggle.com/harlfoxem/housesalesprediction/version/1
#kc <- read.csv("C:/GitHub/r-sandbox01/DataCamp/data/kc_house_data.csv")
#glimpse(kc)

# the severe right-skew of the data makes it hard to distinguish most of the data.  
ggplot(house_prices, aes(x = price)) +
  geom_histogram() +
  labs(x = "house price", y = "count")

# A log10 transform fixes this.
house_prices <- house_prices %>%
  mutate(log10_price = log10(price))

# now data is more normally distributed.
ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram() +
  labs(x = "log 10 house price", y = "count")


# we see a similar pattern in sqft_living
ggplot(house_prices_2, aes(x = sqft_living)) +
  geom_histogram() +
  labs(x = "size", y = "count")

house_prices_2 <- house_prices %>%
  mutate(log10_size = log10(sqft_living))

ggplot(house_prices_2, aes(x = log10_size)) +
  geom_histogram() +
  labs(x = "log10 size", y = "count")


# evals dataset
ggplot(evals, aes(x = bty_avg,)) +
  geom_histogram(binwidth = .5) +
  labs(x = "Beauty score", y = "count")

ggplot(evals, aes(x = bty_avg, y = score)) +
         geom_jitter() +
         labs(x = "Beauty score", y = "teaching score")

# weak positive correlation
evals %>%
  summarize(correlation = cor(score, bty_avg))

ggplot(house_prices, aes(x = waterfront, y = log10_price)) +
  geom_boxplot() +
  labs(x = "waterfront", y = "log10 price")

house_prices %>%
  group_by(waterfront) %>%
  summarize(mean_log10_price = mean(log10_price), n = n())


#----- Chapter 2: Basic regression -----



#----- Chapter 3: Multiple regression -----

#----- Chapter 4: Model assessment -----
