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

ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)

model_score_2 <- lm(score ~ bty_avg, data = evals)
model_score_2
get_regression_table(model_score_2) # get_regression_table part of moderndive pkg

get_regression_points(model_score_2)


ggplot(evals, aes(x = rank, y = score)) +
  geom_boxplot() +
  labs(x = "rank", y = "score")

evals %>%
  group_by(rank) %>%
  summarize(n = n(), mean_score = mean(score), sd_score = sd(score))

model_score_4 <- lm(score ~ rank, data = evals)
get_regression_table(model_score_4)


# plot residuals on on histogram.
model_score_4_points <- get_regression_points(model_score_4)
ggplot(model_score_4_points, aes(x = residual)) +
  geom_histogram() +
  labs(x = "residuals", title = "Residuals from score ~ rank model")

#----- Chapter 3: Multiple regression -----

house_prices <- house_prices %>%
  mutate(log10_price = log10(price),
         log10_size = log10(sqft_living))

glimpse(house_prices)

ggplot(house_prices, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# remove outlier (and re-run above graph)
house_prices <- house_prices %>%
  filter(bedrooms < 20)

# model price on size and bedrooms
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, data = house_prices)
get_regression_table(model_price_2)
get_regression_points(model_price_2)

model_price_4 <- lm(log10_price ~ log10_size + waterfront, data = house_prices)
get_regression_table(model_price_4)

ggplot(house_prices, aes(x=log10_size, y=log10_price, col = waterfront)) +
  geom_point() +
  facet_wrap(~waterfront) +
  geom_smooth(method="lm", se = FALSE, color = "black")

#multiple data point predictions
get_regression_table(model_price_4)

# get new data frame with column names matching the columns in the model
new_houses_2 <- data_frame(
  log10_size = c(2.9, 3.1) ,
  waterfront = c(TRUE, FALSE)
)
# call get_regression_points with newdata = <new data frame>
get_regression_points(model_price_4, newdata = new_houses_2) %>% 
  mutate(price_hat = 10^log10_price_hat)

#----- Chapter 4: Model assessment -----
