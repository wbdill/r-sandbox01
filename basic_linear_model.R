# linear regression
# https://www.scribbr.com/statistics/linear-regression-in-r/
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("broom")

library(ggplot2)
library(dplyr)
library(readr)
library(broom)
rm(list = ls())

income.data <- read_csv("c:/github/r-sandbox01/data/income.data.csv")
str(income.data)
glimpse(income.data)

summary(income.data)

# do we have a normal distribution?
hist(income.data$happiness)
hist(income.data$happiness, breaks = 30)
?hist

# basic scatterplot
?plot
plot(happiness ~ income, data = income.data)

# create the linear model
?lm
income.happiness.lm <- lm(happiness ~ income, data = income.data)


summary(income.happiness.lm)
# happiness increased .713 per unit of income increase
# adjusted R-squared = .7488 so very high correlation
# p-value < .05


# qqplot to get a visual of normality of residuals.  Normal distribution is necessary for a good model fit.
qqnorm(residuals(income.happiness.lm))
qqline(residuals(income.happiness.lm),  col="red")

shapiro.test(income.happiness.lm$residuals)
# p-value > .05 means we ACCEPT the null hypothesis that the samples come from normal distribution. 


plot(income.happiness.lm)
# QQ plot fits well to normal distribution
# the red lines representing the mean of the residuals are all basically horizontal and centered around zero. 
# This means there are no outliers or biases in the data that would make a linear regression invalid.

# make pretty graph

income.graph <- ggplot(income.data, aes(x=income, y=happiness)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +  # to hide the error spread: , se = FALSE
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x $10,000)",
       y = "Happiness score (0 to 10)")

income.graph

# do a prediction based on income value
predict(income.happiness.lm , data.frame(income = 20))
