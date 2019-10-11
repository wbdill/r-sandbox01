# https://www.datacamp.com/courses/data-visualization-with-ggplot2-2
rm(list = ls())
install.packages("Hmisc")
library(tidyverse)

load("C:/GitHub/r-sandbox01/DataCamp/data/CHIS2009_reduced_2.Rdata")
adult


#========== Ch 1: Statistics ==========
# stat_bin() is the default stat for histogram, bar and freqpoly
str(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") # method = "loess", se = FALSE

library(Hmisc)
set.seed(123)
xx <- rnorm(100)
smean.sdl(xx)
smean.sdl(xx, mult = 1)  # mult is # of std dev.  defaults to 2
mean_sdl(xx, mult = 1)   # mean_sdl is ggplot's mean.sdl

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  #geom_jitter() +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1)) # mean_sdl is ggplot's mean.sdl

?smean.cl.normal

?stat_function
# other stat_ functions
#stat_summary()
#stat_function()
#stat_qq()
library(MASS)
mam.new <- data.frame(body = log10(mammals$body))
ggplot(mam.new, aes(x = body)) +
  geom_histogram(aes(y = ..density..)) +   # WTF is ..density.. ?????
  geom_rug() +
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(mam.new$body), 
                           sd = sd(mam.new$body)))

## qq plot - how he knows what to use for slope and int is beyond me.
mam.new$slope <- diff(quantile(mam.new$body, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
mam.new$int <- quantile(mam.new$body, 0.25) - mam.new$slope * qnorm(0.25)

ggplot(mam.new, aes(sample = body)) +
  stat_qq() +
  geom_abline(aes(slope = slope, intercept = int), col = "red")


#========== Ch 2: Coordinates and Facets ==========

#========== Ch 3: Themes ==========

#========== Ch 4: Best Practices ==========

#========== Ch 5: Case Study ==========