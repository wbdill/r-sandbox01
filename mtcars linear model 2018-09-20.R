#install.packages("broom")
library(broom)
library(tidyverse)

head(mtcars)

#----- plot weight and mpg  -----
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth()

#----- linear model: predict mpg from wt -----
mod <- lm(mpg ~ wt, data = mtcars)

coef(mod)
summary(mod)

#----- broom's augment() gives fitted, residuals, hat (predicted), cook's distanct, etc. -----
augment(mod)  


new <- data.frame(wt = 4.5)  # create data from with same input var name
predict(mod, newdata = new)   # ask the model to predict the output var weight

c = data.frame(coef(mod))
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_abline(slope = c[2,1], intercept = c[1,1])

