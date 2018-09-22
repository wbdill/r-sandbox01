# https://www.rdocumentation.org/packages/openintro/versions/1.7.1
# http://www.openintro.org

install.packages("openintro")
library(openintro)
data(ncbirths)

rm(list = ls())
head(ncbirths)
glimpse(ncbirths)

ggplot(ncbirths, aes(weeks, weight)) + 
  geom_point()

ggplot(data = ncbirths, aes(x = cut(weeks, breaks = 5), y = weight)) + 
  geom_boxplot()

#----- plots of 4 data sets -----
ggplot(mammals, aes(BodyWt, BrainWt)) + geom_point()

ggplot(mlbBat10, aes(OBP, SLG)) + geom_point()

ggplot(bdims, aes(hgt, wgt, col = factor(sex)) ) + geom_point()

ggplot(smoking, aes(age, amtWeekdays)) + geom_point()

#----- plot log scales -----
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()

#----- filtering out outliers (AB > 200) ------
mlbBat10 %>%
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

#----- Correlation coefficient ignoring NAs ------
ncbirths %>%
  summarize(N = n(), r = cor(weeks, weight, use = "pairwise.complete.obs"))

#-----  ------
# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#-----  ------
# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG~OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

#-----  ------
mod <- lm(wgt ~ hgt, data = bdims)
coef(mod)
summary(mod)

# Mean of weights equal to mean of fitted values?
mean(fitted.values(mod)) == mean(bdims$wgt)

#----- broom pkg and augment()  ------
# broom's augment() takes a model object returns a data frame that contains the data on which the model was fit,
# along with several quantities specific to the regression model, including the fitted values, residuals, 
# leverage scores, and standardized residuals.
library(broom)
bdims_tidy <- augment(mod)
glimpse(bdims_tidy)

bdims_tidy %>%
  arrange(desc(.resid)) %>%
  head()

new <- data.frame(hgt = 176)  # create data from with same input var name
predict(mod, newdata = new)   # ask the model to predict the output var weight

# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - var_e/var_y)

# .hat in augment() is leverage score
# .cooksd in augment() is cook's distance (measure of outlier-ness)
