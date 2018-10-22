# Great Courses: Learning Statistics: Concepts and Applications in R
# Professor Talithia Williams

# Harman23.cor1
rm(list = ls())
library(datasets)
data("Harman23.cor")
round(Harman23.cor$cov,2)



#----- Lesson 5: Continuous and Normal Distributions -----
#----- Find P(X <= 45)
pnorm(45, mean = 50, sd = 10)

# find P(X >= 60)
pnorm(60, mean = 50, sd = 10, lower.tail = FALSE)

#----- probability of plus/minus 1,2,3 standard deviations from mean 
pnorm(1) - pnorm(-1)  # 68%
pnorm(2) - pnorm(-2)  # 95%
pnorm(3) - pnorm(-3)  # 99%


# what is 90% percentile for N(10,5)?
qnorm(.90, mean = 10, sd = 5)

# given X~N(129,23) what is P(X>158)?
pnorm(158, mean = 129, sd = 23, lower.tail=FALSE)  # ans: 10.3%

# given X~N(129,23) what is the level where 95% of population has lower?
qnorm(0.95, mean = 129, sd = 23)

#----- Lesson 6: Covariance and Correlation -----
install.packages("car")
library(car)
data("Salaries")
head(Salaries)
str(Salaries)
summary(Salaries)

hist(Salaries$yrs.service)

cor(Salaries[,c(3,4,6)])

summary(cars)

#----- Lesson 7: Validating Statistical Assumptions -----
library(datasets)
library(RColorBrewer)
attach(iris)

barplot(iris$Petal.Length, main = "Petal Length")

barplot(iris$Sepal.Length, main = "Sepal Length")

barplot(table(iris$Species,iris$Sepal.Length), 
        col = brewer.pal(3,"Set1"), 
        main = "Stacked Plot of Sepal Length by Species")

summary(iris)

boxplot(iris[,1: 4], names=c("SepL","SepW", "PetL", "PetW"))


boxplot(iris$Petal.Length~iris$Species, main = "Petal Length vs. Species", col=heat.colors(3))

plot(iris$Petal.Length, main="Petal Length", ylab = "Petal Length", xlab = "Species")

hist(iris$Petal.Width, breaks = 13)
hist(iris$Petal.Width, breaks = 25)


# Density plot
dens.pw = density(iris$Petal.Width)
plot(dens.pw, ylab = "Frequency", xlab = "Width", main= "Petal Width Density")

# Contour plot
library(MASS)
petal.dens = kde2d(iris$Petal.Length, iris$Petal.Width)
contour(petal.dens)

# Heat map
library(MASS)
petal.dens = kde2d(iris$Petal.Length, iris$Petal.Width)
image(petal.dens)

#----- Lesson 8: Sample Size and Sampling Distributions -----

times <- (0: 59)
counts <- c(49, 51, 50, 85, 47, 61, 29, 29, 32, 21, 36, 
            38, 30, 27, 24, 34, 38, 37, 24, 32, 26, 23, 27, 22, 19, 
            10, 10, 12, 13, 8, 12, 5, 6, 6, 1, 11, 2, 4, 2, 1, 3, 0, 
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
visits <- rep.int(times, counts)

# take n samples 1000 times and store the mean of your n samples into a matrix
mean.age<-function(n) {
  trials = 1000
  my.samples <- matrix(sample(visits, size=n*trials, 
                              replace=TRUE), trials)
  means <- apply(my.samples, 1, mean)
  means
}

?par
par(mfrow=c(2,3))  # set par[amater] mfrow(2,3) means 2 rows 3 cols
MA1<-mean.age(1)
MA2<-mean.age(2)
MA10<-mean.age(10)
MA20<-mean.age(20)
MA100<-mean.age(100)
MA200<-mean.age(200)
hist(MA1, xlim=c(0,60))
hist(MA2, xlim=c(0,60))
hist(MA10, xlim=c(0,60))
hist(MA20, xlim=c(0,60))
hist(MA100, xlim=c(0,60))
hist(MA200, xlim=c(0,60))

# change in variance as sample size increases
vars<- data.frame(n=c(1,2,10,20,100,200), 
                  variance = c(var(MA1), var(MA2), var(MA10), var(MA20), var(MA100), var(MA200)))
vars
plot(vars$n, vars$variance)

# qqplot to see how it compares to normal dist.
par(mfrow=c(2,3))
qqnorm(MA1)
qqnorm(MA2)
qqnorm(MA10)
qqnorm(MA20)
qqnorm(MA100)
qqnorm(MA200)

#Shapiro-Wilk normality test

shapiro.test(MA1)
shapiro.test(MA2)
shapiro.test(MA10)
shapiro.test(MA20)
shapiro.test(MA100)
shapiro.test(MA200)

# Central Limit Theory: sample means of any distribution approach normal distribution as sample size increases.
# need at least 30-40 samples.

#----- Lesson 9: Point Esimates and Standard Error -----
library(datasets)
library(ggplot2)
data("Orange")
Orange

plot(Orange$age, Orange$circumference)
ggplot(Orange, aes(age, circumference)) +
  geom_point() +
  geom_smooth()

data("women")
women
plot(women$height, women$weight)
ggplot(women, aes(height, weight)) + geom_point() + geom_smooth()

#----- mean vs median as an estimate.  Mean is more efficient b/c it gets closer to population statistic.
set.seed(1234)
x = cbind(rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1), 
          rnorm(100,0,1),rnorm(100,0,1),rnorm(100,0,1), rnorm(100,0,1),
          rnorm(100,0,1),rnorm(100,0,1), rnorm(100,0,1))
x
apply(x,2,'mean')  # apply function 'mean' to columns of matrix x.  (1 means rows, 2 means columns. see ?apply)
apply(x,2,'median')

round(mean(apply(x,2,'mean')),3)
round(mean(apply(x,2,'median')),3)
round(var(apply(x,2,'mean')),3)
round(var(apply(x,2,'median')),3)

#----- Lesson 10: Interval Estimates and Confidence Intervals -----

# simulate a population of 10,000 so we can know the actual population mean and sd.
set.seed(343) 
milk = 129-rexp(100000,0.95)
hist(milk, main="Histogram of Milk Population", col="red")

true_mean = mean(milk)
true_sd = sd(milk)

# sample from that simulated population to see how well the samples do against the known population stats.
set.seed(343)
sample_milk = sample(milk, size=50, rep = T)  # rep = T means sample with replacement
sample_mean = mean(sample_milk)
sample_mean
sample_mean - true_mean

# calculate 95% confidence interval (Z alpha/2 = 1.96)
n = 50
sample_mean - 1.96 * sd(sample_milk) / sqrt(n)  # lower
sample_mean + 1.96 * sd(sample_milk) / sqrt(n)

boxplot(milk, sample_milk)

#----- Lesson 11: Hypothesis Testing: 1 sample -----

#----- Lesson 12: Hypothesis Testing: 2 samples, paired test -----
library(ggplot2)
library(tidyverse)
data(chickwts)
summary(chickwts)
glimpse(chickwts)
ggplot(chickwts, aes(x = feed, y = weight)) + geom_boxplot()

meat = chickwts[chickwts$feed == "meatmeal", 1]
horse = chickwts[chickwts$feed == "horsebean", 1]
meat
horse
boxplot(meat, horse)

# manual T-test (Welch's T-test aka student's test) (assumes independent sample data  - not paired)
mean.meat = mean(meat)
mean.horse = mean(horse)
sd.meat = sd(meat)/sqrt(length(meat))
sd.horse = sd(horse)/sqrt(length(horse))
T.stat = (mean.meat - mean.horse)/sqrt(sd.meat^2+sd.horse^2)
T.stat

# built-in t.test function in R
?t.test
t.test(meat, horse)

# Paired t-test (dependent sample data)
install.packages("PairedData")
library(PairedData)
data(IceSkating)
attach(IceSkating)
IceSkating
?PairedData

summary(IceSkating)

qqplot(Extension, Flexion, xlim=c(1.5, 2.5), ylim=c(1.5, 2.5), pch=20, cex=2)
abline(a=0, b=1, lwd=3, col="red")

with(IceSkating,plot(paired(Extension,Flexion), type="McNeil")) # 

with(IceSkating, qqnorm(Extension-Flexion))
with(IceSkating, qqline(Extension-Flexion))

shapiro.test(Extension-Flexion)

t.test(Extension, Flexion, paired = TRUE)


# what if the diff b/t pairs are not normal?  Don't use t-test.
# use Wilcoxon signed-rank test.  It doesn't depend on data being normal.
wilcox.test(Extension, Flexion, paired=TRUE)

#----- Lesson 13: Linear Regression Models and assumptions -----
# explanatory / predictor / independent variable X
#               response  / dependent variable Y
# lm assumptions: 
# 1) Normality (check with qqplot of residuals)
# 2) Constant variability (no pattern in plot of residuals - homoscedacity)
# 3) Indepenence
# 4) Linearity (check with basic scatter plot)

rainfall = c(3.07, 3.55,3.9,4.38,4.79,5.3,5.42,5.99,6.45,6.77)
wheat = c(78,82,85,91,92,96,97,104,111,119)
summary(cbind(rainfall, wheat))
plot(rainfall, wheat)

wheat.mod <- lm(wheat ~ rainfall)
summary(wheat.mod)

plot(rainfall, wheat, pch=20, xlim = c(0,8), ylim = c(0,120))
abline(wheat.mod, lw = 3, col="red")

newdf <- data.frame(rainfall = c(5, 6, 6.25))
predict(wheat.mod, newdf)

# plot the residuals
plot(rainfall, residuals(wheat.mod))
abline(0,0, lw = 3, col="red")

# qqplot to get a visual of normality
qqnorm(residuals(wheat.mod))
qqline(residuals(wheat.mod), lwd=2, col="Blue")

# 3 deadly statistical sins:
# 1. Failure to randomize
# 2. "Accept" the alternative
# 3. Extrapolating (interpolating OK)

#----- Lesson 14: Regression Predictions, Confidence Intervals -----
summary(mtcars)
plot(mpg ~ wt, data=mtcars)
mpg_model = lm(mpg ~ wt, data = mtcars)
summary(mpg_model)

plot(mtcars$mpg, residuals(mpg_model))
abline(0,0, lw = 3, col="red")

qt(.975, df=28)  #quantiles of the t-distribution
# result: 2.0484
# std error from lm = 0.5591
# slope from lm = -5.3445
# -5.3445 +/- 2.0484 * 0.5591 = -4.1992 to -6.5987
# 95% sure that slope is b/t -4.1992 and -6.5987

# percentile of the t-distribution
pt(-9.559, df=28)  # = 1.29e-10 = Pr(>|t|) from sumary(mpg_model)

hist(mpg_model$residuals)
qqnorm(mpg_model$residuals)
qqline(mpg_model$residuals)

shapiro.test(mpg_model$residuals)  # p-value 0.1044 so residuals are NOT normal

#----- Lesson 15: Multiple Linear Regression -----
library(tidyverse)
library(skimr)
install.packages("MASS")
library(MASS)
data(Pima.tr)
head(Pima.tr)

glimpse(Pima.tr)
skim(Pima.tr)
ggplot(Pima.tr, aes(age, npreg)) + geom_jitter() + geom_smooth()

pairs(Pima.tr[1:4])
pairs(Pima.tr[5:8])

round(cor(Pima.tr[1:7]), 2)  # correlation matrix

lm1 <- lm(bmi~npreg + glu + bp + skin + ped + age + type, data = Pima.tr)
summary(lm1)
# p-value over .05 or .1 means that variable is not significant in the model.

lm2 <- lm(bmi~npreg + glu + bp + skin + ped + age + type, data = Pima.tr)
drop1(lm2, test = "F")
# npreg has highest p-value, so drop it

lm2 <- lm(bmi~glu + bp + skin + ped + age + type, data = Pima.tr)
drop1(lm2, test="F")

# glu has highest p-value so drop it
lm2 <- lm(bmi~bp + skin + ped + age + type, data = Pima.tr)
drop1(lm2, test="F")

lm2 <- lm(bmi~skin + ped + age + type, data = Pima.tr)
drop1(lm2, test="F")

lm2 <- lm(bmi~skin + ped + type, data = Pima.tr)
drop1(lm2, test="F")


#-----
lm3 <- lm(glu~bmi+npreg+bp+skin+ped+age+type, data = Pima.tr)
summary(lm3)
plot(lm3$fitted.values, lm3$residuals, pch=20)
hist(lm3$residuals)
qqnorm(lm3$residuals)
qqline(lm3$residuals)


# what about predictors that ARE highly correlated?
mtcars
mpg_model = lm(mpg~wt+hp, data=mtcars)
summary(mpg_model)

# add all variables
mpg_model = lm(mpg ~ . , data=mtcars) # dot means all other variables
summary(mpg_model)
#adjusted R^2 dropped with all variables.

pairs(mtcars[ ,c(1,3:4)])
pairs(mtcars[ ,c(5:7)])
pairs(mtcars[ ,c(1,3:7)], col='blue' )

round(cor(mtcars[ ,c(1,3:7)]), 2)

# stepwise regression
mpg_model2 <- step(lm(mpg ~ . , data=mtcars))

mpg_model2 <- lm(mpg ~ wt + qsec + am, data=mtcars)
summary(mpg_model2)


#----- Lesson 16: ANOVA: Comparing 3 Means -----

#----- Lesson 17:  -----
#----- Lesson 18:  -----
#----- Lesson 19:  -----
#----- Lesson 20:  -----
#----- Lesson 21:  -----
#----- Lesson 22:  -----
#----- Lesson 23:  -----
#----- Lesson 24:  -----

