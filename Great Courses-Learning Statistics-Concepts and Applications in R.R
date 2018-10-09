# Great Courses: Learning Statistics: Concepts and Applications in R
# Professor Talithia Williams

# Harman23.cor1

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