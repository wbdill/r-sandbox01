# DataCamp Course - Exploratory data analysis
# https://www.comicsacamp.com/courses/exploratory-comicsa-analysis
library(tidyverse)

rm(list = ls())
library(data.table)
comics <- fread("C:/GitHub/r-sandbox01/DataCamp/data/comics.csv")
glimpse(comics)

#----- factorize several vars -----
comics$id    <- as.factor(comics$id)
comics$align <- as.factor(comics$align)
comics$eye   <- as.factor(comics$eye)
comics$hair  <- as.factor(comics$hair)
comics$gender <- as.factor(comics$gender)
comics$gsm   <- as.factor(comics$gsm)
comics$alive <- as.factor(comics$alive)
comics$publisher <- as.factor(comics$publisher)
glimpse(comics)

head(comics)

levels(comics$align)
levels(comics$gender)
table(comics$align, comics$gender)

#----- drop levels of "Reformed Criminals" b/c the counts are insignificant -----
comics <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()

#----- couple of plots -----
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "dodge")

ggplot(comics, aes(x = gender, fill = align)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

#----- stacked bar plots -----
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()

# position = "fill" shows proportions rather than counts.
ggplot(comics, aes(x = align, fill = gender)) + 
  geom_bar(position = "fill") +
  ylab("proportion")

#----- reorder data before plotting -----
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~gender)

#----- cars dataset (chapter 2) -----
cars <- fread("C:/GitHub/r-sandbox01/DataCamp/data/cars04.csv")
str(cars)

#filter to common cylinders
common_cyl <- filter(cars, ncyl %in% c(4,6,8))

# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)


cars %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Horsepower")

#----- boxplots vs density -----
cars %>%
  ggplot(aes(x = 1, y = city_mpg)) +
  geom_boxplot()

cars %>%
  ggplot(aes(x = city_mpg)) +
  geom_density()

#----- multivariate -----
# facet wrap with labeller set
common_cyl %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv, labeller = label_both) +
  ggtitle("Cylinders and SUV")

#----- Chapter 3 County Demographics -----
