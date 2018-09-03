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
install.packages("gapminder")
library(gapminder)
?gapminder
str(gapminder)

gap2007 <- filter(gapminder, year == 2007)

gap2007 %>%
  group_by(continent) %>%
  summarize(mean_le = mean(lifeExp),
            median_le = median(lifeExp))
gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()

# get measures of spread
gap2007 %>%
  group_by(continent) %>%
  summarize(sd = sd(lifeExp),
            iqr = IQR(lifeExp),
            n = n())

# density shows the shape of the data - modality (# of humps)
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)

# population
gap2007 %>%
  ggplot(aes(x = pop)) +
  geom_density()

gap2007 <- gap2007 %>% 
  mutate(log_pop = log(pop))

#density plot of log_pop shows clear unimodal distribution
gap2007 %>%
  ggplot(aes(x = log_pop)) +
  geom_density()

#----- Chapter 4 Email -----
library(openintro)
?email
email$spam <- as.factor(email$spam)
str(email)
# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char),
            IQR(num_char))
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()

email %>%
  group_by(spam) %>%
  summarize(median(exclaim_mess),
            IQR(exclaim_mess),
            n())

#----- email spam plots -----
#log(0) = -Inf, so add .01 before log to aleviate this
email %>%
  group_by(spam) %>%
  mutate(log_exclaim = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = spam, y = log_exclaim)) +
  geom_boxplot()

email %>%
  group_by(spam) %>%
  mutate(log_exclaim = log(exclaim_mess + .01)) %>%
  ggplot( aes(log_exclaim)) +
  geom_histogram() +
  facet_wrap(~spam)

email %>%
  group_by(spam) %>%
  mutate(log_exclaim = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim, fill = spam)) +
  geom_density(alpha = 0.3)

# collapse the tail of msgs w/ images into a single category with mutate
table(email$image)
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")   #position = "fill|dodge|identity)

# compare image and attach.  Do any emails have image > attach?
sum(email$image != email$attach)
sum(email$image > email$attach)
email %>%
  filter(email$image != email$attach) %>%
  select(image, attach)


# Reorder levels
email$number <- factor(email$number, levels = c("none", "small", "big"))

# Construct plot of number
ggplot(email, aes(x = number)) +
  geom_bar() +
  facet_wrap(~spam, labeller = label_both)
