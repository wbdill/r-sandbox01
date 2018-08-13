# DataCamp Course - Exploratory comicsa analysis
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
