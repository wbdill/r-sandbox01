# https://www.youtube.com/watch?v=fQHlmip1Nwo&t=425s
# https://www.kaggle.com/zynicide/wine-reviews/version/4#winemag-data_first150k.csv
install.packages("dplyr")

library(dplyr)
library(ggplot2)

rm(list = ls())
wine <- read.csv("D:/Downloads/wine-reviews/winemag-data_first150k.csv"
                , stringsAsFactors = FALSE, encoding="UTF")
wine <- wine[, -c(1,3)] #remove cols 1 & 3
str(wine)
head(wine, 6)

#----------------------------------------------------------------------
#https://www.youtube.com/watch?v=Cce4WOZQSxk
# look at top 10
top_10_producers <- wine %>% 
  group_by(country) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10) %>%
  select(country)

top_10_producers <- as.character(top_10_producers$country)  # convert to char vector
top_10_producers


# get data from top 10 countries
select_point <- wine %>%
  filter(country %in% top_10_producers) %>%
  select(country, points) %>%
  arrange(country)
  

# quick view of overall data
ggplot(wine, aes(points, price)) + geom_point() + geom_smooth()


ggplot(select_point, aes(x = reorder(country, points, median), y = points)) +
  geom_boxplot(aes(fill = country)) +
  xlab("Country") +
  ylab("Points") +
  ggtitle("Distribution of top 10 Wine producing countries") +
  theme(plot.title = element_text(hjust = .5))
  #coord_flip()

# look at countries NOT in top 10 producers
wine %>% filter(!country %in% top_10_producers) %>%
  group_by(country) %>%
  summarize(median = median(points)) %>%
  arrange(desc(median))

top <- wine %>% group_by(country) %>%
  summarize(median = median(points)) %>%
  arrange(desc(median)) %>%
  select(country)

top

#----------------------------------------------------------------------
#https://www.youtube.com/watch?v=esl4TWLBSp8
# dplyr: intersect & setdiff

top <- top[1:10,]
top <- as.character(top$country)
top
both = intersect(top, top_10_producers)
both

topwine <- wine %>% group_by(variety) %>%
  summarize(number = n()) %>%
  arrange(desc(number)) %>%
  top_n(10)
topwine
topwine <- as.character(topwine$variety)
topwine

wine %>% filter(variety %in% topwine) %>%
  group_by(variety) %>%
  summarize(median = median(points)) %>%
  ggplot(aes(x = reorder(variety, median), y = median)) +
  geom_col(aes(fill = variety)) + 
  xlab("Variety") +
  ylab("Median Point") +
  scale_x_discrete(labels = abbreviate)


#best bang for the buck
top15p <- wine %>%
  arrange(desc(points)) %>%
  filter(points > quantile(points, prob = .85))
top15p

cheapest15p <- wine %>% 
  arrange(price) %>% 
  head(nrow(top15p)) #select exact # of rows as top15p set

goodvalue <- intersect(top15p, cheapest15p)
goodvalue
