# https://campus.datacamp.com/courses/visualization-best-practices-in-r/proportions-of-a-whole?ex=1
install.packages("waffle")
library(data.table)
library(tidyverse)
library(waffle)

who_disease <- fread("C:/GitHub/r-sandbox01/DataCamp/data/who_disease.csv")
glimpse(who_disease)
#========== Chapter 1: Proportions of a whole ==========
ggplot(who_disease, aes(x=region)) +
  geom_bar()


# filter data to AMR region. 
amr_region <- who_disease %>%
  filter(region == "AMR")

# map x to year and y to cases.  # lower alpha to 0.5 to see overlap. 
ggplot(amr_region, aes(x=year, y=cases)) + 
  geom_point(alpha = 0.5)


# pie chart
disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  geom_col() +
  theme_void() +
  coord_polar(theta = 'y') +
  labs(title = "Proportions of diseases")

# waffle chart (waffle library)
disease_counts <- who_disease %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100))

case_counts <- disease_counts$percent
names(case_counts) <- disease_counts$disease
waffle(case_counts)


disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, year) %>% # note the addition of year to the grouping.
  summarise(total_cases = sum(cases))
ggplot(disease_counts, aes(x = year, y = total_cases, fill = disease)) +
  geom_col(position = "fill")

disease_counts <- who_disease %>%
  mutate(
    disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other') %>% 
      factor(levels = c('measles', 'other', 'mumps')) 
  ) %>%
  group_by(disease, year) %>%
  summarise(total_cases = sum(cases)) 

# plot
ggplot(disease_counts, aes(x = year, y = total_cases, fill = disease)) +
  geom_col(position = 'fill')


disease_counts <- who_disease %>%
  filter(year >= 1999) %>%  # Filter to on or later than 1999
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, region) %>%    
  summarise(total_cases = sum(cases))

# Set aesthetics so disease is the stacking variable, region is the x-axis and counts are the y
ggplot(disease_counts, aes(x = region, y = total_cases, fill = disease)) +
  geom_col(position = "fill")

#========== Chapter 2: Point data ==========


#========== Chapter 3: Single Distributions ==========


#========== Chapter 4: Comparing Distributions ==========

