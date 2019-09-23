# https://campus.datacamp.com/courses/visualization-best-practices-in-r/proportions-of-a-whole?ex=1
library(data.table)
library(tidyverse)

who_disease <- fread("C:/GitHub/r-sandbox01/DataCamp/data/who_disease.csv")
glimpse(who_disease)
#========== Chapter 1 ==========
ggplot(who_disease, aes(x=region)) +
  geom_bar()


# filter data to AMR region. 
amr_region <- who_disease %>%
  filter(region == "AMR")

# map x to year and y to cases.  # lower alpha to 0.5 to see overlap. 
ggplot(amr_region, aes(x=year, y=cases)) + 
  geom_point(alpha = 0.5)


disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  geom_col() +
  coord_polar(theta = 'y')

#========== Chapter 2 ==========


#========== Chapter 3 ==========

#========== Chapter 4 ==========