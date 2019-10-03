# https://www.datacamp.com/courses/categorical-data-in-the-tidyverse
rm(list = ls())
install.packages("fivethirtyeight")
library(fivethirtyeight)
library(data.table)
library(tidyverse)
library(forcats)
# smc_with_js.csv flying-etiquette.csv



multiple_choice_responses <- fread("C:/GitHub/r-sandbox01/DataCamp/data/smc_with_js.csv")
flying <- fread("C:/GitHub/r-sandbox01/DataCamp/data/flying-etiquette.csv")

#========== Ch 1: Introduction to Factor Variables ==========
# categorical and ordinal are typically factors
is.factor(multiple_choice_responses$CurrentJobTitleSelect)
glimpse(multiple_choice_responses)

responses_as_factors <- multiple_choice_responses %>%
  mutate_if(is.character, as.factor)

number_of_levels <- responses_as_factors %>%
  summarise_all(nlevels) %>%  # apply the function nlevels to each column
  gather(variable, num_levels)  # change the dataset from wide to long


# Select the 3 rows with the highest number of levels
number_of_levels %>%
  top_n(3, num_levels)

number_of_levels %>%
  filter(variable == 'CurrentJobTitleSelect') %>%  # filter for where the column called variable equals CurrentJobTitleSelect
  pull(num_levels)

responses_as_factors %>%
  pull(CurrentJobTitleSelect) %>%   # pull CurrentJobTitleSelect
  levels()   # get the values of the levels

# fct_reorder, fct_infreq, fct_rev part of forcats pkg
ggplot(multiple_choice_responses, aes(EmployerIndustry)) + 
  geom_bar() + 
  coord_flip()

ggplot(multiple_choice_responses, aes(fct_rev(fct_infreq(EmployerIndustry)))) + 
  geom_bar() + 
  coord_flip()

multiple_choice_responses %>%
  filter(!is.na(Age) & !is.na(EmployerIndustry) )    # remove NAs
  group_by(EmployerIndustry) %>%
  summarize(mean_age = mean(Age))) %>%
  mutate(EmployerIndustry = fct_reorder(EmployerIndustry, mean_age)) %>%
  ggplot(aes(x = EmployerIndustry, y = mean_age)) + 
  geom_point() + 
  coord_flip()
#========== Ch 2: Manipulating Factor Variables ==========

#========== Ch 3: Creating Factor Variables ==========

#========== Ch 4: Case Study on Flight Etiquette ==========