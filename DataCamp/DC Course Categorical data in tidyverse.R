# https://www.datacamp.com/courses/categorical-data-in-the-tidyverse
# https://cran.r-project.org/web/packages/fivethirtyeight/vignettes/fivethirtyeight.html
rm(list = ls())
install.packages("fivethirtyeight")
library(fivethirtyeight)
library(data.table)
library(tidyverse)
library(forcats)
# smc_with_js.csv flying-etiquette.csv



multiple_choice_responses <- fread("C:/GitHub/r-sandbox01/DataCamp/data/smc_with_js.csv")
flying_etiquette <- fread("C:/GitHub/r-sandbox01/DataCamp/data/flying-etiquette.csv")

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
  summarize(mean_age = mean(Age)) %>%
  ggplot(aes(x = EmployerIndustry, y = mean_age)) + 
  geom_point() + 
  coord_flip()
  
#========== Ch 2: Manipulating Factor Variables ==========

multiple_choice_responses$WorkInternalVsExternalTools <- as.factor(multiple_choice_responses$WorkInternalVsExternalTools)
levels(multiple_choice_responses$WorkInternalVsExternalTools)

mc_responses_reordered <- multiple_choice_responses %>%
    mutate(WorkInternalVsExternalTools = fct_relevel(WorkInternalVsExternalTools,
                                                     "Entirely internal", 
                                                     "More internal than external",
                                                     "Approximately half internal and half external",
                                                     "More external than internal", 
                                                     "Entirely external",
                                                     "Do not know"))
levels(multiple_choice_responses$WorkInternalVsExternalTools)
levels(mc_responses_reordered$WorkInternalVsExternalTools)

ggplot(mc_responses_reordered, aes(x=WorkInternalVsExternalTools)) +
  geom_bar() +
  coord_flip()

multiple_choice_responses %>%
  # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
  mutate(FormalEducation = fct_relevel(FormalEducation, "I did not complete any formal education past high school", "Some college/university study without earning a bachelor's degree")) %>%
  # Move "I prefer not to answer" to be the last level.
  mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
  # Move "Doctoral degree" to be after the 5th level
  mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
  # Examine the new level order
  pull(FormalEducation) %>%
  levels()

ggplot(multiple_choice_responses, aes(FormalEducation)) + 
  geom_bar()

multiple_choice_responses %>%
  # rename the appropriate levels to "High school" and "Some college"
  mutate(FormalEducation = fct_recode(FormalEducation,
                               "High school" = "I did not complete any formal education past high school", 
                               "Some college" = "Some college/university study without earning a bachelor's degree")) %>%
  ggplot(aes(x = FormalEducation)) + 
  geom_bar()


# fct_collapse - collapses multiple levels into one
# fct_other keep/drop, fct_lump n= or prop=.05
multiple_choice_responses %>%
  # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
  mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect, 
                           "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"), 
                           "Researcher" = "Scientist/Researcher", 
                           "Data Analyst/Scientist/Engineer" = c("DBA/Database Engineer", "Data Scientist", 
                                                                 "Business Analyst", "Data Analyst", 
                                                                 "Data Miner", "Predictive Modeler"))) %>%
  # Keep all the new titles and turn every other title into "Other"
  mutate(grouped_titles = fct_other(grouped_titles, 
                           keep = c("Computer Scientist", 
                                   "Researcher", 
                                   "Data Analyst/Scientist/Engineer"))) %>% 
  count(grouped_titles)  # Get a count of the grouped titles

multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
           # create ml_method, which lumps all those with less than 5% of people into "Other"
           mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = .05)) %>%
           # count the frequency of your new variable, sorted in descending order
           count(ml_method, sort = TRUE)

multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n=5, other_level = "other method")) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort=TRUE)

#========== Ch 3: Creating Factor Variables ==========
learning_platform_usefulness <- multiple_choice_responses %>%
  select(contains("LearningPlatformUsefulness")) %>%  # select columns with LearningPlatformUsefulness in title
  gather(learning_platform, usefulness) %>%
  filter(!is.na(usefulness)) %>%   # remove rows where usefulness is NA
  mutate(learning_platform = str_remove(learning_platform, "LearningPlatformUsefulness"))    # remove "LearningPlatformUsefulness" from each string in learning_platform 

perc_useful_platform <- learning_platform_usefulness %>%
  # change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness) %>%
  add_count(learning_platform, wt = n) %>%   # add_count to add group count to each record
  mutate(perc = n / nn)

ggplot(perc_useful_platform, aes(x = usefulness, y = perc, group = learning_platform)) +
   geom_line() +
  facet_wrap(~learning_platform)

usefulness_by_platform <- learning_platform_usefulness %>%
  mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1)) %>%   # If usefulness is "Not Useful", make 0, else 1 
  group_by(learning_platform) %>%  # Group by learning platform 
  summarize(avg_usefulness = mean(usefulness))  # Summarize the mean usefulness for each platform

ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") +
  scale_y_continuous(labels = scales::percent)

usefulness_by_platform %>%
  # reorder learning_platform by avg_usefulness
  mutate(learning_platform = fct_reorder(learning_platform, avg_usefulness)) %>%
  # reverse the order of learning_platform
  mutate(learning_platform = fct_rev(learning_platform)) %>%
  ggplot(aes(x = learning_platform, y = avg_usefulness)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
  scale_y_continuous(labels = scales::percent)


# dplyer::case_when
multiple_choice_responses %>%
  filter(between(Age, 10, 90)) %>%   # Filter for rows where Age is between 10 and 90
  # Create the generation variable based on age
  mutate(generation = case_when(
    between(Age, 10, 22) ~ "Gen Z", 
    between(Age, 23, 37) ~ "Gen Y", 
    between(Age, 38, 52) ~ "Gen X", 
    between(Age, 53, 71) ~ "Baby Boomer", 
    between(Age, 72, 90) ~ "Silent"
  )) %>%
  count(generation)

# case_when multiple columns

multiple_choice_responses %>%
  # Filter out people who selected Data Scientist as their Job Title
  filter(CurrentJobTitleSelect != "Data Scientist") %>%
  # Create a new variable, job_identity
  mutate(job_identity = case_when(
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS analysts", 
    CurrentJobTitleSelect == "Data Analyst" & 
      DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst", 
    CurrentJobTitleSelect != "Data Analyst" & 
      DataScienceIdentitySelect == "Yes" ~ "DS non-analysts", 
    TRUE ~ "NDS non analysts")) %>%
  group_by(job_identity) %>%
  summarize(avg_js = mean(JobSatisfaction, na.rm = TRUE))

#========== Ch 4: Case Study on Flight Etiquette ==========


