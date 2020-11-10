# https://datascienceineducation.com/c08.html

rm(list = ls())

# Load libraries
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(dataedu)

# download from: https://github.com/data-edu/data-science-in-education/blob/master/data/gradebooks/ExcelGradeBook.xlsx
ExcelGradeBook <- read_excel("data/ExcelGradeBook.xlsx", sheet = 1, skip = 10)

str(ExcelGradeBook)
colnames(ExcelGradeBook)

gradebook <- ExcelGradeBook  # make a copy

# look at original column names
colnames(gradebook)

gradebook <- gradebook %>%  
  clean_names() %>%                     # make the names "clean" and easy to work with in code
  remove_empty(c("rows", "cols")) %>%   # Removing rows with nothing but missing data
  select(-absent, -late)                # Remove specific columns because we don't use absent and late at this school.

str(gradebook)  # nice names, empty rows/cols gone
View(gradebook)

# Creates new data frame, selects desired variables from gradebook, and gathers all classwork scores into key/value pairs
classwork_df <- gradebook %>%
  select(name,running_average,letter_grade,homeworks,
         classworks,formative_assessments,projects,
         summative_assessments,contains("classwork_")) %>%
  mutate(across(contains("classwork_"), as.numeric)) %>%
  pivot_longer(
    cols = contains("classwork_"),
    names_to = "classwork_number",
    values_to = "score"
  ) 
summary(gradebook)
skim(gradebook)
visdat::vis_dat(gradebook)

# Bar graph for categorical variable
gradebook %>%
  ggplot(aes(x = letter_grade, fill = running_average > 90)) +
  geom_bar() +
  labs(title = "Bar Graph of Student Grades",
       x = "Letter Grades",
       y = "Count",
       fill = "A or Better") +
  scale_fill_dataedu() +
  theme_dataedu()

# Scatterplot of continuous variable
classwork_df %>%
  ggplot(aes(x = classwork_number, y = score, fill = classwork_number)) +
  geom_boxplot() +
  labs(title = "Distribution of Classwork Scores",
       x = "Classwork",
       y = "Scores") +
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(
    legend.position = "none",  # removes legend
    axis.text.x = element_text(angle = 45, hjust = 1)  # angles the x axis labels
  )


# Scatterplot between formative assessment and grades by percent to determine linear relationship
gradebook %>%
  ggplot(aes(x = formative_assessments, y = running_average)) +
  geom_point(color = dataedu_colors("green")) +
  geom_smooth(method = "lm", se = T) +
  labs(title = "Relationship Between Overall Grade and Formative Assessments",
       x = "Formative Assessment Score",
       y = "Overall Grade in Percentage") +
  theme_dataedu()

# are there any assessment score outliers?
ggplot(gradebook, aes(x = "", y = formative_assessments)) +
  geom_boxplot()

# are there any grade score outliers?
ggplot(gradebook, aes(x = "", y = running_average)) +
  geom_boxplot()

cor(gradebook$formative_assessments, gradebook$running_average)

linear_mod <- lm(running_average ~ formative_assessments, data = gradebook)
summary(linear_mod)

sjPlot::tab_model(linear_mod, title = "Table 3.14")
