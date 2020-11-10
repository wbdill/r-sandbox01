
# https://datascienceineducation.com/c07.html

library(tidyverse)
library(apaTables)
library(sjPlot)
library(readxl)
library(dataedu)
library(skimr)
library(janitor)

#data.frame()
#dplyr::summarize()
#tidyr::pivot_longer() and tidyr::pivot_wider()
#tidyr::left_join(), tidyr::right_join(), tidyr::semi_join(), and tidyr::anti_join()
#lm()
#ggplot2::ggplot()
#apaTables::apa.cor.table()
#sjPlot::tab_model()

# Pre-survey for the F15 and S16 semesters
pre_survey <- dataedu::pre_survey

# Gradebook and log-trace data for F15 and S16 semesters
course_data <- dataedu::course_data

# Log-trace data for F15 and S16 semesters - this is for time spent
course_minutes <- dataedu::course_minutes

str(pre_survey)
glimpse(pre_survey)

pre_survey  <-
  pre_survey  %>%
  # Rename the qustions something easier to work with because R is case sensitive
  # and working with variable names in mix case is prone to error
  rename(
    q1 = Q1MaincellgroupRow1,
    q2 = Q1MaincellgroupRow2,
    q3 = Q1MaincellgroupRow3,
    q4 = Q1MaincellgroupRow4,
    q5 = Q1MaincellgroupRow5,
    q6 = Q1MaincellgroupRow6,
    q7 = Q1MaincellgroupRow7,
    q8 = Q1MaincellgroupRow8,
    q9 = Q1MaincellgroupRow9,
    q10 = Q1MaincellgroupRow10
  ) %>%
  mutate_at(vars(q1:q10), list( ~ as.numeric(.)))  # Convert all question responses to numeric

reverse_scale <- function(question) {
  # Reverses the response scales for consistency
  #   Arguments:
  #     question - survey question
  #   Returns: 
  #    a numeric converted response
  # Note: even though 3 is not transformed, case_when expects a match for all
  # possible conditions, so it's best practice to label each possible input
  # and use TRUE ~ as the final statement returning NA for unexpected inputs
  x <- case_when(
    question == 1 ~ 5,
    question == 2 ~ 4,
    question == 3 ~ 3, 
    question == 4 ~ 2,
    question == 5 ~ 1,
    TRUE ~ NA_real_
  )
  x
}

# Reverse scale for questions 4 and 7
pre_survey <-
  pre_survey %>%
  mutate(q4 = reverse_scale(q4),
         q7 = reverse_scale(q7))


# Pivot the dataset from wide to long format
measure_mean <-
  pre_survey %>%
  pivot_longer(cols = q1:q10,  # Gather questions and responses
               names_to = "question",
               values_to = "response")

# Add measure variable 
measure_mean <- measure_mean %>% 
  # Here's where we make the column of question categories called "measure"
  mutate(
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",  # interest
      question %in% c("q2", "q6", "q9") ~ "uv",                # utility value
      question %in% c("q3", "q7") ~ "pc",                      # perceived competence
      TRUE ~ NA_character_)
  )



measure_mean <- measure_mean %>%
  group_by(measure) %>%  # First, we group by the new variable "measure"
  summarize(    # Here's where we compute the mean of the responses
    mean_response = mean(response, na.rm = TRUE),  # Creating a new variable to indicate the mean response for each measure
    percent_NA = mean(is.na(response))  # Creating a new variable to indicate the percent of each measure that had NAs in the response field
  )

measure_mean


#----- Course data -----
# split course section into components
course_data <- 
  course_data %>%
  # Give course subject, semester, and section their own columns
  separate(
    col = CourseSectionOrigID,
    into = c("subject", "semester", "section"),
    sep = "-",
    remove = FALSE
  )

pre_survey <-
  pre_survey %>%
  rename(student_id = opdata_username,
         course_id = opdata_CourseID)

pre_survey

str_sub("_99888_1", start = 2, end = -3)
# Re-create the variable "student_id" so that it excludes the extraneous characters
pre_survey <- pre_survey %>% 
  mutate(student_id = str_sub(student_id, start = 2, end = -3))

# Save the new variable as numeric so that R no longer thinks it is text 
pre_survey <- pre_survey %>% 
  mutate(student_id = as.numeric(student_id))

course_data <-
  course_data %>%
  rename(student_id = Bb_UserPK,
         course_id = CourseSectionOrigID)

dat <- left_join(course_data, pre_survey, by = c("student_id", "course_id"))
dat
glimpse(dat)
distinct(dat, Gradebook_Item)
distinct(dat, course_id, Gradebook_Item)

course_minutes <-
  course_minutes %>%
  rename(student_id = as.integer(Bb_UserPK),
         course_id = CourseSectionOrigID)
dat <- 
  dat %>% 
  left_join(course_minutes, 
            by = c("student_id", "course_id"))

dat <- distinct(dat, course_id, student_id, .keep_all = TRUE)
dat <- rename(dat, final_grade = FinalGradeCEMS)

dat %>% ggplot(aes(x = TimeSpent, y =  final_grade)) +
  geom_point(color = dataedu_colors("green")) +
  geom_smooth(method = "lm") +
  theme_dataedu() +
  labs(x = "Time Spent", y = "Final Grade")

m_linear <- lm(final_grade ~ TimeSpent, data = dat)

summary(m_linear)
sjPlot::tab_model(m_linear, title = "Linear Model")
apa.reg.table(m_linear, filename = "regression-table-output.doc")

survey_responses <-
  pre_survey %>%
  # Gather questions and responses
  pivot_longer(cols = q1:q10,
               names_to = "question",
               values_to = "response") %>%
  mutate(
    # Here's where we make the column of question categories
    measure = case_when(
      question %in% c("q1", "q4", "q5", "q8", "q10") ~ "int",
      question %in% c("q2", "q6", "q9") ~ "uv",
      question %in% c("q3", "q7") ~ "pc",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(student_id, measure) %>%
  # Here's where we compute the mean of the responses
  summarize(
    # Mean response for each measure
    mean_response = mean(response, na.rm = TRUE)
  ) %>%
  # Filter NA (missing) responses
  filter(!is.na(mean_response)) %>%
  pivot_wider(names_from = measure, 
              values_from = mean_response)

survey_responses

survey_responses %>% 
  apa.cor.table()  # American Psychological Association (APA) correlation table

#-----
# creating a new variable for the amount of time spent in hours
dat <- dat %>% 
  mutate(TimeSpent_hours = TimeSpent / 60)

# the same linear model as above, but with the TimeSpent variable in hours
m_linear_1 <-  lm(final_grade ~ TimeSpent_hours, data = dat)
summary(m_linear_1)
# viewing the output of the linear model
sjPlot::tab_model(m_linear_1, title = "Table 7.2")


# this is to standardize the TimeSpent variable to have a mean of 0 and a standard deviation of 1
dat <- dat %>% 
  mutate(TimeSpent_std = scale(TimeSpent))

# the same linear model as above, but with the TimeSpent variable standardized
m_linear_2 <- 
  lm(final_grade ~ TimeSpent_std, data = dat)

# viewing the output of the linear model
sjPlot::tab_model(m_linear_2, title = "Table 7.3")


# a linear model with the subject added 
# independent variables, such as TimeSpent_std and subject, can simply be separated with a plus symbol:
m_linear_3 <-  lm(final_grade ~ TimeSpent_std + subject, data = dat)
sjPlot::tab_model(m_linear_3, title = "Table 7.4")
