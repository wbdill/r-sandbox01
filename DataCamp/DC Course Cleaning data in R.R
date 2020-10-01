# Cleaning Data in R
# https://campus.datacamp.com/courses/cleaning-data-in-r/
# 2020-09-29
library(dplyr)
library(stringr)
library(assertive)

rm(list = ls())

accounts <- read_rds("data/ch3_1_accounts.rds")
sfo_survey <- read_rds("data/sfo_survey_ch2_1.rds")
bike_share_rides <- read_rds("data/bike_share_rides_ch1_1.rds")
fodors <- read_rds("data/fodors.rds")
zagat <- read_rds("data/zagat.rds")

#----- Ch 1 - Common Data Problems -----

# logical check   assert                 convert
# is.character()  assert_is_character()  as.character()
# is.numeric()    assert_is_numeric()    as.numeric()
# is.logical()    assert_is_logical()    as.logical()
# is.factor()     assert_is_factor()     as.factor()
# is.Date()       assert_is_date()       as.Date()

# remove commas in "numbers" in character columns
clean_col <- stringr::str_remove(sales$revenue, ",")

glimpse(bike_share_rides)
summary(bike_share_rides$user_birth_year)

bike_share_rides <- bike_share_rides %>%
  mutate(user_birth_year_fct = as.factor(user_birth_year))
assertive::assert_is_factor(bike_share_rides$user_birth_year_fct)  # Assert user_birth_year_fct is a factor
summary(bike_share_rides$user_birth_year_fct)  # Summary of user_birth_year_fct

bike_share_rides <- bike_share_rides %>%
  mutate(duration_trimmed = str_remove(duration, "minutes"),
         duration_mins = as.numeric(duration_trimmed))
glimpse(bike_share_rides)
assert_is_numeric(bike_share_rides$duration_mins)
mean(bike_share_rides$duration_mins)

# ----- range constraints
assertive::assert_all_are_in_closed_range(bike_share_rides$duration_mins, lower = 0, upper = 999)
# replace out of range values
replace(col, condition, replacement)
bike_share_rides %>% 
  mutate(duration_missing = replace(duration, duration > 999, NA))

# assertive::assert_all_are_in_past()
# filter(date_col <= lubridate::today())
bike_share_rides$duration_mins

str(bike_share_rides)
library(lubridate)
bike_share_rides <- bike_share_rides %>%
  mutate(date = as.Date(date))

assert_all_are_in_past(bike_share_rides$date)  # Make sure all dates are in the past
bike_share_rides_past <- bike_share_rides %>% # Filter for rides that occurred before or on today's date
  filter(date < today())
assert_all_are_in_past(bike_share_rides_past$date)  # Make sure all dates from bike_share_rides_past are in the past

# uniqueness/duplicates
# full duplicate
duplicated(df) 
sum(duplicated(df)) # number of full duplicates
df_unique <- distinct(df)  # dedupe full dupe

# partial dupes
dupe_ids <- df %>% 
  count(col1, col2) %>% 
  filter(n > 1)

df %>% 
  filter(col1 %in% dupe_ids$col1, col2 %in% dupeids$col2)  # show the (partial) dupe rows

# dedupe partial dupes
df %>% 
  distinct(col1, col2, .keep_all = TRUE)  # keep all keeps all columns, not just col1, col2
# note that the non-duplicated cols

bike_share_rides %>%
  group_by(ride_id, date) %>%  # Group by ride_id and date
  mutate(duration_min_avg = mean(duration_min) ) %>%  # Add duration_min_avg column
  distinct(ride_id, date, .keep_all = TRUE) %>%  # Remove duplicates based on ride_id and date, keep all cols
  select(-duration_min)  # Remove duration_min column

#----- Ch 2 - Categorical and Text Data -----
sfo_survey <- read_rds("data/sfo_survey_ch2_1.rds")
sfo_survey %>% 
  count(dest_size)


#----- Ch 3 - Advanced Data Problems -----



#----- Ch 4 - Record Linkage -----
