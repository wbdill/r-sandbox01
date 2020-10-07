# Cleaning Data in R
# https://campus.datacamp.com/courses/cleaning-data-in-r/
# 2020-09-29
install.packages("visdat")
library(visdat)
install.packages("stringdist")  # determine edit distance
library(stringdist)
install.packages("fuzzyjoin")   # do fuzzy join based on edit distance
library(fuzzyjoin)
install.packages("reclin")  # record linkage -generates matching pairs between 2 data sets
library(reclin)

library(tidyverse)
#library(dplyr)
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

# 2.1) trim whitespace and capilization cleaning
count(sfo_survey, dest_size)
count(sfo_survey, cleanliness)
sfo_survey <- sfo_survey %>%
  mutate(dest_size_trimmed = str_trim(dest_size),  # dest_size_trimmed: dest_size without whitespace
         cleanliness_lower = str_to_lower(cleanliness))  # cleanliness_lower: cleanliness converted to lowercase
count(sfo_survey, dest_size_trimmed)
count(sfo_survey, cleanliness_lower)

# 2.2) categorical cleaning (factor collapsing)
library(forcats)
?fct_collapse
count(sfo_survey, dest_region)
europe_categories <- c("EU", "Europ", "eur")  # Categories to map to Europe
sfo_survey %>% # Add a new col dest_region_collapsed
  mutate(dest_region_collapsed = fct_collapse(dest_region,  
                                        Europe = europe_categories)) %>%  # Map all categories in europe_categories to Europe
  count(dest_region_collapsed)   # Count categories of dest_region_collapsed

# 2.3) text data cleaning ---
# 2.3.1)  formatting inconsistency    ### NOTE: phone isn't in the dataset ****
sfo_survey %>%
  filter(str_detect(phone, "-")) # Filter for rows with "-" in the phone column


# 2.3.2) info inconsistency (ex: sub parts missing like area code in phone number)
sfo_survey %>%
  filter(str_detect(phone, fixed("(")) | str_detect(phone, fixed(")")))  # Filter for rows with "(" or ")" in the phone column

phone_no_parens <- sfo_survey$phone %>%   # Remove parentheses from phone column
  str_remove(fixed("(")) %>%  # Remove "("s
  str_remove(fixed(")"))  # Remove ")"s

# Add phone_no_parens as column
sfo_survey %>%
  mutate(phone_no_parens = phone_no_parens,
         phone_clean = str_replace_all(phone_no_parens, "-", " ")) # Replace all hyphens in phone_no_parens with spaces


# 2.3.3) invalid data (ex longer or shorter than allowed)
sfo_survey %>%
  str_length(phone != 12)

#----- Ch 3 - Advanced Data Problems -----

library(lubridate)
# accounts <- read_rds("data/ch3_1_accounts.rds")
accounts <- read_tsv("data/accounts.tsv")
account_offices <- read_tsv("data/account_offices.tsv")
accounts$date_opened <- as.Date(accounts$date_opened)

# 3.1) Uniformity

# 3.1.1 different units
head(accounts)

formats <- c("%Y-%m-%d", "%B %d, %Y")  # Define the date formats
accounts %>%
  mutate(date_opened_clean = lubridate::parse_date_time(date_opened, formats))  # Convert dates to the same format

accounts %>%
  ggplot(aes(x = date_opened, y = total)) +  # Scatter plot of opening date and total amount
  geom_point()

accounts %>% 
  left_join(account_offices, by = "id") %>% 
  mutate(total_usd = ifelse(office == "Tokyo", total / 104, total)) %>% 
  ggplot(aes(x = date_opened, y = total_usd)) +  # Scatter plot of opening date and total amount
  geom_point()

# 3.2) Cross field validation
library(lubridate)
date_difference <- as.Date("2015-09-04") %--% today()
date_difference
as.numeric(date_difference, "years")

accounts %>% 
  mutate(theoretical_total = fund_A + fund_B + fund_C) %>% 
  filter(theoretical_total != total)

# this exampe will soon be moot as this dataset ages b/c acct_age will eventually be of for all records.
accounts %>% 
  mutate(theoretical_age = floor(as.numeric(as.Date(date_opened) %--% today(), "years" ) ) ) %>% 
  filter(acct_age != theoretical_age)

# alternate date/time diff:
time_length(difftime(coalesce(end_date, today()), start_date), "years")

# 3.3) Missing data  Missing completely at random (MCAR), Missing at random (MAR), Missing not at random (MNAR)
# simple approach: 1 drop data or 2 impute wil statistical measures (mean, median) or domain knowledge
# complex approach: 1 impute using algorithms or 2 imput with machine learning models
library(visdat)
vis_miss(sfo_survey)

# filter(!is.na(col1), !is.na(col2))  # remove rows with missing values
# mutate(col1_filled = ifelse(is.na(col1), mean(col1, na.rm = TRUE), col1))  # replace missing with mean
accounts <- read_tsv("data/accounts_ch3.tsv")
accounts %>% 
  vis_miss()

accounts %>%
  mutate(missing_inv = is.na(inv_amount)) %>%  # missing_inv: Is inv_amount missing?
  group_by(missing_inv) %>%  # Group by missing_inv
  summarise(avg_age = mean(age))  # Calculate mean age for each missing_inv group

accounts %>%
  arrange(age) %>%  # Sort by age and visualize missing vals
  vis_miss()


#----- Ch 4 - Record Linkage -----
# when traditional joins can't be used because of no exact match
library(stringdist)
library(fuzzyjoin)

fodors <- read_rds("data/fodors.rds")
zagat <- read_rds("data/zagat.rds")   # this dataset already has clean city names
head(fodors)
head(zagat)
cities <- tribble(
  ~city_actual,
  "new york",
  "los angeles",
  "atlanta",
  "san fransisco",
  "las vegas"
)

### edit distance (add char, delete char, transpose 2 chars, substitute char)
### types of edit distance: Damerau-Levenshtein, Levenshtein (no transpose), LCS (only ins and del)

stringdist("las angelos", "los angeles", method = "dl")  # dl = Damerau-Levenshtein
stringdist("las angelos", "los angeles", method = "lcs")
stringdist("las angelos", "los angeles", method = "jaccard")

zagat %>%
  count(city)

zagat %>%   # Join zagat and cities and look at results
  stringdist_left_join(cities, by = c("city" = "city_actual")) %>%  # Left join based on stringdist using city and city_actual cols
  select(name, city, city_actual)  # Select the name, city, and city_actual cols

### record linkage
library(reclin)

pair_blocking(dfA, dfB, blocking_var = "foo") %>% 
compare_pairs(by = "col", default_comparator = lcs())

pair_blocking(zagat, fodors)  #165,230 pairs
pair_blocking(zagat, fodors, blocking_var = "city") #40,532

pair_blocking(zagat, fodors, blocking_var = "city") %>%    # Generate pairs
  compare_pairs(by = "name", default_comparator = lcs())   # Compare pairs by name using lcs()

pair_blocking(zagat, fodors, blocking_var = "city") %>%    # Generate pairs
  compare_pairs(by = c("name", "phone", "addr"), default_comparator = jaro_winkler())   # Compare pairs by name, phone, addr via jaro_winkler()

# score_simsum()  # simple summary of all scores into a new col "simsum"
# score_problink()  # weighted score
# select_n_to_m()   # mathces pairs with highest scores (record from A matches at most 1 record from B)

# Create pairs
pair_blocking(zagat, fodors, blocking_var = "city") %>%
  compare_pairs(by = "name", default_comparator = jaro_winkler()) %>%  # Compare pairs
  score_problink() %>%   # Score pairs based on probabilistic weights
  select_n_to_m() %>%    # select pairs
  link()                 # link data
