# https://datascienceineducation.com/c10.html

#install.packages("dataedu")
install.packages("remotes")
remotes::install_github("data-edu/dataedu")
install.packages("here")

library(tidyverse)

library(dataedu)
library(lubridate)
library(here)

d1 <- read_csv(here::here("data", "bchildcountandedenvironments2012.txt"), skip = 4)
d1


# Get filenames from the data folder 
filenames <- list.files(path = here::here("data", "longitudinal_data"), full.names = TRUE)

# A list of filenames and paths
filenames

all_files <- filenames %>% 
  map(., ~ read_csv(., skip = 4))


# Variables of first and second dataset don't match
identical(names(all_files[[1]]), names(all_files[[2]]))

# Variables of third and second files match
identical(names(all_files[[2]]), names(all_files[[3]]))

all_files %>% 
  map(ncol)  # apply the function ncol to each element of all_files


# Look at the first 10 column names of 2016
names(all_files[[5]])[1:10]

filenames[[1]]
filenames[[5]]

all_files[[5]] <-
  # Skip the first 3 lines instead of the first 4
  read_csv(filenames[[5]], skip = 3)


all_files[[1]] %>%
  select(
    Year,
    contains("State", ignore.case = FALSE),
    contains("SEA", ignore.case = FALSE),
    contains("male")
  ) 

#-----
# build the function
pick_vars <-
  function(df) {
    df %>%
      select_at(vars(
        Year,
        contains("State", ignore.case = FALSE),
        contains("SEA", ignore.case = FALSE),
        contains("male")
      ))
  }

# use the function with `all_files`
all_files <-
  all_files %>%
  map(pick_vars)

# check variable names
all_files %>% 
  map(names)

child_counts <- all_files %>%
  bind_rows()  # combine all datasets in `all_files`
str(child_counts)


child_counts %>%
  count(`SEA Disability Category`)  # count number of times the category appears in the dataset

child_counts <-
  child_counts %>%
  filter(
    `SEA Disability Category` == "All Disabilities",  # filter all but the All Disabilities category
    `SEA Education Environment` %in% c("Total, Age 3-5", "Total, Age 6-21")  # filter all but the age totals
  ) 

child_counts <-
  child_counts %>%
  rename(
    # change these columns to more convenient names
    year = Year,
    state = "State Name",
    age = "SEA Education Environment",
    disability = "SEA Disability Category",
    f_3_5 = "Female Age 3 to 5",
    m_3_5 = "Male Age 3 to 5",
    f_6_21 = "Female Age 6 to 21",
    m_6_21 = "Male Age 6 to 21"
  )

child_counts %>%
  count(state) %>%
  head()

child_counts <-
  child_counts %>%
  mutate(state = tolower(state)) 

child_counts <-
  child_counts %>%
  pivot_longer(cols = f_3_5:m_6_21, 
               names_to = "gender", 
               values_to = "total")

child_counts <- 
  child_counts %>%
  mutate(
    gender = case_when(
      gender == "f_3_5" ~ "f",
      gender == "m_3_5" ~ "m",
      gender == "f_6_21" ~ "f",
      gender == "m_6_21" ~ "m",
      TRUE ~ as.character(gender)
    )
  )

child_counts <-
  child_counts %>%
  mutate(total = as.numeric(total))

child_counts

child_counts <-
  child_counts %>%
  mutate(year = lubridate::ymd(year, truncated = 2))

child_counts <-
  child_counts %>%
  filter(!is.na(total)) 
  
child_counts %>%
  arrange(year, state, gender)

#----- 10.8.1 Visualize
child_counts %>%
  group_by(state) %>%
  summarize(mean_count = mean(total)) %>%
  # which six states have the highest mean count of students with disabilities
  top_n(6, mean_count)

high_count <-
  child_counts %>%
  filter(state %in% c("california", "florida", "new york", "pennsylvania", "texas"))


high_count %>%
  filter(gender == "f", age == "Total, Age 6-21") %>%
  ggplot(aes(x = year, y = total, color = state)) +
  geom_freqpoly(stat = "identity", size = 1) +
  labs(title = "Count of Female Students in Special Education Over Time",
       subtitle = "Ages 6-21") +
  scale_color_dataedu() +
  theme_dataedu()

high_count %>%
  group_by(year, state) %>%
  summarize(n = sum(total)) %>%
  ggplot(aes(x = state, y = n)) +
  geom_boxplot(fill = dataedu_colors("yellow")) +
  labs(title = "Median Students with Disabilities Count",
       subtitle = "All ages and genders, 2012-2017") +
  theme_dataedu() 


high_count %>%
  group_by(year, state, gender) %>%
  summarize(total = sum(total)) %>%
  # Create new columns for male and female student counts
  pivot_wider(names_from = gender, 
              values_from = total) %>% 
  # Create a new ratio column
  mutate(ratio = m / f) %>%
  ggplot(aes(x = year, y = ratio, color = state)) +
  geom_freqpoly(stat = "identity", size = 1) +
  scale_y_continuous(limits = c(1.5, 2.5)) +
  labs(title = "Male Student to Female Student Ratio Over Time",
       subtitle = "Ages 6-21") +
  scale_color_dataedu() +
  theme_dataedu()



