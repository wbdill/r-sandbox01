
# https://datascienceineducation.com/c09.html

# 9 Walkthrough 3: Using School-Level Aggregate Data to Illuminate Educational Inequities

rm(list = ls())
# functions
#dplyr::mutate_at()
#readRDS()
#purrr::map and purrr::map_df()
#purrr::set_names()
#dplyr::slice()

library(tidyverse)
library(here)
library(janitor)
library(dataedu)

#install.packages("tabulizer")
#library(rJava)
#library(tabulizer)

race_pdf <- readRDS(here("data", "race_pdf.Rds"))
typeof(race_pdf)

?map
?as_tibble
?name_repair

race_df <-
  race_pdf %>%
  map(~ as_tibble(.x, .name_repair = "unique")) %>%   # Turn each page into a tibble
  map_df(~ .)
  map_df(~ slice(.,-1:-2)) %>%  # Make data frame and remove unnecessary rows
  set_names(  # Use descriptive column names
    c(
      "school_group",
      "school_name",
      "grade",
      "na_num", # Native American number of students
      "na_pct", # Native American percentage of students
      "aa_num", # African American number of students
      "aa_pct", # African American percentage
      "as_num", # Asian number of students
      "as_pct", # Asian percentage
      "hi_num", # Hispanic number of students
      "hi_pct", # Hispanic percentage
      "wh_num", # White number of students
      "wh_pct", # White percentage
      "pi_pct", # Pacific Islander percentage
      "blank_col",
      "tot" # Total number of students (from the Race PDF)
    )
  )

  
race_df2 <-
  race_df %>%
  select(-school_group, -grade, -pi_pct, -blank_col) %>%  # Remove unnecessary columns
  filter(str_detect(school_name, "Total"), school_name != "Grand Total") %>%  # Remove unnecessary columns
  mutate(school_name = str_replace(school_name, "Total", "")) %>%  # Clean up school names
  mutate_if(is.character, trimws) %>%    # Remove white space
  #mutate_at(vars(contains("pct")), list( ~ as.numeric(str_replace(., "%", "")) / 100))  # Turn percentage columns into numeric and decimal format
  #mutate(across(contains("pct"),    list( ~ as.numeric(str_replace(. , "%", "")) / 100) ))
  mutate(across(contains("pct"),           ~ as.numeric(str_replace(.x, "%", "")) / 100  ))

?across

frpl_pdf <- readRDS(here("data", "frpl_pdf.Rds"))

frpl_df <-
  frpl_pdf %>%
  map(~ as_tibble(.x, .name_repair = "unique")) %>%   # Turn each page into a tibble
  map_df( ~ slice(.,-1)) %>%  # Make data frame and remove unnecessary rows
  set_names(  # Use descriptive column names
    c(
      "school_name",
      "not_eligible_num", # Number of non-eligible students,
      "reduce_num", # Number of students receiving reduced price lunch
      "free_num",   # Number of students receiving free lunch
      "frpl_num",  # Total number of students (from the FRPL PDF)
      "frpl_pct" # Free/reduced price lunch percentage
    )
  )

frpl_df2 <-
  frpl_df %>%
  filter(
    school_name != "",  # Remove blanks
    !school_name %in% c(  # Filter out the rows in this list
      "ELM K_08",
      "Mid Schl",
      "High Schl",
      "Alt HS",
      "Spec Ed Total",
      "Cont Alt Total",
      "Hospital Sites Total",
      "Dist Total"
    )
  ) %>%
  mutate(frpl_pct = as.numeric(str_replace(frpl_pct, "%", "")) / 100)    # Turn percentage columns into numeric and decimal format

# create full dataset, joined by school name
joined_df <-
  left_join(race_df2, frpl_df2, by = c("school_name")) %>%
  mutate(across(2:17, as.numeric))
  #mutate_at(2:17, as.numeric)

str(joined_df)

district_merged_df <-
  joined_df %>%
  mutate(  # Calculate high poverty numbers
    hi_povnum = case_when(frpl_pct > .75 ~ hi_num),
    aa_povnum = case_when(frpl_pct > .75 ~ aa_num),
    wh_povnum = case_when(frpl_pct > .75 ~ wh_num),
    as_povnum = case_when(frpl_pct > .75 ~ as_num),
    na_povnum = case_when(frpl_pct > .75 ~ na_num)
  ) %>%
  adorn_totals() %>%
  mutate(  # Create percentage by demographic
  na_pct = na_num / tot,
  aa_pct = aa_num / tot,
  as_pct = as_num / tot,
  hi_pct = hi_num / tot,
  wh_pct = wh_num / tot,
  frpl_pct = (free_num + reduce_num) / frpl_num,
  # Create percentage by demographic and poverty
  hi_povsch = hi_povnum / hi_num[which(school_name == "Total")],  # Total row created by adorn_totals()
  aa_povsch = aa_povnum / aa_num[which(school_name == "Total")],
  as_povsch = as_povnum / as_num[which(school_name == "Total")],
  wh_povsch = wh_povnum / wh_num[which(school_name == "Total")],
  na_povsch = na_povnum / na_num[which(school_name == "Total")]
  )

# tidy for graphing later
district_tidy_df <-
  district_merged_df %>%
  pivot_longer(
    cols = -matches("school_name"),
    names_to = "category",
    values_to = "value"
  )

# 9.8 View Data
district_tidy_df %>%
  # Filter for Total rows, since we want district-level information
  filter(school_name == "Total",
         str_detect(category, "pct"),
         category != "frpl_pct") %>%
  ggplot(aes(x = reorder(category, -value), y = value)) +    # Reordering x-axis so bars appear by descending value
  geom_bar(stat = "identity", aes(fill = category)) +
  labs(title = "Percentage of Population by Subgroup",
       x = "Subgroup",
       y = "Percentage of Population") +
  scale_x_discrete(    # Make labels more readable
    labels = c(
      "aa_pct" = "Black",
      "wh_pct" = "White",
      "hi_pct" = "Hispanic",
      "as_pct" = "Asian",
      "na_pct" = "Native Am."
    )
  ) +
  scale_y_continuous(labels = scales::percent) +   # Makes labels present as percentages
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")


district_tidy_df %>%
  filter(school_name == "Total",
         str_detect(category, "povsch")) %>%
  ggplot(aes(x = reorder(category,-value), y = value)) +
  geom_bar(stat = "identity", aes(fill = factor(category))) +
  labs(title = "Distribution of Subgroups in High Poverty Schools",
       x = "Subgroup",
       y = "Percentage in High Poverty Schools") +
  scale_x_discrete(
    labels = c(
      "aa_povsch" = "Black",
      "wh_povsch" = "White",
      "hi_povsch" = "Hispanic",
      "as_povsch" = "Asian",
      "na_povsch" = "Native Am."
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")


district_merged_df %>%
  filter(school_name != "Total") %>%
  ggplot(aes(x = wh_pct, y = frpl_pct)) +
  geom_point(color = dataedu_colors("green")) +
  geom_smooth(method = "lm") +
  labs(title = "FRPL Percentage vs. White Percentage",
       x = "White Percentage",
       y = "FRPL Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme_dataedu() +
  theme(legend.position = "none")

dist_merge <- district_merged_df %>%   filter(school_name != "Total")

mod <- lm(frpl_pct ~ wh_pct, data = district_merged_df_xtot)
summary(mod)
