# 2021-2022 TN ACT scores
library(tidyverse)
library(stats)
library(dplyr)
library(readxl)
#detach("package:stats", unload=TRUE)

folder <- "D:/opendata/TN/TDOE/"

dist <- readxl::read_xlsx(paste0(folder, "2021-22_ACT_district_suppressed.xlsx"), na = c("*", "**")) %>% 
  janitor::clean_names() %>% mutate(across(matches("^average"), as.numeric)) %>% 
  mutate(across(matches("^percent"), as.numeric)) %>%
  mutate(across(matches("^number"), as.integer))

school <- readxl::read_xlsx(paste0(folder, "2021-22_ACT_school_suppressed.xlsx"), na = c("*", "**")) %>% 
  janitor::clean_names() %>% mutate(across(matches("^average"), as.numeric)) %>% 
  mutate(across(matches("^percent"), as.numeric)) %>%
  mutate(across(matches("^number"), as.integer))


str(dist)
str(school)

school %>% filter(district_name ==  "Williamson County", subgroup == "All Students") %>% View()

dist %>% filter(subgroup == "All Students") %>% 
  select(district_name, average_composite_score) %>% 
  ggplot(aes(x = reorder(district_name, average_composite_score), y = average_composite_score)) +
  geom_col() +
  coord_flip() +
  labs(title = "2021-22 ACT Scores - TN",
       x = "School District",
       y = "Average Composite ACT Score",
       caption = "@bdill\nData: TDOE")

school %>% filter(subgroup == "All Students", district_name == "Williamson County") %>% 
  mutate(school_name2 = stringr::str_remove(school_name, " High School")) %>%
  select(district_name, school_name2, average_composite_score) %>% 
  ggplot(aes(x = reorder(school_name2, -average_composite_score), y = average_composite_score)) +
  geom_col(fill = "#666699") +
  geom_hline(yintercept = 20.9) +
  scale_y_continuous(breaks = seq(0, 35, 2)) +
  theme_minimal() +
  #coord_flip() +
  labs(title = "2021-22 ACT Scores - Williamson County, TN",
       x = "School",
       y = "Average Composite ACT Score",
       caption = "@bdill\nData: TDOE")
?str_remove


school %>% filter(subgroup == "All Students", district_name == "Williamson County") %>% 
  mutate(school_name2 = stringr::str_remove(school_name, " High School")) %>% 
  select(school_name, school_name2)

  #select(district_name, school_name, average_composite_score) 
  select(district_name, school_name2 = stringr::str_remove(school_name, " High School"), average_composite_score) 
