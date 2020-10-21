# Williamson County TN

library(readxl)
library(tidyverse)

d <- read_xlsx("data/TN-Public-Dataset-County-New.XLSX", col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
?read_xlsx

wmson <- d %>% 
  filter(DATE >= "2020-07-01") %>% 
  filter(COUNTY == "Williamson") 
  
state <- d %>% 
  group_by(DATE) %>% 
  summarise(NEW_TESTS = sum(NEW_TESTS),
            NEW_POS_TESTS = sum(NEW_POS_TESTS),
            TOTAL_HOSPITALIZED = sum(TOTAL_HOSPITALIZED),
            TOTAL_DEATHS = sum(TOTAL_DEATHS))

my_blue <- "#aaaaff"
my_labs <- labs(title = "TITLE HERE",
             subtitle = "Williamson County, TN (2020)",
             caption = "@bdill Source: https://www.tn.gov/health/cedep/ncov/data/downloadable-datasets.html (County New)")

#----- New TESTS -----
wmson %>% 
  filter(DATE >= "2020-04-01") %>% 
  mutate(NEW_TESTS_07da = zoo::rollmean(NEW_TESTS, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, NEW_TESTS)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, NEW_TESTS_07da), color = "red", size = 1) +
  my_labs +
  labs(title = "New Tests")
  ggsave("output/TN_Williamson_new_tests.png", width = 8, height = 5, dpi = 200)

#----- New CASES -----
wmson %>% 
  filter(DATE >= "2020-04-01") %>% 
  mutate(NEW_CASES_07da = zoo::rollmean(NEW_CASES, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, NEW_CASES)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, NEW_CASES_07da), color = "red", size = 1) +
  my_labs +
  labs(title = "New Cases")
  ggsave("output/TN_Williamson_new_cases.png", width = 8, height = 5, dpi = 200)

#----- Positivity -----
wmson %>% 
  filter(DATE >= "2020-04-01") %>% 
  mutate(test_positivity = NEW_POS_TESTS * 100 / NEW_TESTS,
         test_positivity_07da = zoo::rollmean(test_positivity, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, test_positivity)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, test_positivity_07da), color = "red", size = 1) +
  my_labs +
  labs(title = "New Test Positivity")
  
  ggsave("output/TN_Williamson_positivity.png", width = 8, height = 5, dpi = 200)

#----- TOTAL_HOSPITALIZED -----
wmson %>% 
  filter(DATE >= "2020-04-03") %>% 
  pivot_longer(cols = c("TOTAL_HOSPITALIZED", "TOTAL_DEATHS"), names_to = "Stat") %>% 
  select(DATE, Stat, value)  %>% 
  ggplot() +
  geom_line(aes(DATE, value, color = Stat), size = 1) +
  ylim(c(0, max(wmson$TOTAL_HOSPITALIZED))) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "Total Deaths, Hospitalized")

  ggsave("output/TN_Williamson_tot_death_hosp.png", width = 8, height = 5, dpi = 200)

#----- NEW_HOSPITALIZED -----
wmson %>% 
  filter(DATE >= "2020-04-03") %>% 
  pivot_longer(cols = c("NEW_HOSPITALIZED", "NEW_DEATHS"), names_to = "Stat") %>% 
  select(DATE, Stat, value)  %>% 
  ggplot() +
  geom_line(aes(DATE, value, color = Stat), size = 1) +
  #geom_col(aes(DATE, value, color = Stat, fill = Stat), position = "stack") +
  ylim(c(0, max(wmson$NEW_HOSPITALIZED))) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "New Deaths, Hospitalized")  
