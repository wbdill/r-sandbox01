# Williamson County TN

library(readxl)
library(tidyverse)

d <- read_xlsx("data/TN-Public-Dataset-County-New.XLSX", col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
?read_xlsx

wmson <- d %>% 
  filter(DATE >= "2020-04-01") %>% 
  filter(COUNTY == "Williamson")
  


my_blue <- "#aaaaff"
my_labs <- labs(title = "TITLE HERE",
             subtitle = "Williamson County, TN (2020)",
             caption = "@bdill Source: https://www.tn.gov/health/cedep/ncov/data/downloadable-datasets.html (County New)")

#----- New Cases -----
wmson %>% 
  filter(DATE >= "2020-05-01") %>% 
  mutate(NEW_TESTS_07da = zoo::rollmean(NEW_TESTS, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, NEW_TESTS)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, NEW_TESTS_07da), color = "red", size = 1) +
  my_labs +
  labs(title = "New Tests")

#----- Positivity -----
wmson %>% 
  filter(DATE >= "2020-09-03") %>% 
  mutate(test_positivity = NEW_POS_TESTS * 100 / NEW_TESTS,
         test_positivity_07da = zoo::rollmean(test_positivity, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, test_positivity)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, test_positivity_07da), color = "red", size = 1) +
  my_labs +
  labs(title = "New Test Positivity")

#----- TOTAL_HOSPITALIZED -----
wmson %>% 
  filter(DATE >= "2020-04-03") %>% 
  pivot_longer(cols = c("TOTAL_HOSPITALIZED", "TOTAL_DEATHS"), names_to = "Stat") %>% 
  select(DATE, Stat, value)  %>% 
  #select(DATE, TOTAL_HOSPITALIZED)
  ggplot() +
  geom_line(aes(DATE, value, color = Stat), size = 1) +
  ylim(c(0, max(wmson$TOTAL_HOSPITALIZED))) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "Total Deaths, Hospitalized")

  ggsave("output/TN_Williamson_tot_death_hosp.png", width = 8, height = 5, dpi = 200)
  
