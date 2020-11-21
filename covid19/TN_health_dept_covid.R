# Williamson County TN

library(readxl)
library(tidyverse)
library(scales)

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
            TOTAL_DEATHS = sum(TOTAL_DEATHS),
            NEW_CASES = sum(NEW_CASES),
            TOTAL_ACTIVE = sum(TOTAL_ACTIVE))

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
  filter(DATE >= "2020-09-03") %>% 
  mutate(NEW_CASES_07da = zoo::rollmean(NEW_CASES, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, NEW_CASES)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, NEW_CASES_07da), color = "red", size = 1) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "New Cases")

ggsave("output/TN_Williamson_new_cases.png", width = 8, height = 5, dpi = 200)

#----- Active CASES -----
wmson %>% 
  filter(DATE >= "2020-09-03") %>% 
  mutate(TOTAL_ACTIVE_07da = zoo::rollmean(TOTAL_ACTIVE, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, TOTAL_ACTIVE)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, TOTAL_ACTIVE_07da), color = "red", size = 1) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "TOTAL_ACTIVE Cases")

ggsave("output/TN_Williamson_tot_active_cases.png", width = 8, height = 5, dpi = 200)  
  
#----- Positivity -----
wmson %>% 
  filter(DATE >= "2020-09-03") %>% 
  mutate(test_positivity = NEW_POS_TESTS * 100 / NEW_TESTS,
         test_positivity_07da = zoo::rollmean(test_positivity, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, test_positivity)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, test_positivity_07da), color = "red", size = 1) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
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
  filter(DATE >= "2020-09-03") %>% 
  pivot_longer(cols = c("NEW_HOSPITALIZED", "NEW_DEATHS"), names_to = "Stat") %>% 
  select(DATE, Stat, value)  %>% 
  ggplot() +
  geom_line(aes(DATE, value, color = Stat), size = 1) +
  #geom_col(aes(DATE, value, color = Stat, fill = Stat), position = "stack") +
  ylim(c(0, max(wmson$NEW_HOSPITALIZED))) +
  scale_x_datetime(breaks = "1 month", date_labels = "%b") +
  my_labs +
  labs(title = "New Deaths, Hospitalized")  
  

#----- New CASES -----
state %>% 
  filter(DATE >= "2020-09-03") %>% 
  mutate(NEW_CASES_07da = zoo::rollmean(NEW_CASES, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, NEW_CASES)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, NEW_CASES_07da), color = "red", size = 1) +
  scale_x_datetime(breaks = "1 month", labels = date_format("%b") ) +
  my_labs +
  labs(title = "New Cases",
       subtitle = "Tennessee")

ggsave("output/TN_state_new_cases.png", width = 8, height = 5, dpi = 200)

#----- Active CASES -----
state %>% 
  filter(DATE >= "2020-09-03") %>% 
  mutate(TOTAL_ACTIVE_07da = zoo::rollmean(TOTAL_ACTIVE, k = 7, fill = NA, align = "right")) %>% 
  ggplot(aes(DATE, TOTAL_ACTIVE)) +
  geom_col(fill = my_blue) +
  geom_line(aes(DATE, TOTAL_ACTIVE_07da), color = "red", size = 1) +
  scale_x_datetime(breaks = "1 month", labels = date_format("%b") ) +
  my_labs +
  labs(title = "TOTAL_ACTIVE Cases",
       subtitle = "Tennessee")
ggsave("output/TN_state_tot_active_cases.png", width = 8, height = 5, dpi = 200)  


#----- Temps -----
# https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table
library(visdat)
temps <- read_csv("D:/Downloads/noaa_2326833.csv")
temps2 <- temps[complete.cases(temps),] %>% mutate(TAVG = (TMAX + TMIN) / 2)

vis_miss(temps)
vis_miss(temps2)
temps %>% 
  count(NAME)
temps2 %>% 
  count(NAME)

temps %>% filter(STATION == "USC00401039") %>% View()

temps2 %>% 
  filter(NAME == "BRENTWOOD, TN US") %>% 
  #View()
  #pivot_longer(cols = c("TMAX", "TMIN", "TAVG"), names_to = "type", values_to = "temp") %>% 
  ggplot(aes(DATE, TAVG)) +
  geom_line(alpha = 0.4, color = "#990000", size = .7) +
  geom_smooth(se = FALSE) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0)) +  
  geom_hline(yintercept = 32) +
  #geom_text(aes( x=max(temps2$DATE) - 20, y=30, label="freezing"), color="black",  size=6 ) +
  labs(title = "2020 Average Daily Temperature",
       subtitle = "BRENTWOOD, TN US - Station:USC00401039",
       y = "Daily Avg Temp (F) \n (hi+low)/2",
       caption = "Data: https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table")
  ggsave("output/TN_2020_Brentwood_avg_temp.png", width = 8, height = 5, dpi = 200)  

  
temps2 %>% 
  filter(NAME == "BRENTWOOD, TN US") %>% 
  ggplot(aes(DATE, TMAX)) +
  geom_line(alpha = 0.4, color = "#990000", size = .7) +
  geom_smooth(se = FALSE ) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0)) +  
  geom_hline(yintercept = 90) +
  labs(title = "2020 Max Daily Temperature",
       subtitle = "BRENTWOOD, TN US - Station:USC00401039",
       y = "Daily Max Temp (F)",
       caption = "Data: https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table")
ggsave("output/TN_2020_Brentwood_max_temp.png", width = 8, height = 5, dpi = 200)  

temps2 %>% 
  filter(NAME == "BRENTWOOD, TN US") %>% 
  ggplot(aes(DATE, TMIN)) +
  geom_line(alpha = 0.4, color = "#990000", size = .7) +
  geom_smooth(se = FALSE ) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0)) +  
  geom_hline(yintercept = 32) +
  labs(title = "2020 Min Daily Temperature",
       subtitle = "BRENTWOOD, TN US - Station:USC00401039",
       y = "Daily Min Temp (F)",
       caption = "Data: https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table")
ggsave("output/TN_2020_Brentwood_min_temp.png", width = 8, height = 5, dpi = 200)  
  
temps2 %>% 
  filter(NAME == "BRENTWOOD, TN US") %>% 
  #View()
  rename(Max = TMAX, Min = TMIN, Avg = TAVG) %>% 
  pivot_longer(cols = c("Max", "Min", "Avg"), names_to = "type", values_to = "temp") %>% 
  ggplot(aes(DATE, temp)) +
  geom_line(alpha = 0.4, color = "#990000", size = .5) +
  geom_smooth(se = FALSE) +
  facet_wrap(~type) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0)) +  
  geom_hline(yintercept = 32) +
  geom_hline(yintercept = 90) +
  labs(title = "2020 Daily Temperature",
       subtitle = "BRENTWOOD, TN US - Station:USC00401039",
       y = "Daily Temp (F)",
       x = "",
       caption = "Data: https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table")
ggsave("output/TN_2020_Brentwood_temp_facet.png", width = 8, height = 5, dpi = 200)  

#----- Temps stacked bar
temps2 %>% 
  filter(NAME == "BRENTWOOD, TN US") %>% 
  filter(DATE <= "2020-06-01") %>% 
  mutate(High = TMAX - TMIN) %>% 
  rename(Max = TMAX, Low = TMIN, Avg = TAVG) %>% 
  pivot_longer(cols = c("High", "Low"), names_to = "type", values_to = "temp") %>% 
  ggplot() +
  geom_col(aes(DATE, temp, group = type, fill = type), position = "stack") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0)) +  
  geom_hline(yintercept = 32) +
  geom_hline(yintercept = 90) +
  labs(title = "2020 Daily Temperature",
       subtitle = "BRENTWOOD, TN US - Station:USC00401039",
       y = "Daily Temp (F)",
       x = "",
       caption = "Data: https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table")
ggsave("output/TN_2020_Brentwood_temp_bar.png", width = 8, height = 5, dpi = 200)

?geom_col
