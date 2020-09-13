#----- Bureau of Labor Statistics data -----
# https://download.bls.gov/pub/time.series/ce/
# bdill 2020-09-11

library(tidyverse)
library(data.table)
rm(list = ls())

setwd("K:/tmp/BLS")

bls <- fread("ce.data.0.AllCESSeries")
series <- fread("ce.series")
datatype <- fread("ce.datatype")
supersector <- fread("ce.supersector")
industry <- fread("ce.industry")


filter(industry, str_detect(industry_name, "education"))

filter(series, str_detect(industry_code, "90921611"))  # CES9092161101   State government education
filter(series, str_detect(industry_code, "90931611"))  # CEU9092161101   Local government education

filter(bls, series_id == "CES9092161101" | series_id == "CEU9092161101") %>% 
  mutate(series_abbrev = case_when(series_id == "CES9092161101" ~ "State",
                                   series_id == "CEU9092161101" ~ "Local",
                                   TRUE ~ "Other")) %>% 
  mutate(month = paste0(year, "-", substr(period, 2, 3))) %>% 
  filter(year >= 2018) %>% 
  ggplot(aes(x = month, y = value)) +
  geom_line(aes(group = series_abbrev, color = series_abbrev)) +
  theme(axis.text.x = element_text(angle = 90))


#----- State-level data - educational svcs employees -----
# SM = State and Area Employment, Hours, and Earnings (NAICS)
# https://download.bls.gov/pub/time.series/sm/

smdatatype <- fread("https://download.bls.gov/pub/time.series/sm/sm.data_type")
smseries <- fread("https://download.bls.gov/pub/time.series/sm/sm.series")
smstate <- fread("https://download.bls.gov/pub/time.series/sm/sm.state")
smindustry <- fread("https://download.bls.gov/pub/time.series/sm/sm.industry")

smco <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.6.Colorado")
smoh <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.36.Ohio")
smsc <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.42.SouthCarolina")
smtn <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.44.Tennessee")
smtx <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.45c.Texas")
smsd <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.43.SouthDakota")

# join to my states table to get abbreviation, region, etc.
states <- fread("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/covid19/data/state_populations_2019.csv")
states <- left_join(smstate, states, by = c("state_name" = "state"))
states[state_name == "Puerto Rico", 3:3] <- "PR"
states[state_name == "Virgin Islands", 3:3] <- "VI"
states[state_name == "All Metropolitan Statistical Areas", 3:3] <- "All"


# rbind (union) all the state table data into one big table.
lst <- ls()[grep("^sm..$", ls())]       # get all vars starting with "sm" and are only 4 chars long
sm_all_states <- rbindlist(mget(lst))   # union all those tables



#----- Analysis to see which data_type_codes and industry_codes I want -----
# data_type_code == 1  # all employees
# area_code == 0  # 0 = statewide    vs 34980	Nashville-Davidson--Murfreesboro--Franklin, TN

filter(smindustry, str_detect(industry_name, "Education"), !str_detect(industry_name, "excluding"))
# 65000000                       Education and Health Services
# 65610000                                Educational Services
# 90921611               State Government Educational Services
# 90931611               Local Government Educational Services
# 90936111 Local Government Elementary and Secondary Education

# filter the smseries to only desired properties.
desired_series <- smseries %>% 
  left_join(smstate, by = "state_code") %>% 
  filter(data_type_code == 1) %>% 
  filter(industry_code %in% c("90921611", "90931611", "65610000", "90936111")) %>%
  filter(state_name %in% c("Colorado", "Ohio", "South Carolina", "South Dakota", "Tennessee", "Texas")) %>%
  filter(area_code == 0) %>% 
  filter(seasonal == "U") %>% 
  arrange(state_name, industry_code)

#----- series_id decomposition breakdown -----
# SMS45000006561000001
# aabccdddddeeeeeeeeff  a=state series, b=seasonal adjust (S|U)  c=state, d=area, e=industry, f=datatype

#----- Plot employees by state -----
plotdata <- sm_all_states %>% 
  filter(year >= 2017) %>% 
  inner_join(desired_series, by = "series_id") %>% 
  #mutate(state_code = as.integer(substr(series_id, 4, 5))) %>% 
  #mutate(industry_code = as.integer(substr(series_id, 11, 18))) %>% 
  mutate(month = as.Date(paste0(year, "-", substr(period, 2, 3),"-01"))) %>% 
  left_join(states, by = c("state_code", "state_name")) %>% 
  left_join(smindustry, by = "industry_code") %>% 
  mutate(value_pop = value * 1000 * 100 / (population ) ) %>% 
  select(series_id, year, period, value, value_pop, state_code, industry_code, data_type_code, seasonal, month, state_name, state_abbrev, industry_name, population)

ggplot(plotdata, aes(x = month, y = value)) +
  geom_line(aes(group = industry_name, color = industry_name), size = .7) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, max(plotdata$value) + 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +  
  facet_wrap(~ state_name) +
  labs(title = "Education Employees",
       subtitle = "State/Local Educational Services (non-seasonal adjusted)",
       y = "Employees (x1000)",
       x = "Date",
       color = "Industry",
       caption = "Graph: @bdill - Data: BLS https://download.bls.gov/pub/time.series/sm/")

ggsave("BLS_education_employees.png", width = 12, height = 7, dpi = 150)

#----- Plot employees as percent of population by state -----
ggplot(plotdata, aes(x = month, y = value_pop)) +
  geom_line(aes(group = industry_name, color = industry_name), size = .7) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, max(plotdata$value_pop) )) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +  
  facet_wrap(~ state_name) +
  labs(title = "Education Employees",
       subtitle = "State/Local Educational Services (non-seasonal adjusted)",
       y = "Percent Employees to Population (2019)",
       x = "Date",
       color = "Industry",
       caption = "Graph: @bdill - Data: BLS https://download.bls.gov/pub/time.series/sm/")

ggsave("BLS_education_employees_pct_pop.png", width = 12, height = 7, dpi = 150)
