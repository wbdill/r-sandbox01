
library(tidyverse)
library(lubridate)
cases <- read_csv("D:/Downloads/randa_Support_Current_Year_2020.csv")

cases$datetime <- lubridate::mdy_hm(cases$created_time)
cases$date_only <- date(cases$datetime)
#cases$created_time <- as.Date()

cases$case_reason <- as.factor(cases$case_reason)

str(cases)

#----- Manual EDA ------
cases %>%
  group_by(dept_name) %>%
  count()

distinct_at(cases, vars(dept_name, acct_name))                   # 450ish
distinct_at(cases, vars(dept_name, case_reason)) %>% View()      # 78
distinct_at(cases, vars(dept_name, case_subject)) %>% View()     # 125

distinct_at(cases, vars(acct_name))        # 450ish
distinct_at(cases, vars(case_reason))      # 31
distinct_at(cases, vars(case_subject))     # 63

#----- Cases by day -----
cases %>%
  #mutate(wk = week(date_only)) %>%
  group_by(dept_name, date_only) %>%
  tally() %>%
  ggplot(aes(date_only, n, group = dept_name, fill = dept_name, color = dept_name)) +
  geom_line(size = .5) +
  facet_wrap(~ dept_name) +
  labs(x = "Date",
       y = "Cases",
       title = "RANDA Support Cases by Day (2020)") +
  theme(legend.position="none")

ggsave("output/randa_support_cases_by_day.png")
#----- Cases by week -----
cases %>%
  mutate(wk = week(date_only)) %>%
  group_by(dept_name, wk) %>%
  tally() %>%
  ggplot(aes(wk, n, group = dept_name, fill = dept_name, color = dept_name)) +
  geom_line() +
  facet_wrap(~ dept_name) +
  labs(x = "Week",
       y = "Cases",
       title = "RANDA Support Cases by Week (2020)") +
  theme(legend.position="none")

ggsave("output/randa_support_cases_by_week.png")

#----- Cases by month -----

cases %>%
  mutate(month = paste0(year(date_only), "-", month(date_only)) )%>%
  group_by(dept_name, month) %>%
  tally() %>%
  ggplot(aes(month, n, group = dept_name, fill = dept_name, color = dept_name)) +
  geom_col() +
  facet_wrap(~ dept_name) +
  labs(x = "Date",
       y = "Cases",
       title = "RANDA Support Cases by Month (2020)") +
  theme(legend.position="none")

ggsave("output/randa_support_cases_by_month.png")


#-----
top_case_reasons <- cases %>%
  select(case_reason) %>%
  group_by(case_reason) %>%
  
  tally() %>%
  top_n(7, n) %>%
  arrange(desc(n))
  
  
  cases %>%
    mutate(wk = week(date_only)) %>%
    filter(case_reason %in% pull(top_case_reasons, case_reason)) %>%
    group_by(dept_name, case_reason, wk) %>%
    tally() %>%
    #View()
    ggplot(aes(wk, n, group = case_reason, fill = case_reason, color = case_reason)) +
    #geom_col(position = "dodge") +
    geom_line() +
    facet_wrap(~ dept_name) +
    labs(x = "Week",
         y = "Cases",
         title = "RANDA Support Cases by Week ") 

  ggsave("output/randa_support_cases_by_week.png")
  
  