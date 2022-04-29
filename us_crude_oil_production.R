library(tidyverse)
rm(list = ls())

# https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=mcrfpus2&f=m
path <- "C:/Users/bdill/Downloads/U.S._Field_Production_of_Crude_Oil.csv"
dat <- read_csv(path)

dat <- janitor::clean_names(dat)
dat <- dat %>% separate(month, c("month", "year"), " ")
dat <- dat %>% mutate(month2 = case_when(month == "Jan" ~ "01",
                         month == "Feb" ~ "02",
                         month == "Mar" ~ "03",
                         month == "Apr" ~ "04",
                         month == "May" ~ "05",
                         month == "Jun" ~ "06",
                         month == "Jul" ~ "07",
                         month == "Aug" ~ "08",
                         month == "Sep" ~ "09",
                         month == "Oct" ~ "10",
                         month == "Nov" ~ "11",
                         month == "Dec" ~ "12"))
dat <- dat %>% mutate(yearmonth = paste0(year, "-", month2))
dat

dat %>% filter(year > 2015) %>% 
  ggplot(aes(x = yearmonth, y = k_barrels_per_day)) +
  geom_vline(xintercept = "2021-01") +
  geom_point(aes(x = yearmonth,y = k_barrels_per_day)) +
  ylim(0, max(dat$k_barrels_per_day)) +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  labs(title = "US Crude Oil Production (1000 Barrels per day)",
       x = "Date",
       y = "x1000 Barrels / Day",
       caption = "Data: https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=mcrfpus2&f=m")

