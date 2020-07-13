# San Antonio COVID data
#https://cosacovid-cosagis.hub.arcgis.com/
#https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/data?page=10&showData=true

rm(list = ls())
library(tidyverse)
library(lubridate)

#sa <- read_csv("data/SanAntonion_CoVID-19_Dashboard_Data_2020-06-22.csv")
sa <- read_csv("https://opendata.arcgis.com/datasets/94576453349c462598b2569e9d05d84c_0.csv")

#sa$Date <- str_replace_all(sa$Date, "/", "-")
sa$Date <- as.Date(sa$Date)

sa %>% 
  select(Date, StillIll, PosPatients, COVIDnICU, COVIDonVent, DeathsCum, AvailVent) %>% 
  filter(Date > '2020-04-01') %>% 
  pivot_longer( cols = c("PosPatients", "COVIDnICU", "COVIDonVent", "DeathsCum", "AvailVent"), names_to = "var", values_to = "val") %>% 
  ggplot(aes(Date, val, group = var, col = var)) +
  geom_point(size = .5) +
  geom_line() +
  labs(title = "San Antonio",
       subtitle = "Hospitalized, ICU, Vent",
       y = "",
       caption = "graph: @bdill   data: https://cosacovid-cosagis.hub.arcgis.com/datasets/covid-19-dashboard-data/data?page=10")  

ggsave("output/SanAntonio_active_bdill.png")


