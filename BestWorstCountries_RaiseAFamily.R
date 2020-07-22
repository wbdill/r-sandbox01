# Best/Worst countries to raise a family
# https://www.asherfergusson.com/raising-a-family-index/

library(tidyverse)
#rm(list = ls())

#ds <- read_tsv("data/BestWorstCountries_RaiseAFamily.tsv")
ds <- read_tsv("https://raw.githubusercontent.com/wbdill/r-sandbox01/master/data/BestWorstCountries_RaiseAFamily.tsv")

# chop off the non-data columns at the end
ds <- ds[,0:15]
str(ds)

# Countries ranked 1, 5, 10, 15, 20, 25, 30, 35
ds %>% 
  filter(Rank %in% c(1,5, 10,15, 20,25,30,34)) %>% 
  select(Country, Rank, SafetyIndex, HappinessIndex, CostIndex, HealthIndex, EducationIndex, TimeIndex, RaisingFamilyIndex) %>% 
  pivot_longer(c(-"Country",-"Rank"), names_to = "Index", values_to = "Value") %>% 
  ggplot(aes(reorder(Country, Rank), Value, group=Index)) +
  geom_line(aes(color=Index), size=1) +
  labs(title = "Country Indexes", 
       subtitle = "Countries ranked 1, 5, 10, 15, 20, 25, 30, 34",
                x = "Country",
          y = "Index Value")

setwd("C:/GitHub/r-sandbox01")
ggsave(filename = paste0(getwd(), "/output/BestWorstCountries_RaiseAFamily.png"), width = 10, height = 6, dpi = 120)
