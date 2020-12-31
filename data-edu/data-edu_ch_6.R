
# https://datascienceineducation.com/c06.html

#---- devtools failed to install, but "remotes" below worked ----
## install devtools
#install.packages("devtools", repos = "http://cran.us.r-project.org")
## install the dataedu package
#devtools::install_github("data-edu/dataedu")

install.packages("remotes")
remotes::install_github("data-edu/dataedu")

dataedu::install_dataedu()  # takes a LONG time to install
dataedu::dataedu_packages  # list the packages
install.packages("skimr")

library(tidyverse)
library(dataedu)
library(skimr)
library(janitor)

ma_data_init <- dataedu::ma_data_init
names(ma_data_init)
glimpse(ma_data_init)
summary(ma_data_init)

glimpse(ma_data_init$Town)
summary(ma_data_init$Town)
glimpse(ma_data_init$`AP_Test Takers`)
summary(ma_data_init$`AP_Test Takers`)


ma_data_init %>% 
  group_by(`District Name`) %>% 
  count()  

ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n > 10)

ma_data_init %>% 
  group_by(`District Name`) %>% 
  count() %>% 
  filter(n > 10) %>% 
  arrange(desc(n))

ma_data <-  ma_data_init %>% clean_names()
names(ma_data)
