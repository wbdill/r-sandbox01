library(data.table)
library(tidyverse)

rm(list = ls())
# https://hgis.uw.edu/virus/  <- download of the virus.csv file
# https://www.countries-ofthe-world.com/
path <- "C:/Users/brian.dill/Downloads/virus.csv"
v <- fread(path)

v2 <- gather(v, key = "country", value = "val", -datetime)
head(v2, 20)

shell("cls")
v3 <- separate(v2, val, into = c("cases", "z", "recoveries", "deaths"), sep = "-")
head(v3)
v3[is.na(v3)] <- 0
head(v3)

v3 %>% filter(country == "hubei")


v2 %>% mutate(val2 = strsplit(as.character(val), "-")) %>%
  unnest(val2)
?separate_rows




