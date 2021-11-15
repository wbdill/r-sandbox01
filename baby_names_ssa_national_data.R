# SSA baby names
# https://www.ssa.gov/open/data/
# https://www.ssa.gov/oact/babynames/names.zip

#----- National dataset (not by state) -----
library(tidyverse)
rm(list=ls())

#----- import data -----

csv_folder <- "D:/opendata/SSA/babynames_yob/"

import_multiple_csv_files <- function(mypath, mypattern) {
  files <- dir(mypath, pattern = mypattern, full.names = T)
  tmp2 <- list(length = length(files))
  for (i in 1:length(files)){
    tmp2[[i]] <- read_csv(files[i], col_names = F, col_types = "cci")
    tmp2[[i]] <- cbind(tmp2[[i]], str_sub(files[i], 34,37))  # extract the year from the filename
  }
  tmp2
}

lst <- import_multiple_csv_files(csv_folder,".txt")  # read in all the files to a list object
dfnat <- data.table::rbindlist(lst)                  # unlist them into a single dataframe
names(dfnat) <- c("name", "sex", "n", "year")


#----- create graphs -----
names <- c("Kimberly")
p_sex <- "F"

dfnat %>% filter(name %in% names, sex == p_sex) %>% 
  ggplot(aes(x=year, y = n/1000, group = name, color = name)) +
  geom_line(size = 1) +
  scale_x_discrete(breaks = seq(1870, 2020, 10)) +
  #scale_y_log10() +
  labs(title = paste("Babies Named", knitr::combine_words(names)), 
       y = "Babies (thousands)", 
       x = "Year", 
       color = "Name",
       caption = "Graph: @bdill\nData: https://www.ssa.gov/open/data/ (National Data)")
ggsave()
