# https://campus.datacamp.com/courses/working-with-data-in-the-tidyverse/
install.packages("skimr")
library(readr)
library(skimr)
rm(list = ls())

p1 = "C:\\Data\\R\\DataCamp\\data\\bakeoff.csv"
p2 = "C:\\Data\\R\\DataCamp\\data\\Bakers\\messy_baker_results.csv"
p3 = "C:\\Data\\R\\DataCamp\\data\\Bakers\\baker_results.csv"
?read_csv
bakeoff <- read_csv(p1)
str(bakeoff)
head(bakeoff)

bakeoff %>%
  filter(!is.na(us_season)) %>% 
  skim()

#----- Distinct and count -----
bakeoff %>% 
  count(result == "SB")

bakeoff %>% 
  count(series, episode)

bakeoff %>% 
  count(series, episode) %>%
  count(series)

ggplot(bakeoff, aes(episode)) + 
  geom_bar() + 
  facet_wrap(~series)

#----- Import problems -----
filepath4 = "C:\\Data\\R\\DataCamp\\data\\Desserts\\desserts.csv"
desserts <- read_csv(filepath4 ,
  col_types = cols(
    technical = col_number()   
  )
)

problems(desserts)  # readr::problems() shows import issues

desserts <- read_csv(filepath4 ,
                     col_types = cols(
                       technical = col_number()   
                     ) , na = c("", "NA", "N/A")
)
desserts

#----- type casting on import -----

# test your format with parse_date(), parse_*  (double, factor, integer, etc.)
parse_date("17 August 2010", format = "%d %B %Y")

# use col_date()  or more generally col_* to cast on import
desserts <- read_csv(filepath4, 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       technical = col_number(),
                       uk_airdate = col_date("%d %B %Y"), 
                       result = col_factor(levels = NULL)
                     ))

desserts

#----- Recoding on import -----
desserts2 <- desserts %>% 
  mutate(tech_win = recode(technical, `1` = 1,
                           .default = 0))

