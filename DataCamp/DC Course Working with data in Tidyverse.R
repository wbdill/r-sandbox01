# https://campus.datacamp.com/courses/working-with-data-in-the-tidyverse/
install.packages("skimr")
library(tidyverse)
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
  mutate(tech_win = recode(technical, `1` = 1, .default = 0))

# Tidy data like:  baker score_1 score_2 score_3  guess_1 guess_2 guess_3
data %>%
  gather(key = "key", value = "value", score_1:guess_3) %>%
  separate(key, into = c("var", "order"), convert = TRUE) %>%
  spread(var, value)


bakers <- read_csv(p3)
glimpse(bakers)

# recoding with case_when
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  )) %>% 
  drop_na(skill)

bakers_skill %>%  
  count(skill)


bakers_skill %>%
  ggplot(aes(x = skill, fill = as.factor(series_winner))) +
  geom_bar()

# lubridate intervals
library(tidyverse)
bakers_skill <- bakers_skill  %>% 
  mutate(time_on_air = interval(first_date_appeared, last_date_appeared),
         weeks_on_air = time_on_air / weeks(1))
  
bakers_skill %>%
  select(series, weeks_on_air)

# https://here.r-lib.org/
install.packages("here")
library(here)
here()
