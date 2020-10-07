

# failed: Sign in with Google temporarily disabled for this app  This app has not been verified yet by Google in order to use Google Sign In.
#install.packages("googlesheets")
#library(googlesheets)
#gs_auth(new_user = TRUE)
#f <- googlesheets::gs_read("spreadsheets/d/13pWNWf20gSE6BSyktbn-2G4L0PVkoh9bSfIAjVKzlSQ")

# https://docs.google.com/spreadsheets/d/13pWNWf20gSE6BSyktbn-2G4L0PVkoh9bSfIAjVKzlSQ/edit?usp=sharing

library(tidyverse)
library(lubridate)
rm(list = ls())
sc <- readxl::read_xlsx("C:/GitHub/r-sandbox01/data/US_SCOTUS_2020-09-30.xlsx")
setwd("C:/GitHub/r-sandbox01/")
tail(sc)
str(sc)

sc$succeeded_num <- as.numeric(sc$succeeded_num)

formats <- c("%B %d, %Y", "%m/%d/%Y", "%Y-%m-%d")
sc <- sc %>%   # have to run this twice for some reason...
  mutate(confirmed = parse_date_time(confirmed, formats),
         start_date = parse_date_time(start_date, formats),
         end_date = parse_date_time(end_date, formats),
         tenure_years = time_length(difftime(coalesce(end_date, today()), start_date), "years"),
         confirmed_century =   as.integer(paste0(floor(lubridate::year(confirmed) / 100), "00")),
         confirmed_decade = as.integer(paste0(floor(lubridate::year(confirmed) / 10), "0"))
         )

#----- Since 1900 graphs -----
sc_1900 <- filter(sc, confirmed > "1900-01-01", is_repeat == 0)
theme_bdill <- theme(panel.grid.minor.x = element_blank())

sc_1900 %>% 
  group_by(religion, confirmed_decade) %>% 
  count() %>% 
  ggplot(aes(x = confirmed_decade, y = n, group = religion, fill = religion)) +
  geom_col(position = "stack", alpha = 1) +
  scale_fill_discrete(name = "Religion") +
  scale_x_continuous(breaks = seq(1900,2020,10), expand = c(0,0) ) +
  theme_bdill +
  labs(title = "SCOTUS Religion by Decade",
       x = "Decade Confirmed",
       y = "Count",
       caption = "@bdill Source: https://bit.ly/2ELPivo")

  ggsave("output/SCOTUS_religion_decade.png", width = 7, height = 5, dpi = 200)


sc_1900 %>% 
  group_by(sex, confirmed_decade) %>% 
  count() %>% 
  ggplot(aes(x = confirmed_decade, y = n, group = sex, fill = sex )) +
  geom_col(position = "stack") +
  scale_fill_discrete(name = "Sex") +
  scale_y_continuous("Count", breaks = seq(0, 10, 1)) +
  scale_x_continuous(breaks = seq(1900,2020,10), expand = c(0,0) ) +
  theme_bdill +
  labs(title = "SCOTUS Sex by Decade",
       x = "Decade Confirmed",
       y = "Count",
       caption = "@bdill Source: https://bit.ly/2ELPivo")
  
ggsave("output/SCOTUS_sex_decade.png", width = 7, height = 5, dpi = 200)


sc_1900 %>% 
  group_by(race, confirmed_decade) %>% 
  count() %>% 
  ggplot(aes(x = confirmed_decade, y = n, group = race,  fill = race )) +
  geom_col(position = "stack") +
  scale_fill_discrete(name = "Race") +
  scale_x_continuous(breaks = seq(1900,2020,10), expand = c(.01,.01) ) +
  theme_bdill +
  labs(title = "SCOTUS Race by Decade",
     x = "Decade Confirmed",
     y = "Count",
     caption = "@bdill Source: https://bit.ly/2ELPivo")

  ggsave("output/SCOTUS_race_decade.png", width = 7, height = 5, dpi = 200)


#----- timeline graphs -----
sc %>% 
    filter(confirmed > "1900-01-01") %>% 
    ggplot(aes(x = start_date, y = reorder(justice, desc(start_date)), color = nominated_by_party), size = 3) +
    geom_point(size = 3) +
    geom_point(aes(x = end_date ), shape = 15, size = 3) +
    geom_segment(aes(x = start_date, xend = coalesce(end_date, today()), yend = justice), size = 1.5) +
    scale_color_manual(name = "Nominating Party", values = c("#0000ff", "#ff0000", "#559955", "#995599", "#555599", "#990033", "#ffccaa")) +
    labs(title = "SCOTUS",
         subtitle = "Confirmations since 1950",
         x = "Tenure",
         y = "Justice",
         caption = "@bdill Source: https://bit.ly/2ELPivo")
    ggsave("output/SCOTUS_tenure_since_1950.png", width = 9, height = 6, dpi = 200)

#----- Misc -----


filter(sc, confirmed > "1970-01-01") %>% 
  count(nominated_by_party)

sc_1970 <- filter(sc, confirmed > "1970-01-01", is_repeat == 0)
count(sc_1970, nominated_by_party)
count(sc_1970, sex)
count(sc_1970, race)
count(sc_1970, religion)

sc_1970 %>% 
  group_by(sex, race, religion) %>% 
  count()

sc %>% 
  filter(is_repeat == 0)%>% 
  filter(confirmed >= "1900-01-01") %>% 
  count(religion)

sc_1900 %>% 
  group_by(end_reason, nominated_by_party) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = nominated_by_party)

