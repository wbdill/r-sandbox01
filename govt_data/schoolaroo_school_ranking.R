# https://scholaroo.com/report/state-education-rankings/
# 2022
library(tidyverse)

getwd()
setwd("D:/opendata/")
edu <- read_csv("2022_state_education_rank_schoolaroo.com+report+state-education-rankings.csv")

edu <- janitor::clean_names(edu)

elec <- read_csv("PresidentialElection1976_2020_state/US_pres_elec_by_state_2020.csv")

join <- edu |> inner_join(elec)

join |> 
  ggplot(aes(x = pct_dem, y = total_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(aes(label  = state_abbrev), nudge_y = 0.5, nudge_x = 0.5, size = 4) +
  labs(title = "State School Score vs % Voting Democratic in 2020",
       x = "% state voted (D) in 2020",
       y = "Total schoolaroo rank (higher better)",
       caption = "@bdill\nSchool Data: schoolaroo.com/report/state-education-rankings")

#----- linear regression -----
mod <- lm(total_score ~ pct_dem, data = join)
summary(mod)

#install.packages("gtsummary")
library(gtsummary)
#?gtsummary

gtsummary::tbl_regression(mod, label = list(pct_dem ~ "% vote Dem"))
