library(tidyverse)

# https://worldpopulationreview.com/state-rankings/maternal-mortality-rate-by-state
mort <- read_csv("D:/opendata/maternal_mortality_rate_2022.csv")  
elec <- read_csv("D:/opendata/PresidentialElection1976_2020_state/2020_pres_state.csv")

both <- elec %>% inner_join(mort, by = c("state" = "State"))

both %>% 
  ggplot(aes(x = pct_rep, y = maternalMortalityRate, label = state_abbrev)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(check_overlap = TRUE, nudge_x = .8, nudge_y = -.8) +
  #scale(2) +
  labs(title = "Maternal Mortality Rate vs % Voted Republican in 2020",
       x = "% voted Republican in 2020",
       y = "Maternal Mortality Rate",
       caption = "chart: @bdill\ndata: https://worldpopulationreview.com/state-rankings/maternal-mortality-rate-by-state")
?geom_text
