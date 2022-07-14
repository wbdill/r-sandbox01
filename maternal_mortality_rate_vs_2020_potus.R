library(tidyverse)

# https://worldpopulationreview.com/state-rankings/maternal-mortality-rate-by-state
# https://pastebin.com/0TTaVW04
mortality <- read_csv("https://pastebin.com/raw/0TTaVW04")  # "D:/opendata/maternal_mortality_rate_2022.csv"

#https://pastebin.com/NpMgqgiE
election <- read_csv("https://pastebin.com/raw/NpMgqgiE")  # "D:/opendata/PresidentialElection1976_2020_state/2020_pres_state.csv"

join <- election %>% inner_join(mortality, by = c("state" = "State"))

join %>% 
  ggplot(aes(x = pct_rep, y = maternalMortalityRate, label = state_abbrev)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(check_overlap = TRUE, nudge_x = .8, nudge_y = -.8) +
  #theme_minimal() +
  labs(title = "Maternal Mortality Rate vs % Voted Republican in 2020",
       x = "% voted Republican in 2020",
       y = "Deaths per 100,000 births",
       caption = "chart: @bdill\ndata: https://worldpopulationreview.com/state-rankings/maternal-mortality-rate-by-state")

mod <- lm(maternalMortalityRate ~ pct_rep, data = join)
summary(mod)



