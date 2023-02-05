# Gubernatorial candidate Joy Hofmeister said Oklahoma has a higher violent crime rate than New York and California.
# https://twitter.com/WayneStaffordTV/status/1583223098585022464
#https://twitter.com/bdill/status/1583277860290146304

library(tidyverse)

#"https://worldpopulationreview.com/state-rankings/crime-rate-by-state"
crime <- read_csv("D:/opendata/US_crime_rates_by_state_2022_worldpopulationreview.com.csv")

#elec <- read_csv("D:/opendata/PresidentialElection1976_2020_state/2020_pres_state.csv")
elec <- read_csv("https://pastebin.com/raw/NpMgqgiE")

join <- crime |> inner_join(elec, by = c("State" = "state")) |>
    mutate(highlight = factor(ifelse(state_abbrev %in% c("OK", "NY", "CA"), "Highlighted", "Normal")) )

#join$violentRate[join$state_abbrev == "OK"]

#----- bar chart -----
join |>
    ggplot(aes(x = reorder(state_abbrev, violentRate), y = violentRate)) +
    geom_col( aes(fill = highlight) , show.legend = FALSE) +
    geom_hline(yintercept = join$violentRate[join$state_abbrev == "OK"]) +
    theme_bw() +
    labs(title = "Violent Crime Rate (2022)",
         x = "State",
         y = "Violent Crime Rate (per 100k)",
         fill = "",
         caption = "@bdill\nData: worldpopulationreview.com")


#----- scatterplot -----
join |>
    ggplot(aes(x = pct_rep, y = violentRate)) +
    geom_point(aes(color = highlight), size = 3) +
    geom_text(aes(label = state_abbrev), hjust = -.3, vjust = -.4, size = 3) +
    geom_hline(yintercept = join$violentRate[join$state_abbrev == "OK"]) +
    theme_bw() +
    labs(title = "Violent Crime Rate (2022) vs % Voting for Trump (2020)",
         x = "% voted for Trump",
         y = "Violent Crime Rate (per 100k)",
         caption = "@bdill\nData: worldpopulationreview.com")
