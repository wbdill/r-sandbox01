library(tidyverse)
library(stringr)

pres <- read_csv("D:/opendata/PresidentialElection1976_2020_state/1976-2020-president.csv")
pres20 <- pres %>% filter(year == 2020) %>% 
  filter(party_detailed %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
  select(state, state_po, party_detailed, candidatevotes, totalvotes) %>% 
  pivot_wider(names_from = party_detailed, values_from = candidatevotes) %>% 
  mutate(pct_dem = DEMOCRAT * 100 / totalvotes,
         pct_rep = REPUBLICAN * 100 / totalvotes)

states <- read_csv("D:/opendata/FIPS_state_usda.csv")

# vaccine % data source: https://usafacts.org/visualizations/covid-vaccine-tracker-states/
vac <- tibble::tribble(
                     ~state, ~pct_one_dose, ~pct_fully_vac, ~doses_admin,~doses_distrib,~pct_doses_used,
               "Alabama", "48%", "37%",    3963352,    5863220,  "68%",
                "Alaska", "53%", "47%",     720003,     895645,  "80%",
               "Arizona", "56%", "47%",    7456104,    9131170,  "82%",
              "Arkansas", "52%", "40%",    2717593,    3555840,  "76%",
            "California", "68%", "55%",   47304100,   53702165,  "88%",
              "Colorado", "63%", "56%",    6727051,    7582565,  "89%",
           "Connecticut", "73%", "65%",    4761783,    5204915,  "91%",
              "Delaware", "63%", "55%",    1136124,    1420925,  "80%",
  "District of Columbia", "67%", "57%",     936344,    1125745,  "83%",
               "Florida", "63%", "52%",   24308757,   29257575,  "83%",
               "Georgia", "50%", "40%",    9518650,   12743865,  "75%",
                "Hawaii", "73%", "55%",    1761917,    2057660,  "86%",
                 "Idaho", "43%", "39%",    1437006,    1932930,  "74%",
              "Illinois", "65%", "51%",   14358623,   16415395,  "87%",
               "Indiana", "49%", "46%",    6327289,    7469860,  "85%",
                  "Iowa", "56%", "51%",    3264184,    3777875,  "86%",
                "Kansas", "57%", "47%",    2860469,    3503365,  "82%",
              "Kentucky", "56%", "48%",    4456958,    5029095,  "89%",
             "Louisiana", "48%", "40%",    4081272,    5138180,  "79%",
                 "Maine", "71%", "65%",    1720802,    1996940,  "86%",
              "Maryland", "67%", "61%",    7494606,    9154430,  "82%",
         "Massachusetts", "75%", "65%",    9386189,   10319390,  "91%",
              "Michigan", "55%", "50%",   10080912,   12609020,  "80%",
             "Minnesota", "61%", "55%",    6302512,    7150960,  "88%",
           "Mississippi", "46%", "37%",    2414612,    3341105,  "72%",
              "Missouri", "52%", "44%",    5855711,    7112705,  "82%",
               "Montana", "52%", "46%",    1009439,    1212625,  "83%",
              "Nebraska", "57%", "51%",    2033163,    2291390,  "89%",
                "Nevada", "57%", "47%",    3148258,    3590410,  "88%",
         "New Hampshire", "67%", "59%",    1674409,    1990190,  "84%",
            "New Jersey", "69%", "61%",   10835890,   12774075,  "85%",
            "New Mexico", "69%", "59%",    2594590,    2600895, "100%",
              "New York", "66%", "59%",   24002086,   26621305,  "90%",
        "North Carolina", "55%", "46%",   10268978,   12922230,  "79%",
          "North Dakota", "48%", "41%",     686971,     779120,  "88%",
                  "Ohio", "52%", "48%",   11491208,   13672235,  "84%",
              "Oklahoma", "52%", "43%",    3717622,    4560330,  "82%",
                "Oregon", "63%", "57%",    4906981,    6038845,  "81%",
          "Pennsylvania", "68%", "54%",   15190103,   17370885,  "87%",
          "Rhode Island", "71%", "64%",    1376557,    1612785,  "85%",
        "South Carolina", "51%", "42%",    4701997,    6069605,  "77%",
          "South Dakota", "55%", "48%",     885107,    1047165,  "85%",
             "Tennessee", "48%", "41%",    6163529,    7503270,  "82%",
                 "Texas", "56%", "46%",   29471219,   36739015,  "80%",
                  "Utah", "56%", "47%",    3187932,    3630450,  "88%",
               "Vermont", "76%", "68%",     867844,     971810,  "89%",
              "Virginia", "65%", "56%",   10107860,   11472605,  "88%",
            "Washington", "67%", "60%",    9362918,   10258825,  "91%",
         "West Virginia", "47%", "39%",    1505726,    2233035,  "67%",
             "Wisconsin", "58%", "53%",    6384485,    6840465,  "93%",
               "Wyoming", "44%", "38%",     469707,     568995,  "83%"
  )
vac$pct_one_dose <- stringr::str_replace(vac$pct_one_dose, '%', '')
vac$pct_fully_vac <- stringr::str_replace(vac$pct_fully_vac, '%', '')
vac$pct_doses_used <- stringr::str_replace(vac$pct_doses_used, '%', '')

vac %>% mutate(across(vars(starts_with("fac")), as.numeric()))
vac <- vac %>% mutate_at(vars(starts_with("pct")), as.numeric)
str(vac)

final <- pres20 %>% inner_join(states, by = c("state_po" = "state_abbrev")) %>% 
  select(state = state.y, state_po, pct_dem, pct_rep) %>% 
  inner_join(vac, by = c("state"))

final %>% 
  ggplot(aes(x = pct_dem, y = pct_fully_vac)) +
  geom_point(size = 2) +
  geom_abline(color = "blue", size = 1) +
  #geom_smooth() +
  #scale_x_continuous(limits = c(25, 70)) + 
  scale_y_continuous(limits = c(30, 70)) +
  labs(title = "US States",
       subtitle = "% Fully Vaccinated vs % Voting Biden in 2020",
       x = "% of vote for Biden in 2020",
       y = "% of population fully vaccinated",
       caption = "chart: @bdill\nvaccine data: usafacts.org\nelection data: dataverse.harvard.edu")
warnings()  
