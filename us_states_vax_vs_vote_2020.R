
library(tidyverse)
library(stringr)

rm(list = ls())

# election data source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
pres <- read_csv("D:/opendata/PresidentialElection1976_2020_state/1976-2020-president.csv")
pres20 <- pres %>% filter(year == 2020) %>% 
  filter(party_detailed %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
  select(state, state_po, party_detailed, candidatevotes, totalvotes) %>% 
  pivot_wider(names_from = party_detailed, values_from = candidatevotes) %>% 
  mutate(pct_dem = DEMOCRAT * 100 / totalvotes,
         pct_rep = REPUBLICAN * 100 / totalvotes)

states <- read_csv("D:/opendata/FIPS_state_usda.csv")

# vaccine data source: https://usafacts.org/visualizations/covid-vaccine-tracker-states/
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
  ggplot(aes(x = pct_dem, y = pct_fully_vac, label = state_po)) +
  geom_point(size = 2) +
  geom_smooth(method = lm) +
  geom_text(nudge_x = 0.5, nudge_y = 0.2, size = 3, check_overlap = TRUE) +
  #scale_x_continuous(limits = c(25, 70)) + 
  scale_y_continuous(limits = c(30, 70)) +
  labs(title = "US States",
       subtitle = "% Fully Vaccinated vs % Voting Biden in 2020",
       x = "% of vote for Biden in 2020",
       y = "% of population fully vaccinated",
       caption = "chart: @bdill\nvaccine data: usafacts.org\nelection data: dataverse.harvard.edu")

#----- usafacts COVID data -----
state_pop <- read_csv("D:/opendata/census.gov/state_pop_totals_2010_2020/Census_state_pop_nst-est2020.csv")
covid <- tibble::tribble(
                              ~state,   ~avc_cases_7d,  ~avg_death_7d, ~cases,    ~deaths,
                        "Alabama",   3865,  27L,    676795,  12103,
                         "Alaska",    460,   5L,     81754,    426,
                        "Arizona",   3099,  22L,    998164,  18661,
                       "Arkansas",   2015,  32L,    441971,   6806,
                     "California",  10360,  59L,   4156859,  64744,
                       "Colorado",   1538,   6L,    608047,   7095,
                    "Connecticut",    642,   4L,    369920,   8355,
                       "Delaware",    275,   3L,    117587,   1869,
           "District of Columbia",    172,   0L,     54366,   1158,
                        "Florida",  17732, 134L,   3151909,  43632,
                        "Georgia",   6283,  49L,   1056788,  22492,
                         "Hawaii",    711,   2L,     58578,    573,
                          "Idaho",    675,   5L,    217052,   2319,
                       "Illinois",   3657,  29L,   1503063,  26401,
                        "Indiana",   3689,  20L,    842501,  14352,
                           "Iowa",   1016,   6L,    400082,   6268,
                         "Kansas",   1276,  11L,    362321,   5536,
                       "Kentucky",   3895,  27L,    557835,   7667,
                      "Louisiana",   4625,  64L,    676368,  12298,
                          "Maine",    205,   1L,     74703,    926,
                       "Maryland",   1076,   7L,    491174,   9929,
                  "Massachusetts",   1323,   6L,    702877,  18212,
                       "Michigan",   2047,  13L,   1050153,  21446,
                      "Minnesota",   1490,   6L,    642288,   7793,
                    "Mississippi",   3200,  40L,    423599,   8214,
                       "Missouri",   2350,  34L,    747436,  10409,
                        "Montana",    433,   4L,    124910,   1781,
                       "Nebraska",    578,   2L,    240804,   2316,
                         "Nevada",   1111,  20L,    385272,   6398,
                  "New Hampshire",    277,   2L,    106726,   1413,
                     "New Jersey",   2028,   0L,   1081954,  26606,
                     "New Mexico",    804,   5L,    228558,   4497,
                       "New York",   3636,  24L,   2231227,  53894,
                 "North Carolina",   6131,  38L,   1181191,  14272,
                   "North Dakota",    263,   1L,    116022,   1556,
                           "Ohio",   3759,  12L,   1197873,  20729,
                       "Oklahoma",   2113,  19L,    530594,   7812,
                         "Oregon",   2054,  14L,    265210,   3095,
                   "Pennsylvania",   3537,  17L,   1284532,  28158,
                   "Rhode Island",    228,   1L,    160948,   2760,
                 "South Carolina",   4429,  33L,    707435,  10413,
                   "South Dakota",    310,   0L,    129866,   2059,
                      "Tennessee",   6278,  30L,   1013943,  13304,
                          "Texas",  17125, 161L,   3513718,  54671,
                           "Utah",   1150,   7L,    458589,   2615,
                        "Vermont",    120,   1L,     27504,    273,
                       "Virginia",   2835,  13L,    751132,  11729,
                     "Washington",   3150,  20L,    546721,   6471,
                  "West Virginia",    966,   7L,    183372,   3049,
                      "Wisconsin",   1456,   3L,    724716,   8429,
                        "Wyoming",    450,   4L,     72995,    835
           )

vote_covid <-  pres20 %>% inner_join(states, by = c("state_po" = "state_abbrev")) %>% 
  select(state = state.y, state_po, pct_dem, pct_rep, totalvotes) %>% 
  inner_join(state_pop, by = c("state" = "State")) %>% 
  inner_join(covid, by = "state") %>% 
  select(state, state_po, Population = PopEst2020, cases, deaths, pct_dem, pct_rep, totalvotes ) %>% 
  mutate(deaths_per_100k = deaths / (Population / 100000),
         cases_pct = cases / (Population / 100))

ggplot(vote_covid, aes(x = pct_dem, y = deaths_per_100k, label = state_po)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(nudge_x = 0.5, nudge_y = 0.2, size = 3, check_overlap = TRUE) +
  labs(title = "State Deaths vs Vote",
       x = "% voted Biden 2020",
       y = "Deaths per 100k",
       caption = "chart: @bdill\nelection data: dataverse.harvard.edu\nCOVID: usafacts.org")

ggplot(vote_covid, aes(x = pct_dem, y = cases_pct, label = state_po)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(nudge_x = 0.5, nudge_y = 0, size = 3, check_overlap = TRUE) +
  labs(title = "State COVID Cases vs Vote",
       x = "% voted Biden 2020",
       y = "COVID Cases % of Population",
       caption = "chart: @bdill\nelection data: dataverse.harvard.edu\nCOVID: usafacts.org")

#---- CDC COVID data -----
# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab
cdc_covid <- read_csv("D:/opendata/CDC/covid_death_counts_by_week_state/Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv")
cdc_covid <- janitor::clean_names(cdc_covid)
cdc_covid <- cdc_covid %>% mutate_at(vars(ends_with("date")), lubridate::mdy)

str(cdc_covid)

# some states have lots of missing values
cdc_covid %>% 
  filter(end_date > '2021-07-04', group == "By Week") %>% 
  select(end_date, state, covid_19_deaths) %>% 
  pivot_wider(names_from = state, values_from = covid_19_deaths) %>% 
  View()

deaths_since_2021_07 <- cdc_covid %>% 
  filter(end_date > '2021-07-04', group == "By Week") %>% 
  group_by(state) %>% 
  summarize(covid_deaths = sum(covid_19_deaths, na.rm = TRUE)) %>% 
  filter(covid_deaths > 100)   # < 100 indicative of too many missing values

vote_covid_cdc <-  pres20 %>% inner_join(states, by = c("state_po" = "state_abbrev")) %>% 
  select(state = state.y, state_po, pct_dem, pct_rep) %>% 
  inner_join(state_pop, by = c("state" = "State")) %>% 
  full_join(deaths_since_2021_07, by = "state") %>% 
  select(state, state_po, Population = PopEst2020, covid_deaths, pct_dem, pct_rep ) %>% 
  mutate(deaths_per_100k = covid_deaths / (Population / 100000) )

ggplot(vote_covid_cdc, aes(x = pct_dem, y = deaths_per_100k, label = state_po)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_text(nudge_x = 0.5, nudge_y = 0, size = 3, check_overlap = TRUE) +
  scale_x_continuous(limits = c(32, 66)) +
  labs(title = "Recent COVID Deaths vs Vote",
       subtitle = "Deaths from July 4 to Aug 21, 2021",
       x = "% voted Biden 2020",
       y = "COVID Deaths per 100K",
       caption = "chart: @bdill\nCOVID data: CDC\nElection data: dataverse.harvard.edu")
