library(tidyverse)
rm(list = ls())

#----- Cases
# https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36
# https://bit.ly/cdc-cov-state
path <- "D:/opendata/covid-19/CDC_cases_deaths_state_over_time/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv"
cases <- read_csv(path)

str(cases)
cases$submission_date <- lubridate::mdy(cases$submission_date)
cases <- rename(cases, state_po = state)

#----- state fips, name, abbrev
stfips <- read_csv("https://pastebin.com/raw/fL8K9HcY")

#----- population
pop <- read_csv("https://pastebin.com/raw/jQYXHM8e", skip = 7)
pop2 <- pop %>% group_by(state) %>% 
  mutate(pop = sum(pop_2018)) %>% 
  select(state, pop) %>% 
  distinct() %>% 
  inner_join(stfips, by = "state") %>% 
  rename(state_po = state_abbrev)
  

#----- Presidential election results by county (Harvard) -----
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX  
# https://bit.ly/3iCM1hW
path <- "D:/opendata/PresidentialElection1976_2020_state/1976-2020-president.csv"
pres <- read_csv(path)

#pres %>% group_by(year, party) %>% count() %>% View()

pres20 <- pres %>% filter(year == 2020) %>% 
  select(state_po,
         party = party_simplified, 
         votes = candidatevotes,
         tot_votes = totalvotes) %>% 
  group_by(state_po, party, tot_votes) %>% 
  summarize(votes = sum(votes)) %>%           # multiple OTHER
  mutate(pct = votes * 100 / tot_votes,
         party = case_when(party == "DEMOCRAT" ~ "dem",
                           party == "REPUBLICAN" ~ "rep",
                           party == "GREEN" ~ "green",
                           party == "LIBERTARIAN" ~ "liber",
                           party == "OTHER" ~ "other",
                           TRUE ~ "unkown")) %>% 
  select(state_po,
         party, 
         pct, 
         votes,
         tot_votes) %>% 
  pivot_wider(names_from = party, values_from = c(votes, pct), values_fill = 0) %>% 
  arrange(state_po)

#-----
pres_pop <- pres20 %>% 
  inner_join(pop2, by = c("state_po"))

dat <- cases %>%
  inner_join(pres_pop, by = c("state_po")) %>% 
  mutate(tot_cases_pct = tot_cases*100 / pop,
         tot_cases_per_k = tot_cases / (pop/1000)) %>% 
  select(submission_date, state_po, tot_cases, new_case, tot_death, new_death, tot_cases_pct, tot_cases_per_k) %>% 
  arrange(state_po, submission_date)

#dat %>% filter(submission_date == "2021-07-14") %>% arrange((tot_cases_per_k)) %>% select(state, tot_cases_per_k, pop, pct_dem, pct_rep) %>% View()

dat %>% #filter(state_po %in% c("HI", "VT", "OR", "ME", "NY", "ND", "RI", "SD", "UT", "TN")) %>% 
  filter(state_po %in% c("TX", "AL", "GA", "MS", "HI", "ND")) %>% 
  ggplot(aes(x = submission_date, y = tot_cases_per_k, group = state_po, color = state_po)) +
  geom_line(size = 1.2, alpha = 1) +
  #scale_color_gradient2(low = "#ff0000", high = "#0000ff", mid="#ffddff", midpoint = 50) +
  labs(title = "COVID19 Cases Over Time",
       subtitle = "Top / Bottom 5 States\nColor = % voted Biden in 2020",
       color = "% Biden",
       x = "Date",
       y = "Total Cases per 1000 population",
       caption = "Chart: @bdill \nCOVID Data: (CDC) https://bit.ly/cdc-cov-state\nElection Data: (Harvard) https://bit.ly/3iCM1hW\nPopulation: data.census.gov")
