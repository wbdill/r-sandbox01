#Desc:Chart of county level % of vote for Biden vs % vaccinated in 2021
#Auth: Brian Dill (@bdill)
#Date: 2021-07-14

library(tidyverse)
#rm(list = ls())

#----- Presidential election results by county (Harvard) -----
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
path <- "D:/opendata/PresidentialElection2000_2020/countypres_2000-2020.csv"
pres <- read_csv(path)

# pres %>% filter(year == 2020, county_fips == 40001)  # multiple modes, so we need to summarize.

pres2 <- pres %>% filter(year == 2020) %>% 
  select(state = state_po,
         county = county_name,
         county_fips, 
         party, 
         candidate_votes = candidatevotes,
         tot_votes = totalvotes) %>%   
  group_by(state, county, county_fips, party, tot_votes) %>% 
  summarize(candidate_votes = sum(candidate_votes)) %>%           # some states/counties have multiple rows (mail in, early, in person)
  mutate(candidate_pct = candidate_votes * 100 / tot_votes) %>% 
  select(state,
         county,
         county_fips, 
         party, 
         candidate_pct, 
         tot_votes) %>% 
  pivot_wider(names_from = party, values_from = candidate_pct, values_fill = 0) %>% 
  arrange(state, desc(REPUBLICAN))

pres2 %>% filter(state == "TN")

#----- vaccine percents by county (CDC) -----
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh  (115MB csv)
vax_path <- "D:/opendata/covid-19/vaccinations_by_county/COVID-19_Vaccinations_in_the_United_States_County.csv"
vax <- read_csv(vax_path)
#str(vax)

vax$Date <- lubridate::mdy(vax$Date)   # make date so we can filter to a specific date
vax$FIPS <- as.numeric(vax$FIPS)       # make numeric so we can join to pres. UNK coerced to na

vax2 <- vax %>% filter(Date == "2021-07-12") %>% 
  select(date = Date, 
         county_fips = FIPS, 
         county = Recip_County, 
         state = Recip_State, 
         complete_pop_pct = Series_Complete_Pop_Pct, 
         dose1_pop_pct = Administered_Dose1_Pop_Pct)

#----- votes vs vax -----
votevax <- pres2 %>% inner_join(vax2, by = c("county_fips", "state")) %>% 
  select(state, county = county.x, dem_pct = DEMOCRAT, rep_pct = REPUBLICAN, complete_pop_pct, dose1_pop_pct, tot_votes) %>% 
  filter(complete_pop_pct > 0)  # filter out zero values b/c they are probably just missing.

votevax %>%
  filter(state %in% c("CO", "TN", "MS", "CA", "FL", "MI")) %>% 
  ggplot(aes(x = dem_pct, y = complete_pop_pct)) +
  geom_point(aes( size = tot_votes/100000), alpha = .2) +
  geom_smooth(color = "blue", method = "lm") +
  facet_wrap(~state) +
  scale_size_continuous("Tot Votes \n(x100000)") +
    labs(title="Voted Biden in 2020 vs Completely Vaccinated",
       subtitle = "County Level",
       x = "% voted for Biden in 2020",
       y = "pct of population completely vaccinated",
       caption = "Pres. Elec.: dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ\n Vax: data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh")

#----- linear regression of % vaccinated by Dem vote % -----
lm <- lm(complete_pop_pct ~ dem_pct, data = votevax)
summary(lm)


write_csv(votevax, "D:/opendata/vote_2020_vax_2021.csv")
