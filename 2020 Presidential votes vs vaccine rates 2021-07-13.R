#Desc:Chart of county level % of vote for Biden vs % vaccinated in 2021
#Auth: Brian Dill (@bdill)
#Date: 2021-07-14

library(tidyverse)
#rm(list = ls())

#----- Presidential election results by county (Harvard) -----
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
path <- "D:/opendata/PresidentialElection2000_2020_county/countypres_2000-2020.csv"
pres <- read_csv(path)

#pres %>% group_by(year, party) %>% count() %>% View()

# pres %>% filter(year == 2020, county_fips == 40001)  # multiple modes, so we need to summarize.

#pres %>% filter(year == 2020) %>% write_csv("D:/opendata/PresidentialElection2000_2020/countypres_2020.csv")

pres2 <- pres %>% #filter(year == 2020) %>% 
  select(year,
         state = state_po,
         county = county_name,
         county_fips, 
         party, 
         votes = candidatevotes,
         tot_votes = totalvotes) %>%   
  group_by(year, state, county, county_fips, party, tot_votes) %>% 
  summarize(votes = sum(votes)) %>%           # some states/counties have multiple rows (mail in, early, in person)
  mutate(pct = votes * 100 / tot_votes,
         party = case_when(party == "DEMOCRAT" ~ "dem",
                           party == "REPUBLICAN" ~ "rep",
                           party == "GREEN" ~ "green",
                           party == "LIBERTARIAN" ~ "liber",
                           party == "OTHER" ~ "other",
                           TRUE ~ "unkown")) %>% 
  select(year,
         state,
         county,
         county_fips, 
         party, 
         pct, 
         votes,
         tot_votes) %>% 
  pivot_wider(names_from = party, values_from = c(votes, pct), values_fill = 0) %>% 
  arrange(year, state, county)

#write_csv(pres2, "D:/opendata/PresidentialElection2000_2020/countypres_2000_2020_munged.csv")

# select counties change over time 2000-2020
pres2 %>% 
  filter(state == "TN", county %in% c("WILLIAMSON","DAVIDSON", "RUTHERFORD", "SHELBY", "WILSON") ) %>%
  #filter(state == "MS", county %in% c("LOWNDES","OKTIBBEHA", "HINDS", "WARREN") ) %>% 
  #filter(state == "CO", county %in% c("DENVER","DOUGLAS", "EL PASO", "WELD", "ADAMS", "EAGLE") ) %>% 
  ggplot(aes(x = year, y = pct_rep)) +
  geom_line(aes(group = county, color = county), size = 1) +
  scale_x_continuous( breaks = c(2000, 2004, 2008, 2012, 2016, 2020)) +
  labs(title = "US Presidential Election",
       subtitle = "Percent Voting Republican",
       x = "Election Year",
       y = "% voted Republican",
       caption = "data: https://bit.ly/pres-elec-county-2000-2020 \nhttps://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ")

pres2020 <- pres2 %>% filter(year == 2020)

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
votevax <- pres2020 %>% inner_join(vax2, by = c("county_fips", "state")) %>% 
  select(year, 
         state, 
         county = county.x, 
         county_fips, 
         tot_votes, 
         votes_dem, 
         votes_rep, 
         pct_dem, 
         pct_rep, 
         complete_pop_pct, 
         dose1_pop_pct) %>% 
  filter(complete_pop_pct > 0)  # filter out zero values b/c they are probably just missing.

#write_csv(votevax, "D:/opendata/vote_2020_vax_2021.csv")

votevax %>%
  #filter(state %in% c("CO", "TN", "MS", "CA", "MO", "KY")) %>% 
  filter(state %in% c("TX", "AL", "GA", "MS")) %>% 
  ggplot(aes(x = pct_dem, y = complete_pop_pct)) +
  geom_point(aes( size = tot_votes/100000), alpha = .2) +
  geom_smooth(color = "blue", method = "lm") +
  scale_size_continuous("Tot Votes \n(x100000)") +
  ylim(0,100) +
  facet_wrap(~state) +
  
    labs(title="Voted Biden in 2020 vs Completely Vaccinated ",
       subtitle = "County Level - Vaccinations as of 2021-07-12",
       x = "% voted for Biden in 2020",
       y = "pct of population completely vaccinated",
       caption = "Pres. Elec.: dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ\n Vax: data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh")

#----- linear regression of % vaccinated by Dem vote % -----
lm <- lm(complete_pop_pct ~ pct_dem, data = votevax)
summary(lm)

