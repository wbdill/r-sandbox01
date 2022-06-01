# Median income vs 2020 Pres election results
state_income <- get_acs(geography = "state",
                        variables = "B19013_001",
                        year = 2020,
                        product = "acs5",
                        cache = T)

state_income2 <- state_income %>% 
  mutate(state = toupper(NAME))

pres <- read_csv("D:/opendata/PresidentialElection1976_2020_state/1976-2020-president.csv")
states <- read_csv("D:/opendata/FIPS_state_2020_electoral_college.csv")
pres20 <- pres %>% filter(year == 2020) %>% 
  filter(party_detailed %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
  select(state, state_po, party_detailed, candidatevotes, totalvotes) %>% 
  pivot_wider(names_from = party_detailed, values_from = candidatevotes) %>% 
  mutate(pct_dem = round(DEMOCRAT * 100 / totalvotes, 3),
         pct_rep = round(REPUBLICAN * 100 / totalvotes, 3),
         winning_party = case_when(pct_dem > pct_rep ~ "D",
                                   pct_rep > pct_dem ~ "R",
                                   TRUE ~ "?"),
         winning_margin = round((pct_rep - pct_dem), 3) ) %>% 
  rename(state_abbrev = state_po, total_votes = totalvotes, dem_votes = DEMOCRAT, rep_votes = REPUBLICAN) %>% 
  left_join(states, by = c("state_abbrev" = "state_abbrev")) %>% 
  select(state_fips, state_abbrev, state = state.y, total_votes, dem_votes, rep_votes, pct_dem, pct_rep, winning_party, winning_margin, ec_votes = ec_votes_2020)

#str(pres20)
write_csv(pres20, "D:/opendata/PresidentialElection1976_2020_state/2020_pres_state.csv")


state_vote <- state_income2 %>% inner_join(pres20, by = c("NAME" = "state"))

ggplot(state_vote, aes(x = reorder(NAME, desc(estimate)), y = estimate, fill=winning_party)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("D" = "#9999ff", "R" = "#ff9999")) +
  labs(title = "Median Household Income (US Census ACS)",
       subtitle = "Including 2020 Presidential Outcome",
       x = "State",
       y = "Annual Income",
       caption = "Chart: @bdill\nIncome: US Census Bureau ACS 5-year 2016-2020 (B19013_001)\nElection: https://dataverse.harvard.edu")
