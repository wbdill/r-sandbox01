# TN 2020 election
rm(list = ls())
install.packages("rvest")  # web scraping package
install.packages("xml2")
library(xml2)
library(rvest) # web scraping
library(tidyverse)


#----- TN County votes -----
test_xml <- read_html("https://www.elections.tn.gov/county-results.php?OfficeByCounty=United%20States%20President")
test_xml[[1]]

#counties_html <- html_nodes(test_xml, css = ".election-result-table .results-header-row h3")
#counties <- as.data.frame(html_text(counties_html))
#names(counties)  <-  "county"

results_html <- html_nodes(test_xml, css = ".election-result-table")
lst <- html_table(results_html)
county_votes = data.frame(row.names = c("county", "candidate", "votes", "percent"))  # bootstrap the data frame

for(i in 1:95) {
  x <- cbind(names(lst[[i]])[1], lst[[i]])  # take column header (county name) and prepend a new column filled with that name
  names(x) <- c("county", "candidate", "party", "votes", "percent")
  county_votes <- rbind(county_votes, x)  # loop and concatenate each county table to county_votes
}
county_votes$votes <- as.numeric( str_remove_all(county_votes$votes, ",") )     # clean and make number
county_votes$percent <- as.numeric( str_remove_all(county_votes$percent, "%") ) # clean and make number
str(county_votes)

tn_county_votes_wide <- county_votes %>% 
  select(county, party, votes) %>% 
  group_by(county, party) %>% 
  summarize(votes = sum(votes)) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(total_votes = Democratic + Independent + Republican,
         dem_pct = round(Democratic * 100 / total_votes, 3),
         rep_pct = round(Republican * 100 / total_votes, 3) ,
         ind_pct = round(Independent * 100 / total_votes, 3) )

# TODO: get Rep, Dem, Ind votes for all US counties with FIPS code


#----- County population -----
county_pop <- read_csv("https://pastebin.com/raw/jQYXHM8e")
county_pop_tn <- county_pop %>% 
  filter(state == "Tennessee") %>% 
  select(fips_code, county, pop_2018) %>% 
  mutate(county = str_remove(county, " County"))

#----- County demographics -----
# https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf
county_demo <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv")
str(county_demo)

county_votes_wide <- county_votes %>% 
  select(county, party, votes) %>% 
  group_by(county, party) %>% 
  summarize(votes = sum(votes)) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(total_votes = Democratic + Independent + Republican,
         dem_pct = round(Democratic * 100 / total_votes, 3),
         rep_pct = round(Republican * 100 / total_votes, 3) ,
         ind_pct = round(Independent * 100 / total_votes, 3) )

#----- County land -----
county_land <- read_csv("https://pastebin.com/raw/swNsVEZA")
county_land_sm <- county_land %>% select(FIPS, land_sq_mi)


county_demo %>% filter(YEAR == 12, AGEGRP == 0) %>% 
  count(STNAME) %>% 
  select(state = STNAME, num_counties = n)

county_demo %>% filter(YEAR == 12, AGEGRP == 0) %>% 
  group_by(STNAME) %>% 
  summarize(tot_pop = sum(TOT_POP), num_counties = n()) %>% 
  select(state = STNAME, tot_pop, num_counties) %>% 
  View()

county_demo_land <- county_demo %>% 
  filter(YEAR == 12, AGEGRP == 0) %>% 
  mutate(pct_black = (BA_MALE + BA_FEMALE) * 100 / TOT_POP,
         pct_white = (WA_MALE + WA_FEMALE) * 100 / TOT_POP,
         pct_indian = (IA_MALE + IA_FEMALE) * 100 / TOT_POP,
         pct_asian = (AA_MALE + AA_FEMALE) * 100 / TOT_POP,
         pct_pacific = (NA_MALE + NA_FEMALE) * 100 / TOT_POP,
         pct_two = (TOM_MALE + TOM_FEMALE) * 100 / TOT_POP,
         pct_hisp = (H_MALE + H_FEMALE) * 100 / TOT_POP,
         FIPS = paste0(STATE, COUNTY), 
         county = str_replace(CTYNAME, " County", "")) %>% 
  select(FIPS, STFIPS = STATE, state = STNAME, COFIPS = COUNTY, county, tot_pop = TOT_POP, 
         pct_white, pct_black, pct_indian, pct_asian, pct_pacific, pct_two, pct_hisp) %>% 
  inner_join(county_land_sm, by = "FIPS") %>% 
  mutate(person_per_sq_mi = tot_pop / land_sq_mi)


#----- % Dem vs density -----
county_demo_land_votes %>% 
  ggplot(aes(x = person_per_sq_mi, y = dem_pct)) +
  geom_point() +
  geom_smooth(method = "lm")

county_demo_land %>% arrange(desc(person_per_sq_mi)) %>% View()
county_demo_land %>% filter(state == "New York") %>% View()
#----- density vs pct_black -----
setwd("D:/opendata/county_landmass")

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_white)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % White")
ggsave("county_density_vs_white.png", width = 9, height = 6, dpi = 150)

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_white)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  #scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % White")
ggsave("county_density_vs_white_linear.png", width = 9, height = 6, dpi = 150)

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_black)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % Black")
ggsave("county_density_vs_black.png", width = 9, height = 6, dpi = 150)

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_asian)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % Asian")
ggsave("county_density_vs_asian.png", width = 9, height = 6, dpi = 150)

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_indian)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % Native American")
ggsave("county_density_vs_nativeamerican.png", width = 9, height = 6, dpi = 150)

county_demo_land %>% 
  ggplot(aes(x = person_per_sq_mi, y = pct_pacific)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(title = "US County density vs % Native Hawaiian or Pacific Islander")
ggsave("county_density_vs_pacificislander.png", width = 9, height = 6, dpi = 150)


county_demo_land %>% 
  filter(state == "New York") %>% 
ggplot(aes(person_per_sq_mi, pct_black)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "loess") +
  scale_x_log10() +
  labs(title = "Counties",
       x = "County Density",
       y = "% Black")


#-----
votes_blk <- county_votes %>% 
  select(county, party, votes) %>% 
  group_by(county, party) %>% 
  summarize(votes = sum(votes)) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(total_votes = Democratic + Independent + Republican,
         d_pct = round(Democratic * 100 / total_votes, 3),
         r_pct = round(Republican * 100 / total_votes, 3) ,
         i_pct = round(Independent * 100 / total_votes, 3) ) %>% 
  inner_join(tn_demo, by = "county")

votes_blk %>% 
  ggplot(aes(x = pct_black, y = d_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "2020 Presidential Election",
       subtitle = "TN Counties",
       x = "% black in county",
       y = "% Biden vote")

#----- 
df %>% 
  group_by(candidate) %>% 
  summarise(votes = sum(votes)) %>% 
  arrange(desc(votes))

df %>% 
  filter(candidate == "Donald J. Trump") %>% 
  arrange(desc(percent)) %>% 
  select(county, votes, percent)

#----- spreadsheet output -----
df_vote_pct <- df %>% 
  select(county, party, votes) %>% 
  group_by(county, party) %>% 
  summarize(votes = sum(votes)) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(total_votes = Democratic + Independent + Republican,
         d_pct = round(Democratic * 100 / total_votes, 3),
         r_pct = round(Republican * 100 / total_votes, 3) ,
         i_pct = round(Independent * 100 / total_votes, 3) ) %>% 
  inner_join(county_pop_tn, by = "county") %>% 
  mutate(pct_that_voted = round(total_votes* 100 / pop_2018, 3) ) %>% 
  arrange(county)

write_csv(df_vote_pct, "C:/GitHub/r-sandbox01/output/tn_2020_presidential_election.csv")

#----- Select counties -----
df_vote_pct %>% 
  filter(county %in% c("Shelby", "Davidson", "Williamson", "Rutheford", "Maury", "Knox", "Wilson")) %>% 
  arrange(desc(d_pct)) %>% 
  pivot_longer(cols = c(d_pct, i_pct, r_pct), values_to = "val" ) %>% 
  select(county, name, val) %>% 
  ggplot(aes(x = county, y = val, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#6666ff", "#66cc66", "#ff6666"),
                    labels = c("Dem", "Ind", "Rep"),
                    name = "Party")+
  labs(title = "TN 2020 Presidential election",
       subtitle = "Select TN counties",
       x = "Count",
       y = "Percent",
       caption = "data: https://www.elections.tn.gov/county-results.php?OfficeByCounty=United%20States%20President")


#----- Kanye vote -----
df_master %>% 
  filter(candidate == "Kanye West") %>% 
  arrange(desc(votes)) %>% 
  inner_join(county_pop_tn, by = "county") %>% 
  top_n(20, votes) %>% 
  select(county, votes, pct_of_vote = percent, pop_2018) %>% 
  mutate(pct_of_pop = round(votes * 100 / pop_2018, 3))



df_vote_pct %>% 
  ggplot(aes(x = pop_2018, y = r_pct))+
  geom_point() +
  geom_smooth(method = "loess", se = F)+
  labs(title = "County Population vs Trump 2020 vote %",
       x = "TN County Population (2018)",
       y = "Trump 2020 Vote %")

mod <- lm(d_pct ~ pop_2018, data = df_vote_pct)
summary(mod)

ggplot(df_vote_pct, aes(y = pop_2018)) +
  geom_boxplot() +
  scale_x_manual(name = "") +
  labs(title = "TN County Populations",
       x = "",
       y = "Population (2018)")
  

summary(df_vote_pct$pop_2018)



