

# vars used in plot text
src_url <- "https://results.enr.clarityelections.com/AR/122502/web.345435/#/detail/100"
chart_caption = paste0("Chart: X:@bdill threads:@wbdill\n", src_url)
state <- "Arkansas"

raw <- read_csv("D:/opendata/PresidentialElection2024/AR/Arkansas_2024_potus_unofficial.csv") 
raw <- raw %>% slice(1:n()-1)
str(raw)
potus_wide <- raw %>% 
  #pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(ind_votes = ind1_votes + ind2_votes + ind3_votes + ind4_votes + ind5_votes
         #,total_votes2 = dem_votes + rep_votes + ind_votes
         ) %>% 
  select(county, dem_votes, rep_votes, ind_votes, total_votes) %>% 
  mutate(  dem_pct = round(dem_votes/total_votes, digits = 4)
           , rep_pct = round(rep_votes/total_votes, digits = 4)
           , ind_pct = round(ind_votes/total_votes, digits = 4)
  )

potus_long <- potus_wide %>% 
  pivot_longer(cols = c("rep_votes", "dem_votes", "ind_votes", "rep_pct", "dem_pct", "ind_pct")
               , names_pattern = "(rep|dem|ind)_(votes|pct)", names_to = c("party", "calctype")) %>% 
  pivot_wider(id_cols = c("county", "total_votes", "party")
              , names_from = calctype, values_from = value, names_repair = "check_unique") 

#----- counts stack by party -----
potus_long %>% ggplot(aes(x=reorder(county, -total_votes), y=votes, group=party,)) +
  geom_col(aes(fill=party)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("#9999dd", "#55cc55", "#dd9999")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
  labs(title =  paste0("2024 POTUS Race: ", state),
       subtitle = "Unofficial results",
       x = "County",
       y = "Votes",
       caption = chart_caption
  )  

# convert county to a factor with levels based on a rank of RACE_ETHNICITY sub-category
# https://stackoverflow.com/questions/76140846/ordering-bars-in-dodged-ggplot-geom-bar-based-on-group-or-subgroup-maximum
potus_long <- potus_long %>% mutate(county = factor(county, levels = potus_long %>% filter(party == "dem") %>% arrange(-pct) %>% pull(county), ordered = TRUE ) )

potus_long <- potus_long %>% mutate(county = factor(county, levels = potus_wide %>% arrange(-total_votes) %>% pull(county), ordered = TRUE) )

#----- percent stack by party -----
potus_long %>% ggplot(aes(x=county, y=pct*100, group=party)) +
  geom_col(aes(fill=party)) +
  #geom_hline(yintercept=30) +
  geom_hline(yintercept=40, alpha = 0.4) +
  geom_hline(yintercept=50) +
  geom_hline(yintercept=60, alpha = 0.4) +
  geom_hline(yintercept=70) +
  geom_hline(yintercept=80, alpha = 0.4) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 100, by=10)) +
  scale_fill_manual(values=c("#9999dd", "#55cc55", "#dd9999")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
  labs(title = paste0("2024 POTUS Race: ", state),
       subtitle = "Unofficial results",
       x = "County",
       y = "Percent",
       caption = chart_caption
  )  

#----- Scatterplot of size vs %dem -----
potus_wide %>% ggplot(aes(x=total_votes, y=dem_pct*100)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylim(0,80) +
  scale_x_log10(labels = scales::comma) +
  #scale_x_continuous(labels = scales::comma) +
  labs(title="Size of County based on vote count vs Dem %"
       , subtitle = paste0("2024 ", state, " POTUS Race")
       , x = "Total Votes"
       , y = "Percent of votes for Dem"
       , caption = chart_caption
  )
