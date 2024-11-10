library(tidyverse)
library(rvest)

# vars used in plot text
src_url <- "https://www.elections.tn.gov/county-breakdown/President%20and%20Vice%20President%20of%20the%20United%20States"
chart_caption = paste0("Chart: X:@bdill threads:@wbdill\n", src_url)
state <- "Tennessee"

# tn <- data_frame(county=character(), candidate=character(), party=character(), votes=integer(), precincts=character())
# url <- "https://www.elections.tn.gov/county-breakdown/President%20and%20Vice%20President%20of%20the%20United%20States"  
# tables <- rvest::read_html(url) %>% html_nodes("table")
# for(i in 1:length(tables)) {
#   dfi <- tables %>% pluck(i) |>
#   rvest::html_table(header = FALSE) |> 
#   mutate(county = X1[1], .before = X1) |> 
#   slice(-(1:2)) |> 
#   select(-c(X5), county, candidate = X1, party = X2, votes = X3, precincts = X4)
#   tn <- rbind(tn, dfi)
# }




#----- Web scraper -----
# blank data frame
potus_raw <- data_frame(county=character(), candidate=character(), party=character(), votes=integer(), precincts=character())

url <- src_url
tables <- rvest::read_html(url) %>% html_nodes("table") 
#tables %>% pluck(1) %>% rvest::html_table(header = FALSE)

# loop through all county tables and append to tn
for(i in 1:length(tables)) {
  # my old crappy way 
  #county_val <- tables[i] %>% html_node("tr") %>% html_text() %>% str_replace_all("\n", "") %>% str_trim()
  #tmp <- html_table(tables[i] )
  #r1 <- as.character(tmp[[1]][ 1 , 1:4]) # row1 contains the col names.  Get 1st row and cols 1:4
  #names <- c("County", r1 ) 
  #dfi <- tmp[[1]][ 2:8 , 1:4]  # actual data is in rows 2-8 of each table
  #dfi <- cbind(county_val, dfi) #add the county name in first column
  #names(dfi) <- names
  # 
  dfi <- tables %>% pluck(i) |>
    rvest::html_table(header = FALSE) |> 
    mutate(county = X1[1], .before = X1) |> 
    slice(-(1:2)) |> 
    select(-c(X5), county, candidate = X1, party = X2, votes = X3, precincts = X4)    
  potus_raw <- rbind(potus_raw, dfi)
}

potus_raw <- janitor::clean_names(potus_raw)
head(potus_raw)
potus_raw <- potus_raw %>% mutate(
  party2 = case_when(           # add party2 to uniquify the multiple independents
      party == "Republican" ~ "rep_votes"
    , party == "Democratic" ~ "dem_votes"
    , party == "Independent" & grepl("Kennedy", candidate) ~ "ind1"
    , party == "Independent" & grepl("Stein",   candidate) ~ "ind2"
    , party == "Independent" & grepl("Bowman",  candidate) ~ "ind3"
    , party == "Independent" & grepl("Cruz",    candidate) ~ "ind4"
    , party == "Independent" & grepl("Fruit",   candidate) ~ "ind5"
  )
)
cat("\014")
#str(tn)
#head(tn, 10)
potus_raw$votes <- as.integer(potus_raw$votes) # force integer b/c it lost the data type somehow
  
  potus_wide <- potus_raw %>% #select(county, party = party2, votes) %>% #head(10)
    pivot_wider(id_cols = county, values_from = votes, names_from = party2) %>% 
    mutate(ind_votes = ind1 + ind2 + ind3 + ind4 + ind5
           , total_votes = rep_votes + dem_votes + ind_votes
           , rep_pct = round(rep_votes / total_votes, digits = 4)
           , dem_pct = round(dem_votes / total_votes, digits = 4)
           , ind_pct = round(ind_votes / total_votes, digits = 4)
           , r_minus_d_gap = round(rep_pct - dem_pct, digits = 4)
           ) %>% 
    select(!c(ind1, ind2, ind3, ind4, ind5))  # select all but the ind1... ind5 cols
  head(potus_wide, 10)
  
  write_csv(potus_wide, paste0("D:/opendata/PresidentialElection2024/POTUS_2024_", state, ".csv") )

  # Double-pivot: https://stackoverflow.com/questions/61367186/pivot-longer-into-multiple-columns
  potus_long <- potus_wide %>% pivot_longer(cols = c("rep_votes", "dem_votes", "ind_votes", "rep_pct", "dem_pct", "ind_pct")
                                              , names_pattern = "(rep|dem|ind)_(votes|pct)", names_to = c("party", "calctype")) %>% 
    pivot_wider(id_cols = c("county", "total_votes", "party"),  names_from = calctype, values_from = value, names_repair = "check_unique")  

  
#----- chart: counts stack by party -----
potus_long %>% ggplot(aes(x=reorder(county, -total_votes), y=votes, group=party,)) +
  geom_col(aes(fill=party)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("#9999dd", "#55cc55", "#dd9999")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
  labs(title = paste0("2024 POTUS Race: ", state)
       , subtitle = "Unofficial results"
       , x = "County"
       , y = "Votes"
       , caption = chart_caption)  


  
# convert county to a factor with levels based on a rank of RACE_ETHNICITY sub-category
# https://stackoverflow.com/questions/76140846/ordering-bars-in-dodged-ggplot-geom-bar-based-on-group-or-subgroup-maximum
potus_long <- potus_long %>% 
  mutate(county = factor(county, levels = potus_long %>% 
                           filter(party == "rep") %>% 
                           arrange(pct) %>%
                           #arrange(-total_votes) %>% 
                           pull(county), ordered = TRUE
                        )
  )
  
#----- chart: percent stack by party -----
potus_long %>% ggplot(aes(x=county, y=pct*100, group=party)) +
  geom_col(aes(fill=party)) +
  geom_hline(yintercept=50) +
  geom_hline(yintercept=60) +
  geom_hline(yintercept=70) +
  geom_hline(yintercept=80) +
  geom_hline(yintercept=90) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 100, by=10)) +
  scale_fill_manual(values=c("#9999dd", "#55cc55", "#dd9999")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
  labs(title = paste0("2024 POTUS Race: ", state)
       , subtitle = "Unofficial results"
       , x = "County"
       , y = "Percent"
       , caption = chart_caption)  

#----- chart: Scatterplot of size vs %dem ----- 
potus_wide %>% ggplot(aes(x=total_votes, y=dem_pct*100)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylim(0,80) +
  #scale_x_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title=paste0("2024 POTUS Race: ", state)
       , subtitle = "Size of County based on vote count vs Dem %"
       , x = "Total Votes"
       , y = "Percent of votes for Dem"
       , caption = chart_caption)
  


potus_raw %>% group_by(county, party2) %>% summarize(string_agg = toString(votes))
# 
# #---- Reading from ai robot download file
# tn <- read_csv("C:/Users/bdill/Downloads/extract-data-from-election-night-reporting-dashboard_election-results_captured-list_2024-11-07_11-19-48_fa78f178-3257-40bb-85da-3cfe6a3525a5.csv")
# tn <- janitor::clean_names(tn)
# 
# tn2 <- tn %>% mutate(other_votes = kennedy_votes + bowman_votes + stein_votes + de_la_cruz_votes + fruit_votes) %>% 
#   select(county
#           , rep_votes = trump_votes
#           , dem_votes = harris_votes
#           , ind_votes
#           ) %>% 
#   mutate(total_votes = rep_votes + dem_votes + other_votes
#          ,rep_pct = rep_votes / total_votes
#          ,dem_pct = dem_votes / total_votes
#          ,ind_pct = ind_votes / total_votes
#          )
# 
# #tn2 %>% pivot_longer(cols = c(rep_pct, dem_pct, other_pct), names_to = "party", values_to = "percent")
# 
# tn2 %>% 
#   mutate(rep_pct = round(rep_pct, digits=4)
#          ,dem_pct = round(dem_pct, digits=4)
#          ,other_pct = round(other_pct, digits=4)) %>% 
#   write_csv("D:/opendata/PresidentialElection2024/POTUS_2024_TN.csv")
# 
# # Double-pivot: https://stackoverflow.com/questions/61367186/pivot-longer-into-multiple-columns
# tn3 <- tn2 %>% pivot_longer(cols = !c("county", "total_votes"), names_pattern = "(rep|dem|other)_(votes|pct)", names_to = c("party", "calctype")) %>% 
#   pivot_wider(id_cols = c("county", "total_votes", "party"),  names_from = calctype, values_from = value, names_repair = "check_unique")
# 
# 
# tn3 %>% ggplot(aes(x=reorder(county, -total_votes), y=votes, group=party,)) +
#   geom_col(aes(fill=party)) +
#   #scale_y_continuous(labels = scales::comma, breaks = seq(0, 100, by=10)) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_fill_manual(values=c("#9999dd", "#999999", "#dd9999")) +
#   theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
#   labs(title = "TN 2024 POTUS Race",
#        subtitle = "Unofficial results",
#        x = "County",
#        y = "Percent",
#        caption = "Chart: X:@bdill threads:@wbdill\nhttps://www.elections.tn.gov/county-breakdown/President%20and%20Vice%20President%20of%20the%20United%20States")  
# 
# # convert county to a factor with levels based on a rank of RACE_ETHNICITY sub-category
# # https://stackoverflow.com/questions/76140846/ordering-bars-in-dodged-ggplot-geom-bar-based-on-group-or-subgroup-maximum
# tn3 <- tn3 %>% 
#   mutate(county = factor(county, 
#                      levels = tn3 %>% 
#                        filter(party == "rep") %>% 
#                        arrange(pct) %>%
#                        #arrange(-total_votes) %>% 
#                        pull(county), ordered = TRUE
#                     )
#   )
# 
# tn3 %>% ggplot(aes(x=county, y=pct*100, group=party)) +
#   geom_col(aes(fill=party)) +
#   geom_hline(yintercept=50) +
#   geom_hline(yintercept=60) +
#   geom_hline(yintercept=70) +
#   geom_hline(yintercept=80) +
#   geom_hline(yintercept=90) +
#   scale_y_continuous(labels = scales::comma, breaks = seq(0, 100, by=10)) +
#   scale_fill_manual(values=c("#9999dd", "#999999", "#dd9999")) +
#   theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.1)) +
#   labs(title = "TN 2024 POTUS Race",
#        subtitle = "Unofficial results",
#        x = "County",
#        y = "Percent",
#        caption = "Chart: X:@bdill threads:@wbdill\nhttps://www.elections.tn.gov/county-breakdown/President%20and%20Vice%20President%20of%20the%20United%20States")  
