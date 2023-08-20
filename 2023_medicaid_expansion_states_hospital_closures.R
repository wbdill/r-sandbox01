# rural hospitals and Medicaid expansion
# 2023-03-29 @bdill
# https://www.commonwealthfund.org/blog/2022/where-do-states-stand-medicaid-expansion
# https://www.aha.org/2022-09-07-rural-hospital-closures-threaten-access
# https://www.shepscenter.unc.edu/programs-projects/rural-health/rural-hospital-closures/

library(tidyverse)
# https://www.shepscenter.unc.edu/programs-projects/rural-health/rural-hospital-closures/
hosp <- read_tsv("D:/opendata/2023_rural_hospital_closings.tsv") %>% janitor::clean_names()  # State AS

# https://www.commonwealthfund.org/blog/2022/where-do-states-stand-medicaid-expansion
med <- read_tsv("D:/opendata/2023_Medicaid_Expansion.tsv") # State Alabama

states <- read_csv("https://pastebin.com/raw/fL8K9HcY") # state (full) state_abbrev

pop <- read_csv("https://pastebin.com/raw/KjqiYd4A")

pop2 <- pop %>% filter(SummaryLevel == "40") %>%  select(State, pop = PopEst2020) %>% mutate(pop_mill = pop/1000000)
hosp2010 <- hosp %>% filter(closure_year >= 2010)


med <- med %>% mutate(expanded = case_when(State %in% c("Missouri", "Oklahoma", "South Dakota") ~ "full_recent",
                                    Expansion == "Not yet expanded Medicaid (11 states)" ~ "none",
                                    Expansion == "Expanded Medicaid with 1115 waiver (7 states)" ~ "partial",
                                    Expansion == "Traditionally expanded Medicaid (32 states + DC)" ~ "full"))

medst <- med %>% 
  inner_join(states, by = c("State" = "state")) %>% 
  inner_join(pop2)

#----- JOINs -----
join <- med %>% 
  inner_join(states, by = c("State" = "state")) %>% 
  inner_join(hosp2010, by = c("state_abbrev" = "state")) %>% 
  inner_join(pop2, by = "State")


join %>% group_by(expanded) %>% count(name = "closures")
close_by_state <- join %>% group_by(State) %>% 
  summarize(expanded = max(expanded), closures = n(), pop_mill = max(pop_mill), close_per_mill = closures / pop_mill) 
write_csv(close_by_state, "D:/R/2023_hospital_closures_by_state.csv")

medst %>% group_by(expanded) %>% summarize(num_states = n(), states = paste(state_abbrev, collapse = ","), pop_mill = sum(pop_mill)) %>% 
  inner_join(join %>% group_by(expanded) %>% count(name = "closures"), by = "expanded") %>% 
  mutate(closures_per_state = closures / num_states,
         closures_per_pop_mill = closures / pop_mill) %>% 
  relocate(states, .after = last_col())

grouped <- join %>% group_by(state_abbrev, expanded, pop_mill) %>% 
  summarize(closures = n() ) %>% 
  ungroup() %>% 
  mutate(closures_per_mill = closures / pop_mill)
grouped
 #----- Charts _____
group.colors <- c(full = "#66cc66", partial = "#99DD33", full_recent ="#CC9999", none = "#CC3333")

grouped %>% 
  ggplot(aes(x = reorder(state_abbrev, -closures_per_mill), y = closures_per_mill, group = expanded, fill = expanded)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = group.colors)

grouped %>% 
  ggplot(aes(x = reorder(state_abbrev, -closures), y = closures, group = expanded, fill = expanded)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = group.colors, name = "Medicaid expansion") +
  #theme_minimal() +
  labs(title = "Rural Hospital Closures: 2010-2023",
       subtitle = "Grouped by Medicaid Expansion",
       x = "",
       y = "Hospital Closures",
       caption = "@bdill\nClosure data: shepscenter.unc.edu\nMedicaid data: commonwealthfund.org")
