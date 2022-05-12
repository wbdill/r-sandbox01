
library(tidyverse)
library(tidycensus)

# 1) get your own free census API key: https://api.census.gov/data/key_signup.html
#census_api_key("your_key_here", install = TRUE)  # 2) install it to your R environment
# then you can run tidycensus functions like get_acs()
# https://walker-data.com/tidycensus/reference/index.html

vars %>% group_by(concept) %>% count() %>% View()
state_age_sex <- get_acs(
  geography = "state",
  variables = c(
    total    = "B01001_001" ,
    m_total  = "B01001_002" ,
    f_total  = "B01001_026" ,
    f_0to4   = "B01001_027" ,
    f_5to9   = "B01001_028" ,
    f_10to14 = "B01001_029" ,
    f_15to17 = "B01001_030" ,
    f_18to19 = "B01001_031" ,
    f_20     = "B01001_032" ,
    f_21     = "B01001_033" ,
    f_22to24 = "B01001_034" ,
    f_25to29 = "B01001_035" ,
    f_30to34 = "B01001_036" ,
    f_35to39 = "B01001_037" ,
    f_40to44 = "B01001_038" ,
    f_45to49 = "B01001_039" ,
    f_50to54 = "B01001_040" ,
    f_55to59 = "B01001_041" ,
    f_60to61 = "B01001_042" ,
    f_62to64 = "B01001_043" ,
    f_65to66 = "B01001_044" ,
    f_67to69 = "B01001_045" ,
    f_70to74 = "B01001_046" ,
    f_75to79 = "B01001_047" ,
    f_80to84 = "B01001_048" ,
    f_85up   = "B01001_049"
  ),
  year = 2020,
  show_call = TRUE
) 

state_age_sex_wide <- 
  pivot_wider(id_cols = c("GEOID", "NAME"), state_age_sex, names_from = "variable", values_from = "estimate") %>% 
  mutate(f_childbearing_10to54 = f_10to14 + f_15to17 + f_18to19 + f_20 + f_21 + f_22to24 + f_25to29 + f_30to34 + f_35to39 + f_40to44 + f_45to49 + f_50to54)

write_csv(state_age_sex_wide, "D:/tmp/R/state_age_sex_wide.csv")

state_age_sex_wide_all_states
tidycensus::fips_codes %>% distinct(state_code,state, state_name)

#state_age_sex_wide %>% select(GEOID, NAME, total, m_total, f_total, f_childbearing_15to44)
#https://www.guttmacher.org/article/2021/10/26-states-are-certain-or-likely-ban-abortion-without-roe-heres-which-ones-and-why
trigger_states <- c("Idaho", "Wyoming", "Utah", "North Dakota", "South Dakota", "Missouri", "Arkansas", "Oklahoma", "Texas", "Louisiana", "Mississippi", "Tennessee", "Kentucky")
ban_states <- c("Alabama","Arizona","Arkansas","Georgia","Idaho","Iowa","Kentucky","Louisiana",
                "Michigan","Mississippi","Missouri","North Dakota","Ohio","Oklahoma",
                "South Carolina","South Dakota","Tennessee","Texas","Utah","West Virginia",
                "Wisconsin","Wyoming","Florida","Indiana","Montana","Nebraska")

state_age_sex_wide %>% 
  filter(NAME %in% ban_states) %>% 
  select(GEOID, state = NAME, total_pop = total, male_total = m_total, female_total = f_total, f_childbearing_10to54) %>% 
  arrange("NAME") %>% 
  write_csv("D:/tmp/R/roe_states.csv")

  summarise(tot_f_childbearing = sum(f_childbearing_15to44)) 

