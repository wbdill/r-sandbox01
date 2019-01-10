
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_1YR_S0201&prodType=table

library(tidyverse)
census <- read_tsv("C:/GitHub/r-sandbox01/data/US_census_ACS_2017_S0201.txt")
glimpse(census)


#----- drop all the MOE___ columnms
census2 <- census %>%
  select(-starts_with("MOE")) %>%
  select(-starts_with("POPGROUP")) %>%
  rename(state = GEO.displaylabel)

#----- select columns of interest (metadata file has column names/descriptions ACS_17_1YR_S0201_metadata.xlsx)
census3 <- census2 %>%
  select(state,
         tot_pop = EST_VC11,
         median_house_income_2017  = EST_VC319,
         poverty_all_families      = EST_VC366,
         poverty_all_people        = EST_VC376,
         housing_owners            = EST_VC390,
         housing_renters           = EST_VC391,
         housing_median_unit_value = EST_VC445,
         housing_rental_units      = EST_VC457,
         housing_median_rent       = EST_VC458,
         age_00_04  = EST_VC16,
         age_05_17  = EST_VC17,
         age_18_24  = EST_VC18,
         age_25_34  = EST_VC19,
         age_30_44  = EST_VC20,
         age_45_54  = EST_VC21,
         age_55_64  = EST_VC22,
         age_65_74  = EST_VC23,
         age_75_999 = EST_VC24,
         school_age_tot = EST_VC117,
         school_3_prek  = EST_VC118,
         school_k       = EST_VC119,
         school_1_8     = EST_VC120,
         school_9_12    = EST_VC121,
         school_13_plus = EST_VC122,
         educ_age25_total           = EST_VC134,
         educ_age25_no_hs_diploma   = EST_VC135,
         educ_age25_hs_diploma      = EST_VC136,
         educ_age25_some_college    = EST_VC137,
         educ_age25_bachelor_degree = EST_VC138,
         educ_age25_graduate_degree = EST_VC139,
         married  = EST_VC93,
         widowed  = EST_VC94,
         divorced = EST_VC95
         ) 

#----- Graph of age range distributions -----
census3 %>%
  select(state, age_05_17, age_18_24, age_25_34, age_65_74, age_75_999) %>%
  arrange(desc(age_75_999))

census_age <- census3 %>%
  select(state, starts_with("age"), -starts_with("age25")) %>%
  gather(key=age_range, value=percent, starts_with("age")) %>%
  mutate(age_range = gsub("age_", "", age_range))  %>%
  arrange(state, age_range)
  
census_age %>%
  filter(state %in% c("Maine", "Florida", "Tennessee", "New York", "District of Columbia")) %>%
  ggplot(aes(x=age_range, y = percent)) +
  geom_line(aes(group=state, color=state)) +
  labs(
    x = "Age Range",
    y = "Percent",
    title = "US State Age Range Distribution",
    #subtitle = "",
    caption = "census.gov S0201 table 2017"
 )

#----- Graph of marital status -----
census3 %>%
  select(state, married, widowed, divorced) %>%
  arrange(desc(married))

census3 %>%
  filter(state %in% c("California", "Florida", "Tennessee", "New York", "District of Columbia", "Utah")) %>%
  select(state, married, widowed, divorced) %>%
  gather(key=marital_status, value = percent, 2:4) %>%
  ggplot(aes(x = marital_status, y = percent)) +
  geom_line(aes(group=state, color=state))

#----- Graph of school age distribution -----
census_school <- census3 %>%
  select(state, starts_with("school_"), -"school_age_tot") %>%
  gather(key=level, value = percent, starts_with("school_")) %>%
  mutate(level = gsub("school_", "", level))  %>%
  mutate(level = factor(level, levels=c("3_prek", "k", "1_8", "9_12", "13_plus") )) %>%
  arrange(state, level)

census_school  %>%
  filter(state %in% c("California", "Florida", "Tennessee", "District of Columbia")) %>%
  ggplot(aes(x = level, y = percent)) +
  geom_line(aes(group=state, color=state)) +
  labs(
    x = "School Age Level",
    y = "Percent",
    title = "US State School Age Distribution",
    caption = "census.gov S0201 table 2017"
  )
  
#----- Graph of educational attainment -----
census_edu <- census3 %>%
  select(state, starts_with("educ_age25_"), -"educ_age25_total") %>%
  gather(key=educ_level, value = percent, starts_with("educ_age25_")) %>%
  mutate(educ_level = gsub("educ_age25_", "", educ_level))  %>%
  mutate(educ_level = factor(educ_level, levels=c("no_hs_diploma", "hs_diploma", "some_college", "bachelor_degree", "graduate_degree") )) %>%
  arrange(state, educ_level)

census_edu %>%
  filter(educ_level == "hs_diploma") %>%
  arrange(desc(percent))
  
census_edu  %>%
  filter(state %in% c("California", "Florida", "Tennessee", "District of Columbia", "West Virginia")) %>%
  ggplot(aes(x = educ_level, y = percent)) +
  geom_line(aes(group=state, color=state)) +
  labs(
    x = "Educational Attainment",
    y = "Percent",
    title = "US State Educational Attainment",
    caption = "census.gov S0201 table 2017"
  )



