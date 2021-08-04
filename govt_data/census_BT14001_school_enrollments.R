

library(tidyverse)
rm(list=ls())
# https://data.census.gov/cedsci/table?q=school%20enrollment&g=0100000US.050000&tid=ACSDT1Y2019.B14001&hidePreview=true&moe=false&tp=true
sch <- read_csv("D:/opendata/census.gov/school_pop_2019/ACSDT5Y2019.B14001_data_with_overlays_2021-07-27T235831.csv")

sch <- sch[2:nrow(sch),]  # chop off 1st data row

names(sch) <-  c("geoid"
              , "county"
              , "total"
              , "total_moe"
              , "enrolled_in_school"
              , "enrolled_in_school_moe"
              , "enrolled_PK"
              , "enrolled_PK_moe"
              , "enrolled_KG"
              , "enrolled_KG_moe"
              , "enrolled_G1G4"
              , "enrolled_G1G4_moe"
              , "enrolled_G5G8"
              , "enrolled_G5G8_moe"
              , "enrolled_G9G12"
              , "enrolled_G9G12_moe"
              , "enrolled_college"
              , "enrolled_college_moe"
              , "graduate_school"
              , "graduate_school_moe"
              , "not_enrolled"
              , "not_enrolled_moe")

sch$county <- str_replace(sch$county, " County", "")
sch <- sch  %>% mutate_at(vars(matches("enrolled")), as.numeric) %>% 
  mutate_at(vars(matches("graduate")), as.numeric) 

sch <- sch %>% separate(col = county, into = c("county", "state"), sep = ", ") %>% 
  #separate(col = geoid, into = c("geopre", "fips_county"), sep = 9) %>% 
  mutate(fips_county = substr(geoid, 10, 99),
         fips_state = substr(fips_county, 1, 2),
         pop = enrolled_in_school + not_enrolled) %>% 
  select(geoid, fips_state, fips_county, state, county, pop
          , "enrolled_in_school"
          , "enrolled_PK"
          , "enrolled_KG"
          , "enrolled_G1G4"
          , "enrolled_G5G8"
          , "enrolled_G9G12"
          , "enrolled_college"
          , "graduate_school"
          , "not_enrolled"
          , "enrolled_in_school_moe"
          , "enrolled_PK_moe"
          , "enrolled_KG_moe"
          , "enrolled_G1G4_moe"
          , "enrolled_G5G8_moe"
          , "enrolled_G9G12_moe"
          , "enrolled_college_moe"
          , "graduate_school_moe"
          , "not_enrolled_moe")
write_csv(sch, "D:/opendata/census.gov/school_pop_2019/census_school_pop_2019.csv")

# change most columns to numeric
sch <- sch  %>% mutate_at(vars(matches("enrolled")), as.numeric) %>% 
  mutate_at(vars(matches("graduate")), as.numeric) 


#----- Stacked bar chart with controlled stack order and x-axis order
state1 <- "Oregon"
sch %>% filter(state == state1) %>% 
  mutate(pct_enroll = enrolled_in_school * 100 / pop  # create in specific order to use fct_inorder later
         , pct_PK = enrolled_PK * 100 / pop
         , pct_KG = enrolled_KG * 100 / pop
         , pct_G1_4 = enrolled_G1G4 * 100 / pop
         , pct_G5_8 = enrolled_G5G8 * 100 / pop
         , pct_G9_12 = enrolled_G9G12 * 100 / pop
         , pct_College = enrolled_college * 100 / pop
         , pct_Graduate = graduate_school * 100 / pop
         ) %>% 
  select(county, starts_with("pct")) %>% 
  mutate(county = forcats::fct_reorder(county, desc(pct_enroll))) %>%                     # control x-axis plot order
  select(-pct_enroll) %>% 
  pivot_longer(starts_with("pct"), names_to = "Group", values_to = "pct", names_prefix = "pct_") %>% 
  ggplot(aes(x = county, y = pct, group = fct_rev(fct_inorder(Group)))) +                 # control order of legend
  geom_bar(aes(fill = fct_rev(fct_inorder(Group))), position="stack", stat="identity") +  # control order of stack
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="School Enrollment by County",
       subtitle = state1,
       x = "County",
       y = "Enrolled in School %",
       fill = "Edu Cohort",
       caption = "Chart: @bdill\nData: Census table B14001 (2019)"
       )



