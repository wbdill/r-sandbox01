# Desc: NCES data

# https://nces.ed.gov/ccd/files.asp#Fiscal:2,SchoolYearId:35,Page:1
library(tidyverse)

#----- NCES State level -----
nces_state <- read_csv("D:/opendata/nces.ed.gov/state/2020_2021/ccd_sea_052_2021_l_0a_041321.csv")

nces_state <- rename(nces_state, school_year = SCHOOL_YEAR, fips_st = FIPST, state = STATENAME, state_po = ST,
                     grade = GRADE, race_ethnicity = RACE_ETHNICITY, sex = SEX, student_count = STUDENT_COUNT) %>% 
  select(school_year, fips_st, state, state_po, grade, race_ethnicity, sex, student_count) 
  
`%notin%` <- Negate(`%in%`)
nces_state <- nces_state %>% 
  filter(grade %notin% c("No Category Codes","Not Specified", "Ungraded", "Adult Education", "Grade 13")) %>% 
  #filter(grade != "No Category Codes", grade != "Not Specified", grade != "Ungraded", grade != "Adult Education", grade != "Grade 13") %>% 
  filter(race_ethnicity != "No Category Codes") %>% 
  filter(sex != "No Category Codes") %>% 
      mutate(sex_short = case_when(sex == "Female" ~ "f",
                                   sex == "Male" ~ "m",
                                   TRUE ~ "NA") ,
             race_ethnicity_short = case_when(race_ethnicity == "American Indian or Alaska Native" ~ "indian",
                                              race_ethnicity == "Asian" ~ "asian",
                                              race_ethnicity == "Black or African American" ~ "black",
                                              race_ethnicity == "Hispanic/Latino" ~ "hispanic",
                                              race_ethnicity == "Native Hawaiian or Other Pacific Islander" ~ "pacific",
                                              race_ethnicity == "White" ~ "white",
                                              race_ethnicity == "Two or more races" ~ "twoplus",
                                              TRUE ~ "NA"
                                              ),
             grade_short = case_when(grade == "Grade 1" ~ "01",
                                     grade == "Grade 2" ~ "02",
                                     grade == "Grade 3" ~ "03",
                                     grade == "Grade 4" ~ "04",
                                     grade == "Grade 5" ~ "05",
                                     grade == "Grade 6" ~ "06",
                                     grade == "Grade 7" ~ "07",
                                     grade == "Grade 8" ~ "08",
                                     grade == "Grade 9" ~ "09",
                                     grade == "Grade 10" ~ "10",
                                     grade == "Grade 11" ~ "11",
                                     grade == "Grade 12" ~ "12",
                                     grade == "Grade 13" ~ "13",
                                     grade == "Kindergarten" ~ "K",
                                     grade == "Pre-Kindergarten" ~ "PreK"
                                     )
             )

nces_state_output <- nces_state %>% 
  select(school_year, fips_st, state_po, grade = grade_short, race_ethnicity_short, sex_short, student_count) %>%
  #group_by(school_year, fips_st, state_po, grade, grade_short, race_ethnicity_short, sex_short) %>% count() %>% filter(n>1)
  pivot_wider(names_from = c(race_ethnicity_short, sex_short), values_from = student_count) %>% 
  mutate(indian = indian_m + indian_f,
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hispanic = hispanic_f + hispanic_m,
         pacific = pacific_m + pacific_f,
         white = white_m + white_f,
         twoplus = twoplus_m + twoplus_f,
         total = indian + asian + black + hispanic + pacific + white + twoplus,
         indian_pct = round(indian * 100 / total, 3),
         asian_pct = round(asian * 100 / total, 3),
         black_pct = round(black * 100 / total, 3),
         hispanic_pct = round(hispanic * 100 / total, 3),
         pacific_pct = round(pacific * 100 / total, 3),
         white_pct = round(white * 100 / total, 3),
         twoplus_pct = round(twoplus * 100 / total, 3)
         ) %>% 
  select(school_year, fips_st, state_po, grade, total, indian, asian, black, hispanic, pacific, white, twoplus,
         indian_pct, asian_pct, black_pct, hispanic_pct, pacific_pct, white_pct, twoplus_pct,
         indian_m, indian_f, asian_m, asian_f, black_m, black_f, hispanic_m, hispanic_f, pacific_m, pacific_f, white_m, white_f, twoplus_m, twoplus_f) %>% 
  arrange(fips_st, grade)

View(nces_state_output)
write_csv(nces_state_output, "D:/opendata/nces.ed.gov/NCES_state_ethnicity_by_grade.csv")
write_csv(nces_state_output, "C:/GitHub/r-sandbox01/govt_data/output/NCES_state_ethnicity_by_grade.csv")

#----- NCES District level -----

#----- NCES School level -----