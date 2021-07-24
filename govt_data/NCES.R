# Desc: NCES data
# Auth: Brian Dill (@bdill)
# Date: 2021-07-23

library(tidyverse)
rm(list = ls())

`%notin%` <- Negate(`%in%`)

#----- NCES State level -----
# https://nces.ed.gov/ccd/files.asp#Fiscal:2,SchoolYearId:35,Page:1
# https://nces.ed.gov/ccd/Data/zip/ccd_sea_052_2021_l_0a_041321.zip

nces_state <- read_csv("D:/opendata/nces.ed.gov/state/2020_2021/ccd_sea_052_2021_l_0a_041321.csv")

nces_state <- rename(nces_state, school_year = SCHOOL_YEAR, fips_st = FIPST, state = STATENAME, state_po = ST,
                     grade = GRADE, race_ethnicity = RACE_ETHNICITY, sex = SEX, student_count = STUDENT_COUNT) %>% 
  select(school_year, fips_st, state, state_po, grade, race_ethnicity, sex, student_count) 
  

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
                                          ) ,
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

#View(nces_state_output)
write_csv(nces_state_output, "D:/opendata/nces.ed.gov/NCES_state_ethnicity_by_grade.csv")
write_csv(nces_state_output, "C:/GitHub/r-sandbox01/govt_data/output/NCES_state_ethnicity_by_grade.csv")

#----- NCES District level -----
nces_lea <- read_csv("D:/opendata/nces.ed.gov/district/2020_2021/ccd_lea_029_2021_w_0b_041321.csv", col_types = cols(.default = col_character()), na = "Not reported")
nces_lea <- nces_lea %>% 
  mutate(G_PK = case_when(G_PK_OFFERED == "Yes" ~ 1, G_PK_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_KG = case_when(G_KG_OFFERED == "Yes" ~ 1, G_KG_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_01 = case_when(G_1_OFFERED == "Yes" ~ 1, G_1_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_02 = case_when(G_2_OFFERED == "Yes" ~ 1, G_2_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_03 = case_when(G_3_OFFERED == "Yes" ~ 1, G_3_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_04 = case_when(G_4_OFFERED == "Yes" ~ 1, G_4_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_05 = case_when(G_5_OFFERED == "Yes" ~ 1, G_5_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_06 = case_when(G_6_OFFERED == "Yes" ~ 1, G_6_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_07 = case_when(G_7_OFFERED == "Yes" ~ 1, G_7_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_08 = case_when(G_8_OFFERED == "Yes" ~ 1, G_8_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_09 = case_when(G_9_OFFERED == "Yes" ~ 1, G_9_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_10 = case_when(G_10_OFFERED == "Yes" ~ 1, G_10_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_11 = case_when(G_11_OFFERED == "Yes" ~ 1, G_11_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_12 = case_when(G_12_OFFERED == "Yes" ~ 1, G_12_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_13 = case_when(G_13_OFFERED == "Yes" ~ 1, G_13_OFFERED == "No"~ 0, TRUE ~ NA_real_),
         G_UG = case_when(G_UG_OFFERED == "Yes" ~ 1, G_UG_OFFERED == "No"~ 0, TRUE ~ NA_real_),
         G_AE = case_when(G_AE_OFFERED == "Yes" ~ 1, G_AE_OFFERED == "No"~ 0, TRUE ~ NA_real_)
         ) %>% 
  select(SchoolYear = SCHOOL_YEAR, FipsState = FIPST, State = STATENAME, StateAbbrev = ST, LeaName = LEA_NAME, StateLeaID = ST_LEAID, NcesLeaID = LEAID,
         NumSchools = OPERATIONAL_SCHOOLS,
         LeaTypeID = LEA_TYPE, LeaType = LEA_TYPE_TEXT, StartYearStatusID = SY_STATUS, StartYearStatus = SY_STATUS_TEXT, 
         UpdStatusID = UPDATED_STATUS, UpdStatus = UPDATED_STATUS_TEXT, Charter = CHARTER_LEA,
         Street1 = LSTREET1, Street2 = LSTREET2, City = LCITY, Zip = LZIP, Phone = PHONE, Website = WEBSITE,
         LowestGrade = GSLO, HighestGrade = GSHI)
         #level = LEVEL, G_PK, G_KG, G_01, G_02, G_03, G_04, G_05, G_06, G_07, G_08, G_09, G_10, G_11, G_12, G_13, G_UG, G_AE)
       
write_csv(nces_lea, "D:/opendata/nces.ed.gov/NCES_Districts.csv")
#str(nces_lea)
#View(nces_lea)
#nces_lea %>% group_by(state_po, lea_type) %>% count() %>% View()
#nces_lea %>% group_by(lea_type_id, lea_type) %>% count()



#----- NCES School level -----
nces_school <- read_csv("D:/opendata/nces.ed.gov/school/2020_2021/ccd_sch_029_2021_w_0b_041321.csv", col_types = cols(.default = col_character()), na = "Not reported")

nces_school <- nces_school %>% 
  mutate(G_PK = case_when(G_PK_OFFERED == "Yes" ~ 1, G_PK_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_KG = case_when(G_KG_OFFERED == "Yes" ~ 1, G_KG_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_01 = case_when(G_1_OFFERED == "Yes" ~ 1, G_1_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_02 = case_when(G_2_OFFERED == "Yes" ~ 1, G_2_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_03 = case_when(G_3_OFFERED == "Yes" ~ 1, G_3_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_04 = case_when(G_4_OFFERED == "Yes" ~ 1, G_4_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_05 = case_when(G_5_OFFERED == "Yes" ~ 1, G_5_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_06 = case_when(G_6_OFFERED == "Yes" ~ 1, G_6_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_07 = case_when(G_7_OFFERED == "Yes" ~ 1, G_7_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_08 = case_when(G_8_OFFERED == "Yes" ~ 1, G_8_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_09 = case_when(G_9_OFFERED == "Yes" ~ 1, G_9_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_10 = case_when(G_10_OFFERED == "Yes" ~ 1, G_10_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_11 = case_when(G_11_OFFERED == "Yes" ~ 1, G_11_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_12 = case_when(G_12_OFFERED == "Yes" ~ 1, G_12_OFFERED == "No" ~ 0, TRUE ~ NA_real_),
         G_13 = case_when(G_13_OFFERED == "Yes" ~ 1, G_13_OFFERED == "No"~ 0, TRUE ~ NA_real_),
         G_UG = case_when(G_UG_OFFERED == "Yes" ~ 1, G_UG_OFFERED == "No"~ 0, TRUE ~ NA_real_),
         G_AE = case_when(G_AE_OFFERED == "Yes" ~ 1, G_AE_OFFERED == "No"~ 0, TRUE ~ NA_real_)
  ) %>% 
  mutate(IsCharter = case_when(CHARTER_TEXT == "Yes" ~ 1, CHARTER_TEXT == "No" ~ 0, TRUE ~ NA_real_)) %>% 
  select(SchoolYear = SCHOOL_YEAR, FipsState = FIPST, State = STATENAME, StateAbbrev = ST, SchoolName = SCH_NAME, StateAgencyNum = STATE_AGENCY_NO, 
         StateLeaID = ST_LEAID, NcesLeaID = LEAID, StateSchoolID = ST_SCHID, NcesSchoolID = SCHID,
         StartYearStatusID = SY_STATUS, StartYearStatus = SY_STATUS_TEXT, UpdStatusID = UPDATED_STATUS, UpdStatus = UPDATED_STATUS_TEXT, IsCharter,
         Street1 = LSTREET1, Street2 = LSTREET2, City = LCITY, Zip = LZIP, Phone = PHONE, Website = WEBSITE, 
         LowestGrade = GSLO, HighestGrade = GSHI, 
         Level = LEVEL, G_PK, G_KG, G_01, G_02, G_03, G_04, G_05, G_06, G_07, G_08, G_09, G_10, G_11, G_12, G_13, G_UG, G_AE)

write_csv(nces_school, "D:/opendata/nces.ed.gov/NCES_Schools.csv")
#nces_school %>% filter(state_po == "TN", nces_leaid == "4704530", grepl('wood', school_name)) %>%  head(100) %>% View()
#nces_school %>% filter(StateAbbrev == "TN", NcesLeaID == "4704530") %>%  head(100) %>% View()



