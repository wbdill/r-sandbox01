library(tidyverse)
library(scales)
# NCES: 029=Directory, 052=Membership, 059=Staff
#nces_sch_member <- read_csv("D:/opendata/nces.ed.gov/2021_2022/school/ccd_SCH_052_2122_l_1a_071722.csv")

NCES_2223_Memb_State <- read_csv("D:/opendata/nces.ed.gov/2022_2023/state/ccd_sea_052_2223_l_1a_083023.csv")
NCES_2223_Memb_Dist <- read_csv("D:/opendata/nces.ed.gov/2022_2023/district/ccd_lea_052_2223_l_1a_083023.csv")
NCES_2223_Memb_School <- read_csv("D:/opendata/nces.ed.gov/2022_2023/school/ccd_sch_052_2223_l_1a_083023.csv")

head(NCES_2223_Memb_State)
str(NCES_2223_Memb_State)

#df %>% mutate_at(c('col1', 'col2'), as.factor)
NCES_2223_Memb_State <- NCES_2223_Memb_State |> mutate_at(c("SCHOOL_YEAR", "FIPST", "STATENAME", "ST", "STATE_AGENCY_NO", "GRADE", "RACE_ETHNICITY", "SEX", "TOTAL_INDICATOR", "DMS_FLAG"), as.factor)

NCES_2223_Memb_School |> 
  filter(ST == "TN", str_detect(SCH_NAME, "Ravenwood"), GRADE == "Grade 12") |> 
  View()

NCES_2223_Memb_School %>% 
  filter(GRADE == "No Category Codes") %>% 
  filter(SEX != "No Category Codes" & SEX != "Not Specified") %>% 
  View()

#-------------------------------------------------------------------------------
#----- State level Race/Ethnicity by state (all grades all sexes) using state file (missing CA)-----

NCES_2223_Memb_State_ByState_wide <- NCES_2223_Memb_State %>% 
  filter(GRADE == "No Category Codes") %>% 
  filter(SEX != "No Category Codes" & SEX != "Not Specified") %>% 
  select(FIPST, STATENAME, ST, RACE_ETHNICITY, SEX, STUDENT_COUNT) %>% 
  mutate(RACE_ETHNICITY = case_match(
    RACE_ETHNICITY,
    "American Indian or Alaska Native" ~ "AIAN",
    "Asian" ~ "Asian",
    "Black or African American" ~ "Black",
    "Hispanic/Latino" ~ "Hisp",
    "Native Hawaiian or Other Pacific Islander" ~ "NHOPI",
    "Two or more races" ~ "TwoPlus",
    "White" ~ "White",
    .default = RACE_ETHNICITY
  )) %>% 
  pivot_wider(values_from = STUDENT_COUNT, names_from = SEX) %>% # pivot to get male + female combined (All)
  mutate(All = Female + Male) %>% 
  pivot_longer(c(Female, Male, All), names_to = "SEX", values_to = "STUDENT_COUNT") %>% 
  filter(SEX == "All") %>% 
  pivot_wider(values_from = STUDENT_COUNT, names_from = RACE_ETHNICITY) %>% 
  mutate(NonWhite1 = AIAN + Asian + Black + Hisp + NHOPI,
         Total = White + NonWhite1 + TwoPlus,
         Pct_AIAN = round(AIAN / Total, 3),
         Pct_Asian = round(Asian / Total, 3),
         Pct_Black = round(Black / Total, 3),
         Pct_Hisp = round(Hisp / Total, 3),
         Pct_NHOPI = round(NHOPI / Total, 3),
         Pct_TwoPlus = round(TwoPlus / Total, 3),
         Pct_White = round(White / Total, 3),
         Pct_NonWhite1 = round(NonWhite1 / Total, 3)
         ) 

#----- State level Race/Ethnicity by state (all grades all sexes) using ***DISTRICT*** file b/c State file is missing CA -----
NCES_2223_Memb_State_ByState_wide <- NCES_2223_Memb_Dist %>% 
  filter(GRADE == "No Category Codes") %>% 
  filter(SEX != "No Category Codes" & SEX != "Not Specified") %>% 
  select(FIPST, STATENAME, ST, RACE_ETHNICITY, STUDENT_COUNT) %>% 
  mutate(RACE_ETHNICITY = case_match(
    RACE_ETHNICITY,
    "American Indian or Alaska Native" ~ "AIAN",
    "Asian" ~ "Asian",
    "Black or African American" ~ "Black",
    "Hispanic/Latino" ~ "Hisp",
    "Native Hawaiian or Other Pacific Islander" ~ "NHOPI",
    "Two or more races" ~ "TwoPlus",
    "White" ~ "White",
    .default = RACE_ETHNICITY
  )) %>% 
  group_by(FIPST, STATENAME, ST, RACE_ETHNICITY) %>% 
  summarize(STUDENT_COUNT = sum(STUDENT_COUNT, na.rm = TRUE)) %>% 
  #View()
  pivot_wider(values_from = STUDENT_COUNT, names_from = RACE_ETHNICITY) %>% 
  mutate(NonWhite1 = AIAN + Asian + Black + Hisp + NHOPI,
         Total = White + NonWhite1 + TwoPlus,
         Pct_AIAN = round(AIAN / Total, 3),
         Pct_Asian = round(Asian / Total, 3),
         Pct_Black = round(Black / Total, 3),
         Pct_Hisp = round(Hisp / Total, 3),
         Pct_NHOPI = round(NHOPI / Total, 3),
         Pct_TwoPlus = round(TwoPlus / Total, 3),
         Pct_White = round(White / Total, 3),
         Pct_NonWhite1 = round(NonWhite1 / Total, 3)
  ) 

NCES_2223_Memb_State_ByState_wide %>%  write_csv(file = "D:/opendata/nces.ed.gov/2022_2023/state/NCES_2223_Memb_State_ByState_wide.csv", na = "")

# national stats
NCES_2223_Memb_State_ByState_wide %>% 
  group_by() %>% 
  summarize(AIAN = sum(AIAN, na.rm=TRUE),
            Asian = sum(Asian, na.rm=TRUE),
            Black = sum(Black, na.rm=TRUE),
            Hisp = sum(Hisp, na.rm=TRUE),
            NHOPI = sum(NHOPI, na.rm=TRUE),
            TwoPlus = sum(TwoPlus, na.rm=TRUE),
            White = sum(White, na.rm=TRUE),
            NonWhite1 = sum(NonWhite1, na.rm=TRUE)
            ) %>% 
  mutate(Total = AIAN + Asian + Black + Hisp + NHOPI + TwoPlus + White,
         Pct_AIAN = round(AIAN / Total, 3),
         Pct_Asian = round(Asian / Total, 3),
         Pct_Black = round(Black / Total, 3),
         Pct_Hisp = round(Hisp / Total, 3),
         Pct_NHOPI = round(NHOPI / Total, 3),
         Pct_TwoPlus = round(TwoPlus / Total, 3),
         Pct_White = round(White / Total, 3),
         Pct_NonWhite1 = round(NonWhite1 / Total, 3)
  )


#----- State / RaceEth with N and Pct columns -----
# Double-pivot: https://stackoverflow.com/questions/61367186/pivot-longer-into-multiple-columns
NCES_2223_Memb_State_ByState_long_N_Pct <-  NCES_2223_Memb_State_ByState_wide %>% 
  rename(  N_White = White
         , N_Asian = Asian
         , N_Black = Black
         , N_AIAN = AIAN
         , N_NHOPI = NHOPI
         , N_Hisp = Hisp
         , N_TwoPlus = TwoPlus
         , N_NonWhite1 = NonWhite1) %>% 
  pivot_longer(cols = !c("FIPST", "STATENAME", "ST", "Total"), names_pattern = "(Pct|N)_*(AIAN|Asian|Black|Hisp|NHOPI|TwoPlus|White|NonWhite1){1}", names_to = c("CalcType", "RACE_ETHNICITY")) %>% 
  mutate(CalcType = ifelse(CalcType=="", "Count", CalcType)) %>% 
  pivot_wider(id_cols = c("FIPST", "STATENAME", "ST", "Total", "RACE_ETHNICITY"), names_from = CalcType, values_from = value, names_repair = "check_unique") 

# Chart of each state's STUDENT_COUNT stacked by RaceEth
NCES_2223_Memb_State_ByState_long_N_Pct %>% 
  #filter(RACE_ETHNICITY %in% c("White", "TwoPlus", "NonWhite1")) %>%
  filter(!RACE_ETHNICITY %in% c("NonWhite1")) %>% 
  filter(!(ST %in% c("BI", "VI", "MP", "AS", "GU"))) %>% 
  ggplot(aes(x = reorder(ST, -N), y = N/1000, group = RACE_ETHNICITY, fill = RACE_ETHNICITY)) +
  geom_col() +
  geom_hline(yintercept = 500, alpha=0.3) +
  geom_hline(yintercept = 250, alpha=0.3) +
  geom_hline(yintercept = 100, alpha=0.3) +
  #scale_y_continuous(labels = label_comma() +
  scale_y_continuous(breaks = seq(0,6000, by = 500) ) +
  labs(title = "State Level Student Count by Race/Ethnicity (2022-2023)",
       subtitle = "Source: National Center for Education Statistics (NCES)",
       x = "State",
       y = "Student Count (Thousand)",
       caption = "Chart: X:@bdill threads:@wbdill\nSource: NCES\nhttps://nces.ed.gov/ccd/data/zip/ccd_lea_052_2223_l_1a_083023.zip")
  
# convert ST to a factor with levels based on a rank of RACE_ETHNICITY sub-category
# https://stackoverflow.com/questions/76140846/ordering-bars-in-dodged-ggplot-geom-bar-based-on-group-or-subgroup-maximum
NCES_2223_Memb_State_ByState_long_N_Pct <- NCES_2223_Memb_State_ByState_long_N_Pct %>% 
  mutate(ST = factor(ST, 
                     levels = NCES_2223_Memb_State_ByState_long_N_Pct %>% 
                       filter(RACE_ETHNICITY == "White") %>% 
                       arrange(Pct) %>% 
                       pull(ST),
                     ordered = TRUE
                    )
         )

#----- Chart of each state's stacked Race/Eth percent -----
NCES_2223_Memb_State_ByState_long_N_Pct %>% 
  filter(!RACE_ETHNICITY %in% c("NonWhite1")) %>% 
  filter(!(ST %in% c("BI", "VI", "MP", "AS", "GU"))) %>% 
  ggplot(aes(x = ST, y = Pct, group = RACE_ETHNICITY, fill = RACE_ETHNICITY)) +
  geom_col() +
  geom_hline(yintercept=.5) +
  geom_hline(yintercept=.75) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "State Level Student Percent by Race/Ethnicity (2022-2023)",
       subtitle = "Source: National Center for Education Statistics (NCES)",
       x = "State",
       y = "Percent",
       caption = "Chart: X:@bdill threads:@wbdill\nSource: NCES\nhttps://nces.ed.gov/ccd/data/zip/ccd_lea_052_2223_l_1a_083023.zip")























#-------------------------------------------------------------------------------
#----- State level Race/Ethnicity by Grade (all states all sexes) -----

NCES_2223_Memb_State_ByGrade_wide <- NCES_2223_Memb_Dist %>% 
  filter(GRADE %in% c("Pre-Kindergarten", "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12")) %>% 
  filter(SEX != "No Category Codes" & SEX != "Not Specified") %>% 
  select(FIPST, STATENAME, ST, GRADE, RACE_ETHNICITY, SEX, STUDENT_COUNT) %>% 
  mutate(RACE_ETHNICITY = case_match(
    RACE_ETHNICITY,
    "American Indian or Alaska Native" ~ "AIAN",
    "Asian" ~ "Asian",
    "Black or African American" ~ "Black",
    "Hispanic/Latino" ~ "Hisp",
    "Native Hawaiian or Other Pacific Islander" ~ "NHOPI",
    "Two or more races" ~ "TwoPlus",
    "White" ~ "White",
    .default = RACE_ETHNICITY),
    GRADE = case_match(
      GRADE,
      "Pre-Kindergarten" ~ "PreK",
      "Kindergarten" ~ "K",
      .default = GRADE
    )
  ) %>% 
  select(GRADE, RACE_ETHNICITY, STUDENT_COUNT) %>% 
  group_by(GRADE, RACE_ETHNICITY) %>% 
  summarize(STUDENT_COUNT = sum(STUDENT_COUNT, na.rm = TRUE)) %>% 
  #pivot_wider(values_from = STUDENT_COUNT, names_from = SEX) %>% # pivot to get male + female combined (All)
  #mutate(All = Female + Male) %>% 
  #pivot_longer(c(Female, Male, All), names_to = "SEX", values_to = "STUDENT_COUNT") %>% 
  #filter(SEX == "All") %>% 
  pivot_wider(values_from = STUDENT_COUNT, names_from = RACE_ETHNICITY) %>% 
  mutate(NonWhite1 = AIAN + Asian + Black + Hisp + NHOPI,
         Total = White + NonWhite1 + TwoPlus,
         Pct_AIAN = round(AIAN / Total, 3),
         Pct_Asian = round(Asian / Total, 3),
         Pct_Black = round(Black / Total, 3),
         Pct_Hisp = round(Hisp / Total, 3),
         Pct_NHOPI = round(NHOPI / Total, 3),
         Pct_TwoPlus = round(TwoPlus / Total, 3),
         Pct_White = round(White / Total, 3),
         Pct_NonWhite1 = round(NonWhite1 / Total, 3),
         GradeOrder = case_match(
           GRADE,
           "PreK" ~ -1,
           "K" ~ 0,
           "Grade 1" ~ 1,
           "Grade 2" ~ 2,
           "Grade 3" ~ 3,
           "Grade 4" ~ 4,
           "Grade 5" ~ 5,
           "Grade 6" ~ 6,
           "Grade 7" ~ 7,
           "Grade 8" ~ 8,
           "Grade 9" ~ 9,
           "Grade 10" ~ 10,
           "Grade 11" ~ 11,
           "Grade 12" ~ 12,
         )
  ) 

NCES_2223_Memb_State_ByGrade_wide <- NCES_2223_Memb_State_ByGrade_wide %>% arrange(GradeOrder)
NCES_2223_Memb_State_ByGrade_wide <- NCES_2223_Memb_State_ByGrade_wide %>% 
  mutate(GRADE = factor(GRADE, 
                     levels = c("PreK", "K", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12"),
                     ordered = TRUE
                    )
  )

NCES_2223_Memb_State_ByGrade_wide %>%  write_csv(file = "D:/opendata/nces.ed.gov/2022_2023/state/NCES_2223_Memb_State_ByGrade_wide", na = "")

# national stats
NCES_2223_Memb_State_ByGrade_wide %>% 
  group_by() %>% 
  summarize(AIAN = sum(AIAN, na.rm=TRUE),
            Asian = sum(Asian, na.rm=TRUE),
            Black = sum(Black, na.rm=TRUE),
            Hisp = sum(Hisp, na.rm=TRUE),
            NHOPI = sum(NHOPI, na.rm=TRUE),
            TwoPlus = sum(TwoPlus, na.rm=TRUE),
            White = sum(White, na.rm=TRUE),
            NonWhite1 = sum(NonWhite1, na.rm=TRUE)
  ) %>% 
  mutate(Total = AIAN + Asian + Black + Hisp + NHOPI + TwoPlus + White,
         Pct_AIAN = round(AIAN / Total, 3),
         Pct_Asian = round(Asian / Total, 3),
         Pct_Black = round(Black / Total, 3),
         Pct_Hisp = round(Hisp / Total, 3),
         Pct_NHOPI = round(NHOPI / Total, 3),
         Pct_TwoPlus = round(TwoPlus / Total, 3),
         Pct_White = round(White / Total, 3),
         Pct_NonWhite1 = round(NonWhite1 / Total, 3)
  )


#----- State / RaceEth with N and Pct columns -----
# Double-pivot: https://stackoverflow.com/questions/61367186/pivot-longer-into-multiple-columns
NCES_2223_Memb_State_ByGrade_long_N_Pct <-  NCES_2223_Memb_State_ByGrade_wide %>% 
  rename(  N_White = White
           , N_Asian = Asian
           , N_Black = Black
           , N_AIAN = AIAN
           , N_NHOPI = NHOPI
           , N_Hisp = Hisp
           , N_TwoPlus = TwoPlus
           , N_NonWhite1 = NonWhite1) %>% 
  pivot_longer(cols = !c("GRADE", "Total", "GradeOrder"), names_pattern = "(Pct|N)_*(AIAN|Asian|Black|Hisp|NHOPI|TwoPlus|White|NonWhite1){1}", names_to = c("CalcType", "RACE_ETHNICITY")) %>% 
  mutate(CalcType = ifelse(CalcType=="", "Count", CalcType)) %>% 
  pivot_wider(id_cols = c("GRADE", "Total", "GradeOrder", "RACE_ETHNICITY"), names_from = CalcType, values_from = value, names_repair = "check_unique") 

# Chart of each grade's STUDENT_COUNT stacked by RaceEth
NCES_2223_Memb_State_ByGrade_long_N_Pct %>% 
  #filter(RACE_ETHNICITY %in% c("White", "TwoPlus", "NonWhite1")) %>%
  filter(!RACE_ETHNICITY %in% c("NonWhite1")) %>% 
  #filter(!(ST %in% c("BI", "VI", "MP", "AS", "GU"))) %>% 
  ggplot(aes(x = reorder(GRADE, GradeOrder), y = N/1000, group = RACE_ETHNICITY, fill = RACE_ETHNICITY)) +
  geom_col() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Grade Level Student Count by Race/Ethnicity (2022-2023)",
       subtitle = "Source: National Center for Education Statistics (NCES)",
       x = "Grade",
       y = "Student Count (Thousand)",
       caption = "Chart: X:@bdill threads:@wbdill\nSource: NCES\nhttps://nces.ed.gov/ccd/data/zip/ccd_sea_059_2223_l_1a_083023.zip")


#----- Chart of each grade's stacked Race/Eth percent -----
NCES_2223_Memb_State_ByGrade_long_N_Pct %>% 
  filter(!RACE_ETHNICITY %in% c("NonWhite1")) %>% 
  #filter(!(ST %in% c("BI", "VI", "MP", "AS", "GU"))) %>% 
  ggplot(aes(x = reorder(GRADE, GradeOrder), y = Pct, group = RACE_ETHNICITY, fill = RACE_ETHNICITY)) +
  geom_col(alpha = 0.6) +
  geom_hline(yintercept=.5) +
  scale_y_continuous(labels = label_percent(), breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1) ) +
  labs(title = "Grade Level Student Percent by Race/Ethnicity (2022-2023)",
       subtitle = "Source: National Center for Education Statistics (NCES)",
       x = "Grade",
       y = "Percent",
       caption = "Chart: X:@bdill threads:@wbdill\nSource: NCES\nhttps://nces.ed.gov/ccd/data/zip/ccd_sea_059_2223_l_1a_083023.zip")

#
NCES_2223_Memb_State_ByGrade_long_N_Pct %>% 
  filter(RACE_ETHNICITY %in% c("TwoPlus", "White", "Black", "Hisp") ) %>%
  filter(!GRADE == "PreK") %>% 
  ggplot(aes(x = reorder(GRADE, GradeOrder), y = Pct, group = RACE_ETHNICITY, color = RACE_ETHNICITY )) +
  geom_line(size=2) +
  scale_y_continuous(labels = label_percent(), breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1) ) +
  labs(title = "Grade Level Student Percent by Race/Ethnicity (2022-2023)",
     subtitle = "Source: National Center for Education Statistics (NCES)\nRaces: Black, Hispanic, White, Two or more races",
     x = "Grade",
     y = "Percent",
     caption = "Chart: X:@bdill threads:@wbdill\nSource: NCES\nhttps://nces.ed.gov/ccd/data/zip/ccd_sea_059_2223_l_1a_083023.zip")
