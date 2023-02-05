library(tidyverse)
nces_sch_member <- read_csv("D:/opendata/nces.ed.gov/2021_2022/school/ccd_SCH_052_2122_l_1a_071722.csv")

head(nces_sch_member)
str(nces_sch_member)

#df %>% mutate_at(c('col1', 'col2'), as.factor)
nces_sch_member <- nces_sch_member |> mutate_at(c("SCHOOL_YEAR", "FIPST", "STATENAME", "ST", "STATE_AGENCY_NO", "GRADE", "RACE_ETHNICITY", "SEX", "TOTAL_INDICATOR", "DMS_FLAG"), as.factor)

nces_sch_member |> 
  filter(ST == "TN", str_detect(SCH_NAME, "Ravenwood"), GRADE == "Grade 11") |> 
  View()
