# Desc: load state population and US Senators from 2020 election 
#       to calculate % and # of population represented by senators voting for/against a bill
# Auth: @bdill 2022-05-12

library(tidyverse)
library(googlesheets4)

gs4_auth()

#----- Load in data -----
pop19 <- read_csv("https://pastebin.com/raw/UAhAPiYB")   # US Census 2019 population estimates by state
sheet_url <- "https://docs.google.com/spreadsheets/d/1Bjtu39bRscn0ChISs3cHTwV2-_OLBRcTPb5K-tC0BUw/edit#gid=1788686979"
senators2020 <- read_sheet(sheet_url, range = "clean!A1:H101", col_types = "cccDiDic")  # (c)har, (D)ate, (i)nteger
#str(senators2020)

senators2020 <- senators2020 %>% 
  mutate(TenureYears = round(as.numeric(difftime(Sys.Date(), AssumedOffice, units = "weeks"))/52.25, 2) )

#------ join with population -----
senators2020_pop <- senators2020 %>% 
  inner_join(pop19, by = c("State" = "state")) %>% 
  mutate(RepresentedPop = population / 2)

str(senators2020_pop)

#------ Write out clean CSV of joined data -----
senators2020_pop %>% select(StateAbbrev = state_abbrev, State, Senator, Party, DOB, Age, AssumedOffice, 
                            TenureYears, TermEnds, StatePop = population, RepresentedPop) %>% 
  write_csv("D:/opendata/US_Senate_2020.csv")


senators2020_pop2 <-  senators2020_pop%>% 
  mutate(VotedForRoe = case_when(Party == "Republican" ~ 0,
                                   Party == "Democratic" & Senator != "Joe Manchin" ~ 1,
                                   Party == "Independent" ~ 1,
                                   Senator == "Joe Manchin" ~ 0,)) %>% 
  select(state_abbrev, State, Senator, Party, Age, DOB, RepresentedPop, VotedForRoe)

#----- Summarize represented population by vote -----
senators2020_pop2 %>% group_by(VotedForRoe) %>% 
  summarize(TotalSenators = n(), RepresentedPop = sum(RepresentedPop)) %>% 
  ungroup() %>% 
  mutate(TotRepresent = sum(RepresentedPop) ,
         Pct = RepresentedPop / TotRepresent)

#----- Age and Tenure summary stats by party  -----
senators2020_pop %>% group_by(Party) %>% 
  summarize(count = n(),
            avg_age = mean(Age),
            youngest = min(Age),
            oldest = max(Age),
            avg_tenure_years = mean(TenureYears),
            longest_serving_years = max(TenureYears))
