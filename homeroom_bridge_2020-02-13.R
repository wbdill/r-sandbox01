#-------------------------------------------------------------------------------
# 2020-02-13 bdill  script to check Homeroom_Bridge files for internal duplicates 
#                   and cross-check duplicates with the export files
#-------------------------------------------------------------------------------
rm(list = ls())
#install.packages("data.table")
#install.packages("tidyverse")
library(data.table)
library(tidyverse)

# ?fread
ach_export <- fread("D:/_hrb/PROD20200213/ACH_Export.csv")
alt_export <- fread("D:/_hrb/PROD20200213/Alt_Export.csv")
eoc_export <- fread("D:/_hrb/PROD20200213/EOC_Export.csv")

ach_delta <- fread("D:/_hrb/PROD20200213/tnach.csv")
alt_delta <- fread("D:/_hrb/PROD20200213/tnalt.csv")
eoc_delta <- fread("D:/_hrb/PROD20200213/tneoc.csv")

# names(ach_delta)
# head(ach_delta)
# head(ach_export)

shell("cls")  # clear terminal

#----- internal duplicate checks. A tibble: 0 x 5 with no rows means no duplicates. -----
ach_delta %>%
  select(DistrictID, SchoolID, USID, TestCode) %>%
  group_by(DistrictID, SchoolID, USID, TestCode) %>%
  count() %>%
  filter(n > 1)

alt_delta %>%
  select(DistrictID, SchoolID, USID, TestCode) %>%
  group_by(DistrictID, SchoolID, USID, TestCode) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()

eoc_delta %>%
  select(DistrictID, SchoolID, USID, TestCode) %>%
  group_by(DistrictID, SchoolID, USID, TestCode) %>%
  count() %>%
  filter(n > 1)

shell("cls")  # clear terminal.
#----- cross checks with export files.  No results means no duplicates.  -----

inner_join(ach_delta, ach_export, by = c("USID" = "USID", "TestCode" = "TestCode")) %>%
  select(DistrictID.x, DistrictID.y, SchoolID.x, SchoolID.y, USID, TestCode) %>%
  filter(SchoolID.x != SchoolID.y)

inner_join(alt_delta, alt_export, by = c("USID" = "USID", "TestCode" = "TestCode")) %>%
  select(DistrictID.x, DistrictID.y, SchoolID.x, SchoolID.y, USID, TestCode) %>%
  filter(SchoolID.x != SchoolID.y)

inner_join(eoc_delta, eoc_export, by = c("USID" = "USID", "TestCode" = "TestCode")) %>%
  select(DistrictID.x, DistrictID.y, SchoolID.x, SchoolID.y, USID, TestCode) %>%
  filter(SchoolID.x != SchoolID.y)


