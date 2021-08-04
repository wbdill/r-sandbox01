# Desc: parsing/munging of OPE Postsecondary Institutions list (colleges)
# Auth: Brian Dill (@bdill) 2021-08-03
# Note: OPE = Office of Postsecondary Education (part of US Dept. of Education

library(tidyverse)

# https://www2.ed.gov/about/offices/list/ope/index.html
# https://ope.ed.gov/dapip/#/download-data-files
col <- read_csv("D:/opendata/ope.ed.gov/InstitutionCampus.csv")

#head(col)
#col %>% group_by(LocationType) %>% count()

col2 <- col %>% filter(LocationType %in% c("Institution", "Site"))


# split Address into Address, City / ST Zip  on the last comma. Ceates a list with 2 elements
add2 <- strsplit(col2$Address, ",\\s*(?=[^,]+$)", perl=TRUE)
#add2[[1]][1]  # first sub-element of first list item
#add2[[1]][2]

# subset the list elements  https://statisticsglobe.com/get-second-sub-element-of-every-list-element-in-r
col2$AddressP1 <- sapply(add2, "[[", 1)
col2$AddressP2 <- sapply(add2, "[[", 2)

# split AddressP2 into ST / Zip5[-Zip4] on the space after state abbrev
col2 <- separate(col2, AddressP2, into = c("ST", "Zip"), sep = " ", remove = F)

# split Zip into Zip5 / Zip4 on the dash
col2 <- separate(col2, Zip, into = c("Zip", "Zip4"), sep = "-", remove = T)

# split AddressP1 into Street[, Suite] / City on the last comma
add3 <- strsplit(col2$AddressP1, ",\\s*(?=[^,]+$)", perl=TRUE)

col2$Address1 <- sapply(add3, "[[", 1)
#col2$City     <- sapply(add3, "[[", 2)   # Errors out  https://stackoverflow.com/questions/19120189/extract-second-subelement-of-every-element-in-a-list-while-ignoring-nas-in-sapp
col2$City     <- sapply(add3,function(x) x[2])

# col3 final output in desired column order.
col3 <- col2 %>% 
  select(DapipId, OpeId, IpedsUnitIds, LocationName, LocationType, ParentDapipId,
         Address1, City, ST, Zip, Zip4, AddressFull = Address, GeneralPhone, AdminName, AdminEmail, UpdateDate)
#View(col3)

write_csv(col3, "D:/opendataclean/ope_ed_gov_post_secondary_institutions.csv", na = "")
