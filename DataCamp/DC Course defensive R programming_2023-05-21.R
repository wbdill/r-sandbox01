# DC Course - Defensive R Programming

old <- old.packages()
nrow(old)


nnn <- getNamespaceExports("ggplot2")


# Load the dplyr and conflicted packages
library("dplyr")
library("conflicted")

# Prefer the dplyr version of the lag function
conflict_prefer("lag", "dplyr")

# This should return NA, 1, 2, 3
lag(1:4)

stats::lag(1:4)
dplyr::lag(1:4)

#----- Ch 2: 