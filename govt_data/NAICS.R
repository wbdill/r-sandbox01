library(tidyverse)
rm(list = ls())

sic <- read_delim("D:/opendata/sic_codes.txt", delim = "~")
# using write.csv rather than write_csv to be able to force quotes on the code column.
# unfortunatly Excel still fucks it up
write.csv(sic,"D:/opendata/sic_codes.csv", quote = 2, row.names = FALSE)
