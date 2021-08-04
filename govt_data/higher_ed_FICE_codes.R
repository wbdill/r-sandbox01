library(tidyverse)

# https://www.fau.edu/academic/freshman/fice/
fice <- read_fwf("C:/opendata/higher_ed_fice_codes.txt", fwf_widths( c(9, 55, 22, 2), c("fice_code", "org_name", "city", "state_abbrev") ), col_types = NULL)

str(fice)

fice2 <- distinct(fice, fice_code, .keep_all = TRUE)  # dedupe
write_csv(fice2, "C:/opendata/higher_ed_fice_codes.csv")

?read_fwf

fwf_sample <- readr_example("fwf-sample.txt")
writeLines(read_lines(fwf_sample))
