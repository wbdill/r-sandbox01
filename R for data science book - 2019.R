#----- R for Data Science (Hadley Wickham) -----
# https://r4ds.had.co.nz
#----- Chapter 5 -----
rm(list = ls())
library(tidyverse)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

count(diamonds, cut)

diamonds %>%
  filter(between(y, 3, 20))

diamonds %>%
  filter(between(y, 0, 3))

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

diamonds2 %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point()


install.packages("nycflights13")
library(nycflights13)
nycflights13::flights %>%
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + sched_min / 60) %>%
  ggplot(aes(x = sched_dep_time, y = ..density..)) +
  geom_freqpoly(aes(color=cancelled,
                    binwidth = 1/4))
ggplot(diamonds) + 
  geom_count(aes(x = cut, y = color))

diamonds %>%
  count(cut, color)

diamonds %>%
  count(cut, color) %>%
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

# two continuous vars
ggplot(diamonds) +
  geom_point(aes(x = carat, y = price), alpha = .02)

ggplot(diamonds) +
  geom_bin2d(aes(x = carat, y = price))
install.packages("hexbin")
library(hexbin)
ggplot(diamonds) +
  geom_hex(aes(x = carat, y = price))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

#----- Chapter 6 Workflow: Projects -----
# Ctrl + Shift F10 to restart R Studio
# Ctrl + Shift S to rerun current script

getwd()

#----- Chapter 10: Tibbles -----
library(tidyverse)
?tribble
tribble( 
  ~x, ~y, ~z,
  "Hello", 4, 5.5,
  "Test", 3, 8.8
  )
nycflights13::flights %>%
  print(n = 10, width = Inf)

?tibble
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df$x
?tibble::enframe
enframe(5:8)

#---- Chapter 11: Data Import -----
read_csv("a,b,c\n1,2,3\n4,5,6")  # skip = n, na = "-"

x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

read_csv("a,b,c\n1,2,.", na = ".")

x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x) # the set of import/parsing problems

?read_csv2
#----- 11.4.2
library(tidyverse)
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
str(challenge)
tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

# Sometimes it's easier to diagnose problems if you just read in all the columns as character vectors:
challenge2 <- read_csv(readr_example("challenge.csv"), 
 col_types = cols(.default = col_character())
)
# If you're reading a very large file, you might want to set n_max to a smallish number 
#like 10,000 or 100,000. That will accelerate your iterations while you eliminate common problems
challenge2 <- read_csv(readr_example("challenge.csv"), 
       n_max = 100,
       col_types = cols(.default = col_character())
)

#----- 11.5 Writing to a file
write_csv(challenge, "challenge.csv") # encodes in UTF-8 and dates as ISO 8601 format
write_tsv() # same as csv, but tab separated
write_excel_csv() # this writes a special character (a "byte order mark") at the start of the file which tells Excel that you're using the UTF-8 encoding.

# RDS is R's custom binary format.
write_rds(challenge, "challenge.rds") # wrapper for saveRDS() and preserves data tyeps
read_rds("challenge.rds")

# The feather package implements a fast binary file format that can be shared across programming languages:
# Feather tends to be faster than RDS and is usable outside of R. 
# RDS supports list-columns (which you'll learn about in many models); feather currently does not.
install.packages("feather")
library(feather)
write_feather(challenge, "chalenge.feather")
read_feather("chalenge.feather")

#----- 11.6 other readers
haven # SPSS, Stata and SAS files
readxl()
DBI
jsonlite 
xml2 
# further info: https://cran.r-project.org/doc/manuals/r-release/R-data.html

#-----
#----- 12 Tidy data -----
#-----
library(tidyverse)
pivot_longer()  # replacement for gather  https://cmdlinetips.com/2019/09/pivot_longer-and-pivot_wider-in-tidyr/
pivot_wider()   # replacement for spread

?separate()
unite()

tidyr::table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/", convert = TRUE)

# sep = 2 splits at 2nd character position.  Negative values start from end of string
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) %>%
  unite(newcol, century, year, sep = "")

#---
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
# both pivot_wider and complete() explicity show all missing values.
stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>%
  complete(year, qtr)

#----- Excel-like autofill for when NA means it should be ditto from cell above.
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>% 
  fill(person)

#----- 12.6 Case Study
# https://www.who.int/tb/country/data/download/en/
View(who)

who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )
# deprecated gather() equivalent
who %>%
  gather(key = "key", val = "cases", new_sp_m014:newrel_f65, na.rm = TRUE)

who1 %>% 
  count(key)

# fix column name inconsistency.  "newrel" should be "new_rel"
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

# split key into 3 cols
who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")

# select all cols but iso2, iso3 and -new (all cases are new, so no need to keep)
who4 <- who3 %>%
  select(-iso2, -iso3, -new)

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)

#--- all in one pipe
who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-iso2, -iso3, -new) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

#--- 12.6.1 Exercises
#----- 12.7 Non-tidy data

#-----
#----- 13 Relational data -----
#-----
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
airlines
airports %>% filter(faa == "BNA")
planes
weather

# check for uniqueness
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

# Add a row number surrogate key to flights since it has none.
View(flights)
flights2 <- flights %>% mutate(rownum = row_number())

#--- 13.4 Mutating joins

#-- 13.4.6 Exercises
flights %>%
  mutate(delay = arr_time - sched_arr_time) %>%
  group_by(dest) %>%
  mutate(avg_delay = mean(delay, na.rm = TRUE)) %>%
  select(year, month, day, dest, avg_delay) %>%
  View()

# quick map
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#----- 13.5 Filtering joins
semi_join(x, y) # keeps all x that match y
anti_join(x, y) # drops all x that match y

#----- 13.6 Join problems
# 1. identify PKs in each table
# 2. check that none of the PKs are missing
# 3. check for FK orphans

#----- 13.7 Set operations
# All these operations work with a complete row, comparing the values of every variable.
intersect(x, y) # return only observations in both x and y.
union(x, y)     # return unique observations in x and y.
setdiff(x, y)   # return observations in x, but not in y.
