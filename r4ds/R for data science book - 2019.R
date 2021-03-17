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

#11.4 Parsing a file
x <- read_csv("a,b,c\n1,2,.", na = ".")
install.packages("feather")
library(feather)
write_feather(mtcars, "x_mtcars.feather")
write_csv(mtcars, "x_mtcars.csv")
mtcars


