# DC Course String Manipulation with rtringr in R
# https://learn.datacamp.com/courses/string-manipulation-with-stringr-in-r
# 2020-10-07
library(stringr)
install.packages("rebus")
library(rebus)
rm(list = ls())

catcidents <- readRDS("data/catcidents.rds")
earnest <- read.delim("data/importance-of-being-earnest.txt")
adverbs <- readRDS("data/adverbs.rds")
narratives <- readRDS("data/narratives.rds")
dna <- readRDS("data/dna.rds")

#----- Ch 1 String Basics -----
# stringr and rebus packages
?writeLines
?cat
?Quotes  # Descriptions of the various uses of quoting in R.

est <- 1.342345675
format(est, digits = 3)    # scientific = TRUE|FALSE
formatC(est, format = "f", digits = 2)  # format = "f" | "e" | "g"  (fixed, scientific, sci if saves space)
?format


# Some vectors of numbers
percent_change  <- c(4, -1.91, 3.00, -5.002)
income <-  c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)


format(c(0.0011, 0.011, 1), digits = 1)  # Format c(0.0011, 0.011, 1) with digits = 1
format(c(1.0011, 2.011, 1), digits = 1)  # Format c(1.0011, 2.011, 1) with digits = 1
format(percent_change, digits = 2)  # Format percent_change to one place after the decimal point
format(income, digits = 2)  # Format income to whole numbers
format(p_values, scientific = FALSE)  # Format p_values in fixed format


formatted_income <- format(income, digits = 2)
formatted_income  # Print formatted_income
writeLines(formatted_income)  # Call writeLines() on the formatted income
trimmed_income <- format(income, digits = 2, trim = TRUE)  # Define trimmed_income
writeLines(trimmed_income)  # Call writeLines() on the trimmed_income
pretty_income <- format(income, digits = 2, big.mark = ",")  # Define pretty_income
writeLines(pretty_income)  # Call writeLines() on the pretty_income


# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)

formatC(x, format = "f", digits = 1)  # formatC() on x with format = "f", digits = 1
formatC(y, format = "f", digits = 1)  # formatC() on y with format = "f", digits = 1
formatC(percent_change, format = "f", digits = 1)  # Format percent_change to one place after the decimal point
pretty_percent <- formatC(percent_change, format = "f", digits = 1, flag = "+")  # percent_change with flag = "+"
formatC(p_values, format = "g", digits = 2)  # Format p_values using format = "g" and digits = 2

years <- c(2010, 2011, 2012, 2013)
paste("$", pretty_income)  # Add $ to pretty_income
paste(pretty_percent, "%")  # Add % to pretty_percent
year_percent <- paste(years, ": ", pretty_percent, "%", sep = "")  # Create vector with elements like 2010: +4.0%`
paste(year_percent, collapse = ", ")  # Collapse all years into single string


income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")  # Define the names vector
income <- c(72, 1030, 10292, 1189192)
pretty_income <- format(income, digits = 2, big.mark = ",")  # Create pretty_income
dollar_income <- paste("$", pretty_income, sep = "")  # Create dollar_income
formatted_names <- format(income_names, justify = "right")  # Create formatted_names
rows <- paste(formatted_names, dollar_income, sep = "   ")  # Create rows
writeLines(rows)  # Write rows

toppings <- c("anchovies", "artichoke", "bacon", "breakfast bacon", "Canadian bacon", "cheese", "chicken", "chili peppers", "feta", "garlic", "green peppers", "grilled onions", "ground beef", "ham", "hot sauce", "meatballs", "mushrooms", "olives", "onions", "pepperoni", "pineapple", "sausage", "spinach", "sun-dried tomato", "tomatoes")
my_toppings <- sample(toppings, size = 3)  # Randomly sample 3 toppings
my_toppings  # Print my_toppings
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")  # Paste "and " to last element: my_toppings_and
these_toppings <- paste(my_toppings_and, collapse = ", ")  # Collapse with comma space: these_toppings
my_order <- paste("I want to order a pizza with ", these_toppings, ".", sep = "")  # Add rest of sentence: my_order
writeLines(my_order)  # Order pizza with writeLines()



#----- Ch 2 Introduction to stringr -----
rm(list = ls())
library(stringr)
library(babynames)
library(dplyr)

str_c()
str_length()
str_sub()
str_detect()
str_subset()
str_count()
str_split()
str_replace()

my_toppings <- c("cheese", NA, NA)
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")

my_toppings_and  # Print my_toppings_and
my_toppings_str <- str_c(c("", "", "and "), my_toppings)  # Use str_c() instead of paste(): my_toppings_str
my_toppings_str  # Print my_toppings_str
paste(my_toppings_and, collapse = ", ")  # paste() my_toppings_and with collapse = ", "
str_c(my_toppings_str, collapse = ", ")  # str_c() my_toppings_str with collapse = ", "


# Extracting vectors for boys' and girls' names
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name

head(boy_names)  # Take a look at a few boy_names
boy_length <- str_length(boy_names)  # Find the length of all boy_names
head(boy_length)  # Take a look at a few lengths
girl_length <- str_length(girl_names) # Find the length of all girl_names
mean(girl_length) - mean(boy_length)  # Find the difference in mean length
head(str_length(factor(boy_names)))  # Confirm str_length() works with factors

table(boy_length)
hist(boy_length)

boy_first_letter <- str_sub(boy_names, 1, 1)  # Extract first letter from boy_names
table(boy_first_letter)  # Tabulate occurrences of boy_first_letter

boy_last_letter <- str_sub(boy_names, -1, -1)   # Extract the last letter in boy_names, then tabulate
table(boy_last_letter)

girl_first_letter <- str_sub(girl_names, 1, 1)   # Extract the first letter in girl_names, then tabulate
table(girl_first_letter)

girl_last_letter <- str_sub(girl_names, -1, -1)  # Extract the last letter in girl_names, then tabulate
table(girl_last_letter)


contains_zz <- str_detect(boy_names, pattern = fixed("zz"))  # Look for pattern "zz" in boy_names
str(contains_zz)  # Examine str() of contains_zz
sum(contains_zz)  # How many names contain "zz"?
boy_names[contains_zz]  # Which names contain "zz"?


str_subset(boy_names, pattern = fixed("zz"))  # Find boy_names that contain "zz"
str_subset(girl_names, pattern = fixed("zz"))  # Find girl_names that contain "zz"
starts_U <- str_subset(girl_names, pattern = fixed("U"))  # Find girl_names that contain "U"
starts_U
str_subset(starts_U, pattern = fixed("z"))  # Find girl_names that contain "U" and "z"


number_as <- str_count(girl_names, pattern = fixed("a"))  # Count occurrences of "a" in girl_names
number_As <- str_count(girl_names, pattern = fixed("A"))  # Count occurrences of "A" in girl_names
hist(number_as)  # Histograms of number_as and number_As
hist(number_As)  
total_as <- number_as + number_As  # Find total "a" + "A"
girl_names[total_as > 4]  # girl_names with more than 4 a's


date_ranges <- c("23.01.2017 - 29.01.2017", "30.01.2017 - 06.02.2017")  # Some date data
split_dates <- str_split(date_ranges, pattern = fixed(" - "))  # Split dates using " - "
split_dates

split_dates_n <- str_split(date_ranges, pattern = fixed(" - "), n = 2, simplify = TRUE)  # Split dates with n and simplify specified
split_dates_n

start_dates <- split_dates_n[ ,1]  # Subset split_dates_n into start_dates and end_dates
str_split(start_dates, pattern = fixed("."), n = 3, simplify = TRUE)  # Split start_dates into day, month and year pieces

both_names <- c("Box, George", "Cox, David")
both_names_split <- str_split(both_names, pattern = fixed(", "), simplify = TRUE)  # Split both_names into first_names and last_names
first_names <- both_names_split[, 2]  # Get first names
last_names <- both_names_split[, 1]  # Get last names

lines <- c("The table was a large one, but the three were all crowded together at one corner of it:", "\"No room! No room!\" they cried out when they saw Alice coming.", "\"There’s plenty of room!\" said Alice indignantly, and she sat down in a large arm-chair at one end of the table.")
words <- str_split(lines, pattern = fixed(" "))  # Split lines into words
lapply(words, length)  # Number of words per line
word_lengths <- lapply(words, str_length)  # Number of characters in each word
lapply(word_lengths, mean)  # Average word length per line

ids <- c("ID#: 192", "ID#: 118", "ID#: 001")  # Some IDs
id_nums <- str_replace(ids, pattern = fixed("ID#: "), "")  # Replace "ID#: " with ""
id_ints <- as.numeric(id_nums)  # Turn id_nums into numbers

phone_numbers <- c("510-555-0123", "541-555-0167")  # Some (fake) phone numbers
str_replace(phone_numbers, pattern = fixed("-"), " ")  # Use str_replace() to replace "-" with " "
str_replace_all(phone_numbers, pattern = fixed("-"), " ")  # Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers, pattern = fixed("-"), ".")  # Turn phone numbers into the format xxx.xxx.xxxx

genes <- readRDS("data/dna.rds")
str_length(genes)  # Find the number of nucleotides in each sequence
str_count(genes, pattern = fixed("A"))  # Find the number of A's occur in each sequence
str_subset(genes, pattern = fixed("TTTTTT"))  # Return the sequences that contain "TTTTTT"
str_replace_all(genes, pattern = fixed("A"), "_")  # Replace all the "A"s in the sequences with a "_"


names <- c("Diana Prince", "Clark Kent")  # Define some full names
names_split <- str_split(names, pattern = fixed(" "), simplify = TRUE)  # Split into first and last names
abb_first <- str_sub(names, 1, 1)  # Extract the first letter in the first name
str_c(abb_first, ". ", names_split[ ,2])  # Combine the first letter ". " and last name

library(babynames)
library(dplyr)
babynames_2014 <- filter(babynames, year == 2014)
all_names <- babynames_2014$name  # Use all names in babynames_2014
last_two_letters <- str_sub(all_names, -2)  # Get the last two letters of all_names
ends_in_ee <- str_detect(last_two_letters, pattern = fixed("ee"))  # Does the name end in "ee"?
sex <- babynames_2014$sex[ends_in_ee]  # Extract rows and "sex" column
table(sex)  # Display result as a table

#----- Ch 3 Pattern matching with regular expressions -----




#----- Ch 4 More advanced matching and manipulation -----


#----- Ch 5 Case studies -----


