# DC Course String Manipulation with rtringr in R
# https://learn.datacamp.com/courses/string-manipulation-with-stringr-in-r
# 2020-10-07
library(stringr)
install.packages("rebus")
library(rebus)
rm(list = ls())

catcidents <- readRDS("data/catcidents.rds")
earnest <- readLines("data/importance-of-being-earnest.txt")
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
library(babynames)
library(dplyr)
library(rebus)
x <- c("cat", "coat", "scotland", "tic toc")  # Some strings to practice with
END # Print END
str_view(x, pattern = START %R% "c")  # Run me
str_view(x, pattern = START %R% "co")  # Match the strings that start with "co" 
str_view(x, pattern = END %R% "at")  # Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)  # Match the strings that end with "at"
str_view(x, pattern = START %R% "cat" %R% END)  # Match the string that is exactly "cat"
str_view(x, pattern = exactly("cat"))  
str_view(x, pattern = ANY_CHAR %R% "t")  # Match two characters, where the second is a "t"
str_view(x, pattern = "t" %R% ANY_CHAR)  # Match a "t" followed by any character
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)  # Match two characters
str_view(x, pattern = exactly(ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR))  # Match a string with exactly three characters


babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name

pattern <- "q" %R% ANY_CHAR
names_with_q <- str_subset(boy_names, pattern = pattern)  # Find names that have the pattern
length(names_with_q)  # How many names were there?

part_with_q <- str_extract(boy_names, pattern)  # Find part of name that matches pattern
table(part_with_q)  # Get a table of counts

count_of_q <- str_count(boy_names, pattern)  # Did any names have the pattern more than once?
table(count_of_q)  # Get a table of counts

with_q <- str_detect(boy_names, pattern)  # Which babies got these names?
mean(with_q)  # What fraction of babies got these names?


whole_names <- or("Jeffrey", "Geoffrey")  # Match Jeffrey or Geoffrey
str_view(boy_names, pattern = whole_names, match = TRUE)

common_ending <- or("Je", "Geo") %R%  "ffrey"  # Match Jeffrey or Geoffrey, another way
str_view(boy_names, pattern = common_ending, match = TRUE)

by_parts <- or("Je", "Geo") %R% "ff" %R% or("ry", "ery", "rey", "erey")  # Match with alternate endings
str_view(boy_names, pattern = by_parts, match = TRUE)

ckath <- START %R% or("Cath", "Kath")  # Match names that start with Cath or Kath
str_view(girl_names, pattern = ckath, match = TRUE)


x <- c("grey sky", "gray elephant")
vowels <- char_class("aeiouAEIOU")  # Create character class containing vowels
vowels  # Print vowels
str_view(x, vowels)  # See vowels in x with str_view()
str_view_all(x, vowels)  # See vowels in x with str_view_all()

num_vowels <- str_count(boy_names, vowels)  # Number of vowels in boy_names
name_length <- str_length(boy_names)  # Number of characters in boy_names
mean(num_vowels)  # Calc mean number of vowels
mean(num_vowels / name_length)  # Calc mean fraction of vowels per name


vowels <- char_class("aeiouAEIOU")  # Vowels from last exercise
str_view(boy_names,   # See names with only vowels
         pattern = exactly(one_or_more(vowels)), 
         match = TRUE)

not_vowels <- negated_char_class("aeiouAEIOU")  # Use `negated_char_class()` for everything but vowels
str_view(boy_names,   # See names with no vowels
         pattern = exactly(one_or_more(not_vowels)), 
         match = TRUE)

# rebus shortcuts:  DGT (digit) WRD (word) SPC (whitespace char)

contact <- c("Call me at 555-555-0191","123 Main St", "(555) 555 0191", "Phone: 555.555.0191 Mobile: 555.555.0192")
three_digits <- DGT %R% DGT %R% DGT  # Create a three digit pattern
str_view_all(contact, pattern = three_digits)  # Test it

separator <- char_class("-.() ")  # Create a separator pattern
str_view_all(contact, pattern = separator)  # Test it

three_digits <- DGT %R% DGT %R% DGT  # Use these components
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")

phone_pattern <- optional(OPEN_PAREN) %R%  # Create phone pattern
  three_digits %R%
  zero_or_more(separator) %R%
  three_digits %R% 
  zero_or_more(separator) %R%
  four_digits
str_view_all(contact, pattern = phone_pattern) # Test it  
str_extract(contact, pattern = phone_pattern)  # Extract phone numbers
str_extract_all(contact, pattern = phone_pattern)  # Extract ALL phone numbers


narratives <- c("19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ","31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ","ANKLE STR. 82 YOM STRAINED,ANKLE GETTING OUT OF BED ","TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*","10YOM CUT THUMB ON METAL TRASH CAN DX,AVULSION OF SKIN OF THUMB ","53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ","13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON,RADIATOR DX LACERATION","14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ","55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ","5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN")
age <- optional(DGT) %R% DGT  # Pattern to match one or two digits
str_view(narratives, pattern = age)  # Test it

age <- DGT %R% optional(DGT)  # Use this pattern
unit <- optional(SPC) %R% or("YO", "YR", "MO")  # Pattern to match units 
str_view(narratives, pattern = age %R% unit)  # Test pattern with age then units

gender <- optional(SPC) %R% or("M", "F")  # Pattern to match gender
str_view(narratives, pattern = age %R% unit %R% gender)  # Test pattern with age then units then gender

age_gender <- str_extract(narratives, pattern = age %R% unit %R% gender)  # Extract age, unit, gender
as.numeric(str_extract(age_gender, pattern = age))  # Extract age and make numeric
# Replace age and units with ""
genders <- str_remove(age_gender, pattern = age %R% unit)
genders <- str_remove_all(genders, pattern = one_or_more(SPC))  # Replace extra spaces

ages_numeric <- as.numeric(str_extract(age_gender, age))  # Numeric ages, from previous step
time_units <- str_extract(age_gender, pattern = unit)  # Extract units 
time_units_clean <- str_extract(time_units, pattern = WRD)  # Extract first word character  (why does WRD pattern return only 1st char?)
ifelse(time_units_clean == "Y", ages_numeric, ages_numeric / 12)  # Turn ages in months to years

load("K:/tmp/injuries.rda")
str(injuries)
head(injuries)
glimpse(injuries)

#----- Ch 4 More advanced matching and manipulation -----
library(stringr)
library(stringi)
library(rebus)

hero_contacts <- c("(wolverine@xmen.com)", "wonderwoman@justiceleague.org", "thor@avengers.com")

email <- capture(one_or_more(WRD)) %R%   # Capture parts between @ and . and after .
  "@" %R% capture(one_or_more(WRD)) %R% 
  DOT %R% capture(one_or_more(WRD))
str_view(hero_contacts, pattern = email)  # Check match hasn't changed

email_parts <- str_match(hero_contacts, pattern = email)  # Pull out match and captures
email_parts
host <- email_parts[,3]  # Save host
host


contact <- c("Call me at 555-555-0191","123 Main St", "(555) 555 0191", "Phone: 555.555.0191 Mobile: 555.555.0192")
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R%   # Add capture() to get digit parts
  capture(three_digits) %R% zero_or_more(separator) %R%
  capture(four_digits)

phone_numbers <- str_match(contact, phone_pattern)  # Pull out the parts with str_match()
str_c("(", phone_numbers[,2], ") ", phone_numbers[,3], "-",phone_numbers[,4])  # Put them back together


narratives <- c("19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ","31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ","ANKLE STR. 82 YOM STRAINED,ANKLE GETTING OUT OF BED ","TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*","10YOM CUT THUMB ON METAL TRASH CAN DX,AVULSION OF SKIN OF THUMB ","53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ","13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON,RADIATOR DX LACERATION","14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ","55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ","5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN")
# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

str_match(narratives, pattern) # Pull out from narratives

pattern2 <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%  # Edit to capture just Y and M in units
  optional(SPC) %R% capture(or("M", "F"))

str_view(narratives, pattern2)  # Check pattern
str_match(narratives, pattern2)  # Pull out pieces

# backreferences  REF1 ... REF9
boy_names <- str_to_lower(boy_names)  # converted to lower for these exercises
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1  # Names with three repeated letters
str_view(boy_names, pattern = repeated_three_times, match = TRUE)

pair_of_repeated <- capture(LOWER %R% LOWER) %R% REF1  # Names with a pair of repeated letters
str_view(boy_names, pattern = pair_of_repeated, match = TRUE)  # Test it

pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1  # Names with a pair that reverses
str_view(boy_names, pattern = pair_that_reverses, match = TRUE)  # Test it

four_letter_palindrome <- exactly(capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1)  # Four letter palindrome names
str_view(boy_names, pattern = four_letter_palindrome, match = TRUE)  # Test it


# replacing with backreferences
contact  # View text containing phone numbers
str_replace(contact, DGT, "X")  # Replace digits with "X"
str_replace_all(contact, DGT, "X")  # Replace all digits with "X"
str_replace_all(contact, DGT, c("X", ".", "*", "_"))  # Replace all digits with different symbol

pattern <-  capture(one_or_more(WRD) %R% "ING")   # Build pattern to match words ending in "ING"
str_view(narratives, pattern)

str_replace(narratives, pattern, str_c("CARELESSLY", REF1, sep = " "))  # Test replacement

adverbs_10 <- c("VERY", "VAGUELY", "PROMPTLY", "POORLY", "RARELY", "QUIETLY", "HUNGRILY", "LIMPLY", "DEEPLY", "EVENLY")  # One adverb per narrative

str_replace(narratives, capture(pattern), str_c(adverbs_10, REF1, sep = " "))  # Replace "***ing" with "adverb ***ly"

# Unicode in R    unicode.org/charts
"\U03BC"  # mu
as.hexmode(utf8ToInt("a"))


# Names with builtin accents
(tay_son_builtin <- c(
  "Nguy\u1ec5n Nh\u1ea1c", 
  "Nguy\u1ec5n Hu\u1ec7",
  "Nguy\u1ec5n Quang To\u1ea3n"
))

tay_son_separate <- stri_trans_nfd(tay_son_builtin)  # Convert to separate accents
tay_son_separate  # Verify that the string prints the same
str_view_all(tay_son_separate, ANY_CHAR)  # Match all accents
str_view_all(tay_son_separate, GRAPHEME)  # Match all graphemes
tay_son_builtin <- stri_trans_nfc(tay_son_builtin)  # Combine the diacritics with their letters
tay_son_builtin
str_view_all(tay_son_builtin, GRAPHEME)  # View all the graphemes in tay_son_builtin

#----- Ch 5 Case studies -----
rm(list = ls())
library(stringr)
library(stringi)
library(rebus)
earnest <- readLines("data/importance-of-being-earnest.txt")
#stri_read_lines() is much faster
earnest <- stringi::stri_read_lines("data/importance-of-being-earnest.txt")  

start <- str_which(earnest, fixed("START OF THE PROJECT"))  # Detect start and end lines
end <- str_which(earnest, fixed("END OF THE PROJECT"))
earnest_sub  <- earnest[(start + 1):(end - 1)]  # Get rid of gutenberg intro text

lines_start <- str_which(earnest_sub, fixed("FIRST ACT"))  # Detect first act
intro_line_index <- 1:(lines_start - 1)  # Set up index
intro_text <- earnest_sub[intro_line_index]  # Split play into intro and play
play_text <- earnest_sub[-intro_line_index]
writeLines(play_text[1:20])  # Take a look at the first 20 lines

empty <- stri_isempty(play_text)
play_lines <- play_text[!empty]

pattern_1 <- START %R% one_or_more(WRD) %R% DOT  # Pattern for start, word then .
str_view(play_lines, pattern_1, match = TRUE)   # Test pattern_1
str_view(play_lines, pattern_1, match = FALSE)

pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT  # Pattern for start, word then .
str_view(play_lines, pattern_2, match = TRUE)   # Test pattern_1
str_view(play_lines, pattern_2, match = FALSE)


pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT  # Pattern from last step
lines <- str_subset(play_lines, pattern = pattern_2)  # Get subset of lines that match
who <- str_extract(lines, pattern = pattern_2)  # Extract match from lines
unique(who)  # Let's see what we have


# Create vector of characters
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble", "Merriman", "Lady Bracknell", "Miss Prism")

pattern_3 <- START %R% or1(characters) %R% DOT  # Match start, then character name, then .
str_view(play_lines, pattern = pattern_3, match = TRUE)  # View matches of pattern_3
str_view(play_lines, pattern = pattern_3, match = FALSE)  # View non-matches of pattern_3


lines <- str_subset(play_lines, pattern = pattern_3)  # Pull out matches
who <- str_extract(lines, pattern = pattern_3)  # Extract match from lines
unique(who)  # Let's see what we have
table(who)  # Count lines per character

rm(list = ls())
catcidents <- readRDS("data/catcidents.rds")

head(catcidents)  # catcidents has been pre-defined
whole_dog_pattern <- whole_word("DOG")  # Construct pattern of DOG in boundaries
str_view(catcidents, whole_dog_pattern, match = TRUE)  # See matches to word DOG

catcidents_upper <- str_to_upper(catcidents)  # Transform catcidents to upper case
str_view(catcidents_upper, whole_dog_pattern, match = TRUE)    # View matches to word "DOG" again

has_dog <- str_detect(catcidents_upper, whole_dog_pattern)  # Which strings match?
catcidents[has_dog]  # Pull out matching strings in original 


str_view(catcidents, pattern = "TRIP", match = TRUE)  # View matches to "TRIP"
trip_pattern <- regex("TRIP", ignore_case = TRUE)  # Construct case insensitive pattern
str_view(catcidents, pattern = trip_pattern, match = TRUE)  # View case insensitive matches to "TRIP"
trip <- str_subset(catcidents, trip_pattern)  # Get subset of matches
str_extract(trip, trip_pattern)  # Extract matches


library(stringi)
cat5 <- catcidents[1:5]  # Get first five catcidents
writeLines(cat5)  # Take a look at original
writeLines(str_to_title(cat5))  # Transform to title case
writeLines(stri_trans_totitle(cat5))  # Transform to title case with stringi  (identical output)
writeLines(stri_trans_totitle(cat5, type = "sentence"))  # Transform to sentence case with stringi

