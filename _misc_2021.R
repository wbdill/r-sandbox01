library(tidyverse)

dat <- data.frame(year = rep(seq(2011,2020), each = 3), 
                  location = rep(c("beach", "park", "lake"), 10), 
                  N = round(runif(30, 10, 100), 0))
dat

datwide <- dat %>% pivot_wider(names_from = location,
                               values_from = N)

datlong <- datwide %>% pivot_longer(cols = c("beach", "park", "lake"),
                                names_to = "location",
                                values_to = "N")


#----- generate several files for loading -----
genfile <- function(year) {
  names <- c("James", "Mary", "John", "Patricia", "Robert", "Jennifer", "Michael", "Linda", "William", "Elizabeth", 
             "David", "Barbara", "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah", "Charles", "Karen", 
             "Christopher", "Nancy", "Daniel", "Lisa", "Matthew", "Margaret", "Anthony", "Betty", "Donald", "Sandra", 
             "Mark", "Ashley", "Paul", "Dorothy", "Steven", "Kimberly", "Andrew", "Emily", "Kenneth", "Donna", "Joshua", 
             "Michelle", "Kevin", "Carol", "Brian", "Amanda", "George", "Melissa", "Edward", "Deborah", "Ronald", 
             "Stephanie", "Timothy", "Rebecca", "Jason", "Laura", "Jeffrey", "Sharon", "Ryan", "Cynthia", "Jacob", 
             "Kathleen", "Gary", "Amy", "Nicholas", "Shirley", "Eric", "Angela", "Jonathan", "Helen", "Stephen", "Anna", 
             "Larry", "Brenda", "Justin", "Pamela", "Scott", "Nicole", "Brandon", "Samantha", "Benjamin", "Katherine", 
             "Samuel", "Emma", "Frank", "Ruth", "Gregory", "Christine", "Raymond", "Catherine", "Alexander", "Debra", 
             "Patrick", "Rachel", "Jack", "Carolyn", "Dennis", "Janet", "Jerry", "Virginia", "Tyler", "Maria", "Aaron", 
             "Heather", "Jose", "Diane", "Henry", "Julie", "Adam", "Joyce", "Douglas", "Victoria", "Nathan", "Kelly", 
             "Peter", "Christina", "Zachary", "Lauren", "Kyle", "Joan", "Walter", "Evelyn", "Harold", "Olivia", "Jeremy", 
             "Judith", "Ethan", "Megan", "Carl", "Cheryl", "Keith", "Martha", "Roger", "Andrea", "Gerald", "Frances", 
             "Christian", "Hannah", "Terry", "Jacqueline", "Sean", "Ann", "Arthur", "Gloria", "Austin", "Jean", "Noah", 
             "Kathryn", "Lawrence", "Alice", "Jesse", "Teresa", "Joe", "Sara", "Bryan", "Janice", "Billy", "Doris", 
             "Jordan", "Madison", "Albert", "Julia", "Dylan", "Grace", "Bruce", "Judy", "Willie", "Abigail", "Gabriel", 
             "Marie", "Alan", "Denise", "Juan", "Beverly", "Logan", "Amber", "Wayne", "Theresa", "Ralph", "Marilyn", "Roy", 
             "Danielle", "Eugene", "Diana", "Randy", "Brittany", "Vincent", "Natalie", "Russell", "Sophia", "Louis", "Rose", 
             "Philip", "Isabella", "Bobby", "Alexis", "Johnny", "Kayla", "Bradley", "Charlotte")
  n = 10000
  dt <- data.frame(
    year = rep.int(year, n),
    name = sample(names, replace = T, size = n),
    grade = sample(c(1,2,3,4,5,6,7,8,9,10,11,12), replace = T, size = n),
    value1 = runif(n, 0, 100),
    is_active = sample(c(0,1), replace = T, size = n)
  )
  write_csv(dt, paste0("file", year, ".csv"))
  
}
genfile(2010)
genfile(2011)
genfile(2012)
genfile(2013)
genfile(2014)
genfile(2015)
genfile(2016)
genfile(2017)
genfile(2018)
genfile(2019)

# read into a single merged data frame
df <- dir("merge/", full.names = T) %>% map_df(read_csv)

# read into a tibble with one row per file.  Cell 2 contains a sub-table
dt <- tibble(
  file = dir("merge/", full.names = T),
  data = map(file, read_csv)
)

dt %>% transmute(
  x = data,
  file = str_replace_all(file, "csv", "txt")
) %>% pmap(write_tsv)
