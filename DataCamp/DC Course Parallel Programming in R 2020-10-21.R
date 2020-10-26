# DC Course Parallel Programming in R
# https://learn.datacamp.com/courses/parallel-programming-in-r
library(stringr)
library(ggplot2)
library(readr)

rm(list = ls())
jane <- readRDS("data/jane_austen_words.RDS")
migration <- read.csv("data/USmigrationAR1.csv")
obama <- readLines("data/obama.txt")


#----- Ch 1 Can I Run My Application in Parallel? -----

#--- bunch of setup functions to replicate env or DC
austen_books <- function() {
  books <- list(`Sense & Sensibility` = janeaustenr::sensesensibility,
                `Pride & Prejudice` = janeaustenr::prideprejudice,
                `Mansfield Park` = janeaustenr::mansfieldpark,
                Emma = janeaustenr::emma,
                `Northanger Abbey` = janeaustenr::northangerabbey,
                Persuasion = janeaustenr::persuasion
                )
  ret <- data.frame(text = unlist(books, use.names = FALSE), stringsAsFactors = FALSE)
  ret$book <- factor(rep(names(books), sapply(books, length)))
  ret$book <- factor(ret$book, levels = unique(ret$book))
  structure(ret, class = c("tbl_df", "tbl", "data.frame"))
}
boundary <- function(type = c("character", "line_break", "sentence", "word"), skip_word_none = NA, ...) {
  type <- match.arg(type)
  if(identical(skip_word_none, NA)) {
    skip_word_none <- type == "word"
  }
  options <- stringi::stri_opts_brkiter(type = type, skip_word_none = skip_word_none, ...)
  structure(character(), options= options, class = c("boundary", "pattern", "character"))
}
extract_words <- function(book_name) {
  text <- subset(austen_books(), book == book_name)$text
  stringr::str_extract_all(text, boundary("word")) %>% unlist %>% tolower
}
janeausten_words <- function() {
  books <-  austen_books()$book %>% unique %>% as.character
  words <- sapply(books, extract_words) %>% unlist
  words
}
select_words <- function(letter, words, min_length = 1) {
  min_lengh_words <- words[nchar(words) >= min_length]
  grep(paste0("^", letter), min_lengh_words, value = TRUE)
}
max_frequency <- function(letter, words, min_length = 1) {
  w <- select_words(letter, words = words, min_length = min_length)
  frequency <- table(w)
  frequency[which.max(frequency)]
}
# \\\ end setup functions ///

words <- janeausten_words()  # Vector of words from all six books
head(words)

max_frequency(letter = "a", words = words, min_length = 5)  # Most frequent "a"-word that is at least 5 chars long

result <- lapply(letters, max_frequency, words = words, min_length = 5) %>% unlist()  # Partitioning
barplot(result, las = 2)  # Barplot of result


mean_of_rnorm <- function(n) {  # Complete the function definition
  random_numbers <- rnorm(n)  # Generate normally distributed random numbers
  mean(random_numbers)  # Calculate the mean of the random numbers
}
mean_of_rnorm(100)  # Try it out

n_numbers_per_replicate <- 10000
n_replicates <- 50

result <- rep(NA, n_replicates)  # Create a vector to store the results.  Pre-size it so we don't have to grow it.
set.seed(123)  # Set the random seed to 123

?seq_len
for ( iter in seq_len(n_replicates) ) {  # Set up a for loop with iter from 1 to n_replicates
  result[iter] <- mean_of_rnorm(n_numbers_per_replicate)  # Call mean_of_rnorm with n_numbers_per_replicate
}
hist(result)  # View the result


?sapply
n <- rep(n_numbers_per_replicate, n_replicates)  # Repeat n_numbers_per_replicate, n_replicates times
result <- sapply(  # Repeat n_numbers_per_replicate, n_replicates times
  n,   # The vectorized argument to pass
  mean_of_rnorm  # The function to call
)
hist(result)  # View the results


#write.csv(ar1est, "data/parallel_ar1est.csv", row.names = FALSE)
ar1est <- readr::read_csv("data/parallel_ar1est.csv")

# setup functions...
  ar1_one_value <- function(est, r) { est['mu'] + est['phi'] * (r - est['mu']) + rnorm(1, sd = est['sigma']) }
  ar1_one_trajectory <- function(est, rate0, len = 15) {
    trajectory <- rep(NA, len)
    rate <- rate0
    for (time in seq_len(len)) {
      trajectory[time] <- ar1_one_value(est, r = rate)
      rate <- trajectory[time]
    }
    trajectory
  }
  show_migration <- function(trajs) {
    df <- data.frame(time = seq(2020, by = 5, len = ncol(trajs)),
                     migration_rate = apply(trajs, 2, median),
                     lower = apply(trajs, 2, quantile, 0.1),
                     upper = apply(trajs, 2, quantile, 0.9)
    )
    g <- ggplot(df, aes(x = time, y = migration_rate)) + 
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + 
      geom_line()
    print(g)
  }
# end setup functions

ar1_block_of_trajectories <- function(id, rate0 = 0.015, traj_len = 15, block_size = 10) {
  trajectories <- matrix(NA, nrow = block_size, ncol = traj_len)
  for (i in seq_len(block_size)) 
    trajectories[i,] <- ar1_one_trajectory(unlist(ar1est[id, ]), 
                                           rate0 = rate0, len = traj_len)
  trajectories
}
# Function definition of ar1_multiple_blocks_of_trajectories()
ar1_multiple_blocks_of_trajectories <- function(ids, ...) {
  trajectories_by_block <- lapply(ids, ar1_block_of_trajectories, ...)  # Call ar1_block_of_trajectories() for each ids
  do.call(rbind, trajectories_by_block)  # rbind results
}

traj_ids <- seq_len(nrow(ar1est))  # Create a sequence from 1 to number of blocks

trajs <- ar1_multiple_blocks_of_trajectories (  # Generate trajectories for all rows of the estimation dataset
  ids = traj_ids, rate0 = 0.015,  # Apply ar1_multiple_blocks_of_trajectories() to traj_ids with 0.015 as rate0, block size 10, and 15 time periods.
  block_size = 10, traj_len = 15
)
dim(trajs)  #10000 row 15 col matrix

show_migration(trajs)

# packages: parallel (core), foreach, future.apply
# snow, snowFT, snowfall (older outdated? packages)
# future

library(parallel)
ncores <- detectCores(logical = FALSE)
n <- ncores:1
lapply(n, rnorm, mean = 10, sd = 2)  # Use lapply to call rnorm for each n, setting mean to 10 and sd to 2 

cl <- makeCluster(ncores)
clusterApply(cl, n, rnorm, mean = 10, sd = 2)
stopCluster(cl)


# Evaluate partial sums in parallel
cl <- makeCluster(ncores)
part_sums <- clusterApply(cl, x = c(1, 51), fun = function(x) sum(x:(x + 49)))
total <- sum(unlist(part_sums))  # Total sum
total == sum(1:100)  # Check for correctness
stopCluster(cl)


cl <- makeCluster(ncores)  # Create a cluster and set parameters
n_replicates <- 50
n_numbers_per_replicate <- 10000

# Parallel evaluation on n_numbers_per_replicate, n_replicates times
means <- clusterApply(cl, 
             x = rep(n_numbers_per_replicate, n_replicates), 
             fun = mean_of_r<norm)
hist(unlist(means))  # View results as histogram
stopCluster(cl)

#----- Ch 2 The parallel Package -----

#----- Ch 3 foreach, future.apply and Load Balancing  -----

#----- Ch 4 Random Numbers and Reproducibility -----
