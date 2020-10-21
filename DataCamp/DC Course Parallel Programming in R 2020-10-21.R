# DC Course Parallel Programming in R
# https://learn.datacamp.com/courses/parallel-programming-in-r
library(stringr)
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

#----- Ch 2 The parallel Package -----

#----- Ch 3 foreach, future.apply and Load Balancing  -----

#----- Ch 4 Random Numbers and Reproducibility -----