# ----- DataCamp - sentiment analysis -----
install.packages("tidyverse")
install.packages("tidytext")
install.packages("syuzhet")     # lexicons
install.packages("janeaustenr") # Jane Austin boks

#----- lexicons ----------------------------------------------------------------
library(syuzhet)

# 4 lexicons: loughran, afinn, bing, nrc
get_sentiments("loughran") # pos/neg
get_sentiments("bing")     #pos/neg
get_sentiments("afinn")    # +/- score
get_sentiments("nrc")      #categories ex: trust, fear, anger

#------- Jane Austin book load -------------------------------------------------
rm(list = ls())
library(tidyverse)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

#-------------------------------------------------------------------------------

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative)