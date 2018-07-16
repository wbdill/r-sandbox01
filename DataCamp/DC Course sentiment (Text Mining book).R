
# https://www.tidytextmining.com/tidytext.html

install.packages("tidyverse")
install.packages("tidytext")
install.packages("syuzhet")     # lexicons
install.packages("janeaustenr") # Jane Austin books

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
library(tidytext)

# https://www.tidytextmining.com/sentiment.html#sentiment-analysis-with-inner-join
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

library(tidyr)

# create index for every 80 lines, spread to + & - cols and get net sentiment
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

#graph net sentiment over time (index) for each book
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



#----- Project Gutenburg -------------------------------------------------------
install.packages("gutenbergr")
library(gutenbergr)

# Top 100 books: https://www.gutenberg.org/browse/scores/top
#  Let's get The Time Machine, The War of the Worlds, The Invisible Man, 
#and The Island of Doctor Moreau.
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# War and Peace is 2600  https://www.gutenberg.org/ebooks/2600
warpeace <- gutenberg_download(c(2600))
write.csv(warpeace, "D:/data/R/war_and_peace.csv")

# Save an object to a file
saveRDS(warpeace, file = "D:/data/R/war_and_peace.rds")
# Restore the object
warpeace <- readRDS(file = "D:/data/R/war_and_peace.rds")

#-------------------------------------------------------------------------------
original_books <- warpeace %>%
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



#----- apply a lexicon sentiment -----
sbooks <- tidy_books %>% 
  inner_join(get_sentiments("bing"))

swordcounts <- sbooks %>%
  group_by(word, sentiment) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  top_n(20, n)

ggplot(swordcounts, aes(reorder(word, n), n, fill=sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title="War and Peace")
  

#----- total negative vs positive -----
sbooks %>%
  group_by(word, sentiment) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  spread(sentiment, n) %>%
  ungroup() %>%
  summarize(tot_neg = sum(negative, na.rm = TRUE),
            tot_pos = sum(positive, na.rm = TRUE))


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
