# https://datascienceineducation.com/c11.html

library(tidyverse)
library(here)
library(dataedu)
#install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)

raw_tweets <- dataedu::tt_tweets
glimpse(raw_tweets)

tweets <-
  raw_tweets %>%
  filter(lang == "en") %>%  #filter for English tweets
  select(status_id, text) %>%
  mutate(status_id = as.character(status_id))  # Convert the ID field to the character data type

tokens <- 
  tweets %>%
  unnest_tokens(output = word, input = text)

tokens 

data(stop_words)

tokens <- tokens %>% 
  anti_join(stop_words, by = "word")

tokens %>% 
  count(word, sort = TRUE)

get_sentiments("nrc")

nrc_pos <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

pos_tokens_count <- tokens %>% 
  inner_join(nrc_pos, by = "word") %>% 
  count(word, sort = TRUE)

pos_tokens_count %>% 
  filter(n >= 75) %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = dataedu_colors("darkblue")) +
  labs(title = "Count or Words Associated with Positivity",
       subtitle = "Tweets with hashtag #tidytuesday",
       caption = "Data: Twitter and NRC",
       x = "",
       y = "Count")
  theme_dataedu()

# find tweets with "dataviz" that had at least 1 positive word
dv_tokens <- tokens %>% 
  filter(word == "dataviz")

dv_tokens <- dv_tokens %>% 
  distinct(status_id)

pos_tokens <- tokens %>% 
  filter(word %in% nrc_pos$word) %>% 
  distinct(status_id)

dv_pos <- tweets %>%
  filter(status_id %in% dv_tokens$status_id) %>%  # Only tweets that have the dataviz status_id
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0))  # Is the status_id from our vector of positive word?

dv_pos %>% 
  count(positive) %>% 
  mutate(pct = n / sum(n))

# 11.11.3 looking at randomly selected tweets
pos_tweets <- tweets %>%
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0)) %>%
  filter(positive == 1)

tweets %>% 
  slice(1:4)

set.seed(2020)

pos_tweets %>% 
  sample_n(., size = 10) %>% 
  View()
