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

#******************************************************************************

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative)


#******************************************************************************
# ----- Pop songs -----

# https://www.rdocumentation.org/packages/billboard/versions/0.1.0
# download lyrics.rda from
# https://github.com/mikkelkrogsholm/billboard/tree/master/data

load("C:/Data/R/lyrics.rda")
str(lyrics)

tidy_lyrics <- lyrics %>%
  unnest_tokens(word, lyrics)

head(tidy_lyrics)

# get total words per song (title) with count() and rename "n" to "total_words"
totals <- tidy_lyrics %>%
  count(title) %>%
  rename(total_words = n)

# stitch totals back with lyric words
lyric_counts <- tidy_lyrics %>%
  left_join(totals, by = "title")

# join to "nrc" sentiment lexicon
lyric_sentiment <- lyric_counts %>%
  inner_join(get_sentiments("nrc"))

head(lyric_sentiment)

lyric_sentiment %>%
  count(title, sentiment, sort = TRUE)

# songs with most highest % of negative sentiment words
lyric_sentiment %>%
  count(title, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  filter(sentiment == "negative") %>%
  arrange(desc(percent))

lyric_sentiment %>%
  count(title, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  filter(sentiment == "positive", total_words > 1) %>%
  arrange(desc(percent))
 
#----- plotting of pop songs ---------------------------------------------------
# download lyrics.rda from
# https://github.com/mikkelkrogsholm/billboard/tree/master/data
load("C:/Data/R/wiki_hot_100s.rda")
head(lyric_sentiment)
head(wiki_hot_100s)

# get song's top 100 rank from wiki_hot_100s and join to lyric_sentiment
# rename some columns to match exercises from course
lyric_sentiment2 <- lyric_sentiment %>% 
  inner_join(wiki_hot_100s, by = "title") %>%
  select(rank = no, song = title, artist = artist.x, year = year.x, word, total_words, sentiment)

lyric_sentiment2$rank <- as.numeric(lyric_sentiment2$rank)
lyric_sentiment2$year <- as.numeric(lyric_sentiment2$year)

lyric_sentiment2 %>% filter(is.na(rank))


# make boxplots to see if sentiment correlates to song's top rank
lyric_sentiment2[complete.cases(lyric_sentiment2),] %>%
  filter(sentiment == "positive", total_words > 5) %>%
  count(song, rank, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         rank = 10 * floor(rank / 10)) %>%
  ggplot(aes(as.factor(rank), percent)) +
    geom_boxplot()

lyric_sentiment2[complete.cases(lyric_sentiment2),] %>%
  filter(sentiment == "negative", total_words > 5) %>%
  count(song, rank, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         rank = 10 * floor(rank / 10)) %>%
  ggplot(aes(as.factor(rank), percent)) +
  geom_boxplot()  

# no visible effect for either positive or negative sentiments...

# ----- Sentiment scores by year -----
lyric_sentiment2 %>%
  filter(sentiment == "negative", total_words > 5) %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         year = 10 * floor(year / 10)) %>%
  ggplot(aes(as.factor(year), percent)) + 
  geom_boxplot()

lyric_sentiment2 %>%
  filter(sentiment == "positive", total_words > 5) %>%
  count(song, year, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words,
         year = 10 * floor(year / 10)) %>%
  ggplot(aes(as.factor(year), percent)) + 
  geom_boxplot()
