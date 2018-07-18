#-------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("tidytext")    # text mining
#install.packages("syuzhet")     # sentiment lexicons
#install.packages("gutenbergr")  # project Gutenberg book downloads

library(tidyverse)
library(tidytext)    # text mining
#library(syuzhet)     # sentiment lexicons.  get_sentiments("bing")
library(gutenbergr)  # Project Gutenberg downloadable books.  gutenberg_metadata to view index
#-------------------------------------------------------------------------------

gutenberg_metadata # listing of all gutenberg books to get their gutenberg_id


# ----- Mark Twain books -----
# Top 10 books list: https://www.publishersweekly.com/pw/by-topic/industry-news/tip-sheet/article/64432-the-10-best-mark-twain-books.html
twain_books <- gutenberg_metadata %>%
  filter(str_detect(author, "Twain, Mark"), has_text == TRUE) %>%
  select(gutenberg_id, author, title)

View(twain_books)

# pick 6 popular books identified by gutenberg_id
gutenberg_metadata %>%
  filter(gutenberg_id %in% c(74, 76, 86, 245, 1837, 3177)) %>%
  select(gutenberg_id, title, author, rights)

# ----- download from gutenberg -----
twain_book_data <- gutenberg_download(c(74, 76, 86, 245, 1837, 3177))

# save locally so we don't have to re-download it in future R sessions
saveRDS(twain_book_data, "C:/Data/R/twain_book_data.rds")
#twain_book_data <- readRDS("C:/Data/R/twain_book_data.rds")

head(twain_book_data, 15)

# add line numbers by book
twain_book_data <- twain_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number())

# tokenize to one word per row
twain_tokens <- twain_book_data %>%
  unnest_tokens(word, text)    # by word, colname = "text"

#----- Get Twain sentiments and graph net sentiment over time -----
# inner_join sentiment to score each word
# create index for every 80 lines, spread to + & - cols and get net sentiment
twain_sentiment_by_index <- twain_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(gutenberg_id, index = linenum %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  inner_join(twain_books, by = "gutenberg_id")

library(ggplot2)

#graph net sentiment over time (index) for each book
ggplot(twain_sentiment_by_index, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  labs(title = "Net Sentiment over time per 80 lines of text")

#----- plot net positive/negative for each book -----
twain_sentiment <- twain_tokens %>%
  inner_join(get_sentiments("bing"), by = "word")

twain_summary1 <- twain_sentiment %>%
  group_by(gutenberg_id, word) %>%
  count(word, sentiment) %>%
  arrange(gutenberg_id, desc(n)) %>%
  spread(sentiment, n) %>%
  group_by(gutenberg_id) %>%
  summarize(tot_positive = sum(positive, na.rm = TRUE),
            tot_negative = sum(negative, na.rm = TRUE),
            tot = tot_positive + tot_negative,
            pct_positive = tot_positive / tot,
            pct_negative = tot_negative / tot) %>%
  inner_join(twain_books) %>%
  gather(key = "key", value = "percent", c("pct_positive", "pct_negative")) %>%
  arrange(gutenberg_id, key)

head(twain_summary1, 12)

ggplot(twain_summary1, aes( reorder(title, percent), percent, fill = key)) +
  geom_col(alpha = 0.5) +
  coord_flip()

# ----- top 10 sentiment words (by freq) for each book -----
top_10_each_book <- twain_sentiment %>%
  group_by(gutenberg_id, word) %>%
  count(word, sentiment) %>%
  arrange(gutenberg_id, desc(n)) %>%
  group_by(gutenberg_id) %>%
  top_n(10)
