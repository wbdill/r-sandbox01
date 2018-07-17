#-------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("tidytext")    # text mining
#install.packages("syuzhet")     # sentiment lexicons
#install.packages("gutenbergr")  # project Gutenberg book downloads

library(tidyverse)
library(tidytext)    # text mining
library(syuzhet)     # sentiment lexicons.  get_sentiments("bing")
library(gutenbergr)  # Project Gutenberg downloadable books.  gutenberg_metadata to view index
#-------------------------------------------------------------------------------

gutenberg_metadata # listing of all gutenberg books to get their gutenberg_id


# ----- Mark Twain books -----
# Top 10 books list: https://www.publishersweekly.com/pw/by-topic/industry-news/tip-sheet/article/64432-the-10-best-mark-twain-books.html
twain_books <- gutenberg_metadata %>%
  filter(str_detect(author, "Twain, Mark"), has_text == TRUE) %>%
  select(gutenberg_id, author, title)

View(twain_books)

# pick 6 popular books using gutenberg_id
gutenberg_metadata %>%
  filter(gutenberg_id %in% c(74, 76, 86, 245, 3177, 8525)) %>%
  select(gutenberg_id, title, author, rights)

# ----- download from gutenberg -----
twain_book_data <- gutenberg_download(c(74, 76, 86, 245, 3177, 8525))

# save locally so we don't have to re-download it in future R sessions
saveRDS(twain_book_data, "C:/Data/R/twain_book_data.rds")
#twain_book_data <- readRDS("C:/Data/R/twain_book_data.rds")

head(twain_book_data, 15)

# add line numbers by book
twain_book_data <- twain_book_data %>%
  group_by(gutenberg_id) %>%
  mutate(linenum = row_number())

# tokenize
twain_tokens <- twain_book_data %>%
  unnest_tokens(word, text)    # by word, colname = "text"
  
# inner_join sentiment to score each word
twain_sent <- twain_tokens %>%
  inner_join(get_sentiments("bing"), by = "word")

head(twain_sent)

twain_summary1 <- twain_sent %>%
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
  geom_col() +
  coord_flip()

# top 10 sentiment words (by freq) for each book
top_10_each_book <- twain_sent %>%
  group_by(gutenberg_id, word) %>%
  count(word, sentiment) %>%
  arrange(gutenberg_id, desc(n)) %>%
  group_by(gutenberg_id) %>%
  top_n(10)
