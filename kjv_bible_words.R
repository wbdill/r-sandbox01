#----- King James Version Bible analysis -----
# https://github.com/scrollmapper/bible_databases/blob/master/csv/t_kjv.csv
# https://github.com/wbdill/r-sandbox01

#install.packages("tidytext")
#install.packages("grkmisc")
library(tidyverse)
library(tidytext)

kjv_books <- read_csv("C:/Data/R/data/kjv_books.csv")

kjv <- read_csv("C:/Data/R/data/kjv_bible.csv")
names(kjv) = c("id", "book", "chapter", "verse", "text")
kjv <- kjv %>%
  mutate(is_nt = case_when(book > 39 ~ 1,
                           book <= 39 ~ 0))

saveRDS(kjv, "C:/GitHub/r-sandbox01/data/kjv.rds")

kjv_tidy <- kjv %>% 
  tidytext::unnest_tokens(word, text)


#----- verse count by book -----
kjv_book_verses <- kjv %>% count(book) %>%
  full_join(kjv_books) %>%
  select(book, book_name, verses = n)

#----- chapter count by book -----
kjv_book_chapters <- kjv %>% 
  distinct(book, chapter) %>%
  count(book) %>%
  full_join(kjv_books) %>%
  select(book, book_name, chapters = n)

#----- word count by book -----
kjv_book_words <- kjv_tidy %>%
  count(book) %>%
  full_join(kjv_books) %>%
  select(book, book_name, words = n)

#----- summary table of verse count and word count by book -----
kjv_book_summary <- kjv_book_chapters %>%
  full_join(kjv_book_verses) %>%
  full_join(kjv_book_words) %>%
  mutate(is_nt = case_when(book > 39 ~ 1,
                           book <= 39 ~ 0))

#----- word count by book/chapter -----
kjv_tidy %>%
  count(book, chapter) %>%
  full_join(kjv_books) %>%
  select(book, book_name, chapter, words = n)

#----- Chart: Word Count by Book -----
kjv_plot <- kjv_book_summary %>%
  arrange(desc(words)) %>%
#  slice(1:30) %>%
  ggplot(aes(x=reorder(book_name, words), y = words, fill = as.factor(is_nt))) +
  geom_col() + 
  scale_fill_manual(name="New Test.",labels=c("No", "Yes"), values=c("#F8766D", "#00BFC4")) +
  coord_flip() +
  labs(y = "word count", x = NULL, 
       title = "KJV Word Count by Book",
       caption = "") 

kjv_plot
png('C:/github/r-sandbox01/kjv_book_words.png', width = 1200, height = 900)
plot(kjv_plot)
dev.off()

#----- Chart: Top 25 used words in KJV -----
old_english_stop_words <- data.frame(word = c("thou", "thee", "thy", "ye", "hath", "hast", "thine"))

kjv_tidy %>% 
  anti_join(tidytext::stop_words) %>% 
  anti_join(old_english_stop_words) %>%
  filter(!is.na(word), str_detect(word, "[a-zA-Z]")) %>% 
  #filter(book > 38 & book <= 42 ) %>%
  #group_by(word) %>% 
  count(word, sort = TRUE) %>%
  #ungroup() %>% 
  mutate(word = fct_rev(fct_inorder(word))) %>% 
  slice(1:20) %>% 
  ggplot() +
  aes(word, n, label=n) +
  geom_col() +
  geom_text(hjust = -.2, size=4) +
  coord_flip() +
  labs(y = "Number of Times Mentioned", x = NULL, 
       title = "20 Most Used Words in the KJV",
       caption = "excludes common stop words such as: thou, thee, thy, a, and, the, etc.") 

#----- Chart: Top 25 used words in KJV:NT -----
kjv_tidy %>% 
  anti_join(tidytext::stop_words) %>% 
  filter(!is.na(word), str_detect(word, "[a-zA-Z]")) %>% 
  filter(is_nt == 1 ) %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_rev(fct_inorder(word))) %>% 
  slice(1:20) %>% 
  ggplot() +
  aes(word, n) +
  geom_col() +
  coord_flip() +
  labs(y = "Number of Times Mentioned", x = NULL, 
       title = "20 Most Used Words in the KJV New Testament",
       caption = "excludes common stopwords") 


#  scale_y_continuous(expand = c(0.01,0), limits = c(0, 2500)) +
#  scale_fill_gradient(low = "#b7c6d6", high = "#445566", guide = FALSE) +
#  # ggsci::scale_fill_material("deep-purple") +
#  #grkmisc::theme_grk(panel_background_color = NA) +
#  theme(
#    panel.border = element_blank(),
#    panel.background = element_blank(),
#    panel.grid.major.y = element_blank(),
#    plot.caption = element_text(hjust = 0)
#  )


#----- Chart: Top words of the gospels
kjv_tidy %>% 
  anti_join(tidytext::stop_words) %>% 
  anti_join(old_english_stop_words) %>%
  filter(!is.na(word), str_detect(word, "[a-zA-Z]")) %>% 
  filter(book > 38 & book <= 43 ) %>%
  count(word, sort = TRUE) %>%
  slice(1:25) %>%
  mutate(word = fct_rev(fct_inorder(word))) %>%
  ggplot() +
  aes(word, n, label = n) +
  geom_col() +
  coord_flip() +
  geom_text(hjust = -.2, size=4) +
  labs(y = "Number of Times Mentioned", x = NULL, 
       title = "25 Most Used Words in the KJV Gospels",
       subtitle = "Matthew, Mark, Luke, John",
       caption = "excludes common stopwords") 

#----- Chart: Top words of the NT EXCLUDING the gospels
kjv_tidy %>% 
  anti_join(tidytext::stop_words) %>% 
  anti_join(old_english_stop_words) %>%
  filter(!is.na(word), str_detect(word, "[a-zA-Z]")) %>% 
  filter(book > 42  ) %>%
  count(word, sort = TRUE) %>%
  slice(1:25) %>%
  mutate(word = fct_rev(fct_inorder(word))) %>%
  ggplot() +
  aes(word, n, label = n) +
  geom_col() +
  coord_flip() +
  geom_text(hjust = -.2, size=3) +
  labs(y = "Number of Times Mentioned", x = NULL, 
       title = "25 Most Used Words in the KJV New Testament EXCLUDING the Gospels",
       subtitle = "Books 43-66",
       caption = "excludes common stopwords") 

