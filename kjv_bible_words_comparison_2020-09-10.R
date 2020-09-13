library(tidyverse)

#----- KJV Bible book comparison -----
kjv <- read_csv("https://pastebin.com/raw/xU69SmF6")
kjv_books <- read_csv("https://pastebin.com/raw/NkAUpfcS")
kjv <- kjv %>%
  inner_join(kjv_books, by = c("b" = "book"))

#----- Gospels vs Revelation -----
tidy_rev <- kjv %>% 
  filter(book_name == "Revelation") %>% 
  unnest_tokens(word, t) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>% 
  rename(rev = n) %>% 
  mutate(rev_prop = rev / sum(rev))

tidy_gosp <- kjv %>% 
  filter(book_name %in% c("Matthew", "Mark", "Luke", "John")) %>% 
  unnest_tokens(word, t) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>% 
  rename(gosp = n) %>% 
  mutate(gosp_prop = gosp / sum(gosp))

tidy_rev %>% 
  inner_join(tidy_gosp, by = "word") %>% 
  ggplot(aes(x=rev_prop, y=gosp_prop)) +
  geom_abline() +
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  labs(title = "Word Frequency Comparison: King James Bible",
       x="Revelation",
       y="Gospels",
       caption = "Data: KJV Bible")  

#----- OT vs NT -----
tidy_ot <- kjv %>% 
  filter(is_nt == 0) %>% 
  unnest_tokens(word, t) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>% 
  rename(ot = n) %>% 
  mutate(ot_prop = ot / sum(ot))

tidy_nt <- kjv %>% 
  filter(is_nt == 1) %>% 
  unnest_tokens(word, t) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>% 
  rename(nt = n) %>% 
  mutate(nt_prop = nt / sum(nt))

tidy_ot %>% 
  inner_join(tidy_nt, by = "word") %>% 
  ggplot(aes(x=ot_prop, y=nt_prop)) +
  geom_abline() +
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  labs(title = "Word Frequency Comparison: King James Bible",
       x="Old Testament",
       y="New Testament",
       caption = "Data: KJV Bible") 
