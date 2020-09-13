# credits R Bloggers
# https://github.com/ewenme/geniusr#readme
# STEP 1 - Add libraries
install.packages("genius")
install.packages("tidytext")
library(genius)
library(tidytext)
library(tidyverse)
rm(list = ls())

#----- STEP 2 - Downloading lyrics of artist -----
Taylor_Swift <- genius_album(artist = "Taylor Swift", album = "Reputation")
Dua_Lipa <- genius_album(artist = "Dua Lipa", album = "Future Nostalgia")

str(Taylor_Swift)
str(Dua_Lipa)

#----- STEP 3 - Cleaning data (remove stop words, tokenize data) -----
tidy_Taylor <- Taylor_Swift %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

tidy_Dua <- Dua_Lipa %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)


#----- STEP 4 - Balancing both the dataset -----
tidy_Taylor <- tidy_Taylor %>%
  rename(swift = n) %>%
  mutate(swift_prop = swift/sum(swift))

tidy_Dua <- tidy_Dua %>% 
  rename(dua = n) %>% 
  mutate(dua_prop = dua / sum(dua))

head(tidy_Taylor)
head(tidy_Dua)

#----- STEP 5 - Comparing both lyrics -----
compare_words <- tidy_Taylor %>%
  full_join(tidy_Dua, by = "word")
head(compare_words)

#----- STEP 6 - Plotting graph for visualization -----
ggplot(compare_words, aes(x=swift_prop, y=dua_prop)) +
  geom_abline() +
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  labs(title = "Lyrics Word Frequency Comparison",
       x="Taylor Swift - Reputation",
       y="Dua Lipa - Future Nostalgia",
       caption = "Data: genius")  
  #theme_classic()


