# https://datascienceineducation.com/c12.html

install.packages("ggraph")  # https://ggraph.data-imaginist.com/
install.packages("randomNames") # https://www.rdocumentation.org/packages/randomNames/versions/1.4-0.0/topics/randomNames
install.packages("rtweet")  # https://www.rdocumentation.org/packages/rtweet/versions/0.4.0

library(tidyverse)
library(rtweet)
library(dataedu)
library(randomNames)
library(tidygraph)
library(ggraph)
rm(list = ls())

# 12.4.3 Data Sources and Import
tt_tweets <- search_tweets("#trump", n = 2500)

#tt_tweets <- dataedu::tt_tweets  # sample data from dataedu pkg


tt_tweets %>% 
  select(user_id, status_id, created_at, screen_name, retweet_count, text) %>% 
  arrange(desc(retweet_count)) %>% 
  View()

#randomNames(100, ethnicity = c(3,4,5), return.complete.data = T)



# 12.6.1
regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

mentions <- tt_tweets %>%
  mutate(all_mentions = str_extract_all(text, regex)) %>%  # Use regular expression to identify all the usernames in a tweet
  unnest(all_mentions)


mentions %>%
  select(all_mentions, user_id, status_id, created_at, screen_name, retweet_count, text) %>% 
  mutate(all_mentions = str_extract_all(text, regex)) %>%
  unnest(all_mentions) %>% 
  View()

# 12.6.2
edgelist <- mentions %>%
  mutate(receiver = str_sub(str_trim(all_mentions), start = 2)) %>%
  select(sender = screen_name, receiver)


# 12.7.1
interactions_sent <- edgelist %>% 
  count(sender) %>%  # this counts how many times each sender appears in the data frame, effectively counting how many interactions each individual sent 
  filter(n > 1) %>%  # limit list to those with > 1 interaction
  arrange(desc(n))



edgelist <- edgelist %>% 
  # the first of the two lines below filters to include only senders in the interactions_sent data frame
  # the second line does the same, for receivers
  filter(sender %in% interactions_sent$sender,
         receiver %in% interactions_sent$sender)
# ?as_tbl_graph
g <- as_tbl_graph(edgelist)


# ?ggraph
g %>%
  # we chose the kk layout as it created a graph which was easy-to-interpret, but others are available; see ?ggraph
  ggraph(layout = 'kk') +
  #ggraph(layout = 'igraph', algorithm = "kk") +
  # this adds the points to the graph
  geom_node_point() +
  geom_edge_link(alpha = .2) +  # adds the links, or the edges; alpha = .2 makes it so that the lines are partially transparent
  theme_graph()


g %>% 
  # this calculates the centrality of each individual using the built-in centrality_authority() function
  mutate(centrality = centrality_authority()) %>% 
  ggraph(layout = 'igraph', algorithm = "kk") +
  geom_node_point(aes(size = centrality)) +
  scale_color_continuous(guide = 'legend') +  # this line colors the points based upon their centrality
  geom_edge_link(alpha = .2) +
  theme_graph()
