#----- Wikipedia page views -----
# https://www.rdocumentation.org/packages/pageviews/versions/0.3.0
install.packages("pageviews")
library(pageviews)

wpviews <- article_pageviews(project = "en.wikipedia", 
                  article = c("Donald Trump", "Maria Butina", "George_Papadopoulos"), 
                  user_type = "user", 
                  start = "2016100100", 
                  end = "2018100100")

ggplot(wpviews, aes(date, views, col=article)) +
  geom_line() +
  #geom_smooth(se = FALSE) +
  scale_y_log10() +
  labs(x = "Date", y = "Views (log10)", title = "Daily Wikipedia Views (en)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=rel(2.0)))
  #scale_color_hue(labels = c("Trump", "Butina", "Manafort"))  #override legend labels

# look at days with Papadopoulos views > 5000
wpviews %>%
  filter(article == "George_Papadopoulos", views > 5000) %>% 
  arrange(desc(views))

# spread (PIVOT) article values to columns
wpviews %>% 
  spread(article, views) %>% 
  select(-(1:5)) %>% 
  tail(15)


#----- Top Articles function -----
top_articles <- top_articles(project = "en.wikipedia", platform = "all", 
                             start = as.Date("2018-07-22"), 
                             granularity = "daily")

write_csv(top_articles, "C:/Data/R/data/wikipedia_top_articles_2018-07-01.csv")


#----- Direct plot without intermediate saving -----
# , "Drake_(musician)"
article_pageviews(project = "en.wikipedia", 
                  article = c("Cardi_B", "Post_Malone"), 
                  user_type = "user", 
                  start = "2016100100", 
                  end = "2018100100") %>%
ggplot(aes(date, views, col=article)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "Date", y = "Views (log10)", title = "Daily Wikipedia Views (en)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=rel(2.0)))



