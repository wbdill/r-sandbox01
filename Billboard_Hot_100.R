
#----- Billboard top 100 -----
# https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs/metadata?resource=download
#devtools::install_github("hoesler/rwantshue")  # https://github.com/hoesler/rwantshue

bb100 <- read_csv("D:/opendata/billboard_top_100_charts.csv")
bb100 <- janitor::clean_names(bb100)

PlotArtist <- function(p_artist, max_year = 9999) {
  #p_artist <- "The Weeknd"
  #bb100 %>% filter(grepl("Roses", artist))
  
  #max_year = 9999
  #max_date <- as.Date(paste0(max_year, "-01-01"))
  #max_date
  
  hits <- bb100 %>% 
    filter(artist == p_artist) %>%
    #filter(date >= "1985-01-01" & date < "1986-01-01") %>% 
    group_by(song, artist) %>% 
    summarize(start = min(date), end = max(date), peak = min(rank), weeks = n()) %>% 
    arrange(peak, desc(weeks))

  hits

  n <- nrow(hits)
  cols = rainbow(n, s=.6, v=.9)[sample(1:n,n)]
  #p + scale_fill_manual(values=cols)
  
  bb100 %>% filter(artist == p_artist) %>% 
    inner_join(hits) %>% 
    filter(date < max_date) %>% 
    mutate(song2 = paste0("(", substr(start, 1, 4), ") ", song) ) %>% 
  
    ggplot(aes(x = date, y = rank, group = song2)) +
    geom_line(aes(color = song2), size = 1.25) +
    geom_point(aes(color = song2), size = 2) +
    scale_y_reverse(limits = c(100, 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_manual(values=cols) +
    labs(title = paste0(p_artist, " Songs"),
         subtitle = "Billboard Hot 100 Weekly Rank",
         x = "Date",
         y = "Rank",
         color = "Song",
         caption = "Chart: @bdill\nData: Billboard Hot 100\nhttps://www.billboard.com/charts/hot-100")

}

#p_artist <- "Ozzy Osbourne"
p_artist <- "Taylor Swift"
PlotArtist(p_artist = p_artist, max_year = 9999)

bb100 %>% 
  filter(artist == p_artist) %>%
  mutate(points = (101-rank)) %>% 
  #filter(date >= "1960-01-01" & date < "1970-01-01") %>% 
  group_by(song, artist) %>% 
  summarize(start = min(date), end = max(date), peak = min(rank), weeks = n(), total_points = sum(points)) %>% 
  arrange(desc(total_points), peak, desc(weeks)) %>% 
  write_csv(paste0("D:/opendata/billboard_", p_artist, ".csv"))


#----- Top 10 each decade -----

top10decade <- read_csv("C:/users/bdill/Downloads/Billboard_Hot_100_Top10.csv")
top10decade$decade <- as.factor(top10decade$decade)
top10decade %>% 
  ggplot(aes(x = decade_rank, y = total_points, group = decade)) +
  geom_line(aes(color = decade), size = 2) +
  ylim(0, max(top10decade$total_points)) +
  scale_x_discrete(limits=1:10, labels=1:10) +
  labs(title = "Billboard Hot 100 - Top 10 By Decade",
       subtitle = "Total points = sum(101 - weekly rank) for each song",
       x = "Top 10 songs in Decade - Ranked 1-10",
       y = "Total Points",
       color = "Decade",
       caption = "@bdill\nhttps://www.billboard.com/charts/hot-100/")
