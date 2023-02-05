library(tidyverse)
library(rvest)
library(lubridate)

#----- Web scraper -----
ScrapeHot100 <- function(date) {
  #date <- "2021-11-20"
  html <- rvest::read_html(paste0("https://www.billboard.com/charts/hot-100/", date, "/"))
  Sys.sleep(1) #pause for 3 secs
  rows <- html %>% html_nodes("ul.o-chart-results-list-row")
   
  vrank <- rows %>%  html_nodes("li:first-child span:first-child") %>% html_text2()
  vsong <- rows %>% html_nodes("li.lrv-u-width-100p") %>% html_nodes("li:first-child") %>% html_nodes("h3") %>% html_text2()
  vartist <- rows %>% html_nodes("li.lrv-u-width-100p") %>% html_nodes("li:first-child") %>% html_nodes("span") %>% html_text2()
  vlastweek <- rows %>% html_node("li.lrv-u-width-100p ul li:nth-child(4) span") %>% html_text2()
  vpeak <- rows %>% html_node("li.lrv-u-width-100p ul li:nth-child(5) span") %>% html_text2()
  vweeks <- rows %>% html_node("li.lrv-u-width-100p ul li:nth-child(6) span") %>% html_text2()
  
  df <- data.frame(date = date, rank = vrank, song = vsong, artist = vartist, last_week = vlastweek, peak_rank = vpeak, weeks_on_board = vweeks)
  #bb100 <<- bb100
  bb100 <<- rbind(bb100, df)  ## double << to reach out to outer scope
}

date <- "2021-11-27"
while(date < Sys.Date()) {
#while(date < "2021-11-21") {
  print(date)
  ScrapeHot100(date)
  print(nrow(bb100))
  date <- ymd(date) + days(7)
  flush.console()
  #Sys.sleep(3) #pause for 3 secs
}
#head(bb100)
#tail(bb100)
#bb100 %>% count(date) %>% arrange(desc(date)) %>% View()
bb100$last_week <- str_replace(bb100$last_week, "-", "")  # replace dashes with empty string

bb100 %>% arrange(date, rank) %>% write_csv("D:/opendata/Billboard_Hot_100_1958_to_2022-09-24_b.csv")


#----- Billboard top 100 -----
# https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs/metadata?resource=download
#devtools::install_github("hoesler/rwantshue")  # https://github.com/hoesler/rwantshue

#bb100 <- read_csv("D:/opendata/billboard_top_100_charts.csv")  # 18MB 330k rows
bb100 <- read_csv("D:/opendata/Billboard_Hot_100_1958_to_2022-09-24.csv")  #334k rows
bb100 <- janitor::clean_names(bb100)
bb100$rank <- as.integer(bb100$rank)
bb100$last_week <- as.integer(bb100$last_week)
bb100$peak_rank <- as.integer(bb100$peak_rank)
bb100$weeks_on_board <- as.integer(bb100$weeks_on_board)
str(bb100)

bb100_songs <- bb100 %>% 
  mutate(points = (101-rank)) %>% 
  group_by(song, artist) %>% 
  summarize(start = min(date), end = max(date), peak = min(rank), weeks = n(), total_points = sum(points)) %>% 
  mutate(year = substr(start, 1, 4),
         decade = paste0(substr(start, 1, 3), "0")) %>% 
  arrange(desc(total_points))
write_csv(bb100_songs, "D:/opendata/Billboard_Hot_100_songs_all_time.csv")

bb100_songs %>% group_by(artist) %>% 
  summarize(tot_points = sum(total_points),
            hits = n(),
            period = paste0(min(year), " - ", max(year))) %>% 
  arrange(desc(tot_points)) %>% View()


#----- Plot function -----
PlotArtist <- function(p_artist, max_year = 9999) {
  #p_artist <- "The Weeknd"
  #max_year = 9999
  max_date <- as.Date(paste0(max_year, "-01-01"))
  #max_date
  
  hits <- bb100 %>% 
    filter(artist == p_artist) %>%
    #filter(date >= "1985-01-01" & date < "1986-01-01") %>% 
    group_by(song, artist) %>% 
    summarize(start = min(date), end = max(date), peak = min(rank), weeks = n()) %>% 
    arrange(peak, desc(weeks))

  n <- nrow(hits)
  cols = rainbow(n, s=.6, v=.9)[sample(1:n,n)]

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

p_artist <- "Ozzy Osbourne"
p_artist <- "Cher"
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


#-----
bb100 %>% filter(grepl("Lady In", song)) %>% View()
hits <- bb100 %>% 
  filter(song %in% c("Never Gonna Give You Up", "Puttin' On The Ritz", "Mickey", "Don't Worry, Be Happy (From \"Cocktail\")", "Rock Me Amadeus", "The Safety Dance", "Wake Me Up Before You Go-Go", "The Lady In Red", "The Final Countdown", "We Built This City" )  ) %>%
  filter(date >= "1980-01-01" & date < "1990-01-01") %>% 
  group_by(song, artist) %>% 
  summarize(start = min(date), end = max(date), peak = min(rank), weeks = n()) %>% 
  arrange(peak, desc(weeks))

n <- nrow(hits)
cols = rainbow(n, s=.6, v=.9)[sample(1:n,n)]

bb100 %>% 
  inner_join(hits) %>% 
  filter(date < max_date) %>% 
  mutate(song2 = paste0("(", substr(start, 1, 4), ") ", song) ) %>% 
  ggplot(aes(x = date, y = rank, group = song2)) +
  geom_line(aes(color = song2), size = 1.25) +
  geom_point(aes(color = song2), size = 2) +
  scale_y_reverse(limits = c(100, 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values=cols) +
  labs(title = paste0("The 10 \"Worst\" Songs of the 1980s"),
       subtitle = "Billboard Hot 100 Weekly Rank",
       x = "Date",
       y = "Rank",
       color = "Song",
       caption = "Chart: @bdill\nData: Billboard Hot 100\nhttps://www.billboard.com/charts/hot-100")

#-----
install.packages("overviewR")

bb100 %>% filter(artist %in% c("Aerosmith", "The Cars", "Van Halen", "America", "Taylor Swift", "U2", "Cher", "The Weeknd", "Duran Duran", "Elton John", "Phil Collins", "Beyonce", "Mariah Carey")) %>% 
  mutate(year = as.integer(substr(date, 1, 4))) %>% 
  overviewR::overview_heat(id = artist, time = year, xaxis = "Year", yaxis = "Artist")
?overview_heat


#--- #1s
bb100 |> filter(rank == 1)
bb100_songs |> filter(peak == 1) |> group_by(artist) |> 
  summarize(songs = n()) |> 
  arrange(desc(songs)) |> 
  filter(songs >= 5) |> 
  View()

# #1s STRING_AGG(songs)
bb100_songs |> filter(peak == 1) |>
  group_by(artist) |> 
  summarize(number_ones = n(),
            songs = toString(unique(song))) |> 
  filter(number_ones > 4) |> 
  arrange(desc(number_ones)) |> 
  View()

bb100_songs |> filter(peak == 1) |> 

bb100_songs |> filter(peak == 1) |> 
  group_by(artist) |> 
  summarize(songs = toString(unique(song))) |> 
  View()

test <- mtcars |>
  rownames_to_column("model") |>
  group_by(gear) |>
  summarise(string_agg = toString(unique(model)))
