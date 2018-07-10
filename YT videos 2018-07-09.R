# https://www.kaggle.com/datasnaek/youtube/version/24#USvideos.csv
rm(list = ls())
library(tidyverse)
library(stringr)

yt <- read.csv("D:/downloads/USvideos.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
yt <- select(yt, -tags, -thumbnail_link)  #chop off tags and thumbs cols

#yt$video_id <- as.character(yt$video_id)
#yt$title <- as.character(yt$title)
#yt$date <- as.character(yt$date)

yt$comment_total <- as.numeric(yt$comment_total)
yt$channel_title <- as.factor(yt$channel_title)
yt$category_id <- as.factor(yt$category_id)


# combine month and day with "2017" to get a date value ----------------------------
library(lubridate)
yt <- separate(yt, date, c("day", "month"))
yt$video_date <- ymd(paste("2017", yt$month, yt$day, sep=""))

head(yt)
str(yt)


#---------- group by channel_title ------------------------------------------------------------
channels <- yt %>% 
  group_by(channel_title) %>%
  summarize(vid_cnt = n() ,
            tot_cats = n_distinct(category_id) ,
            tot_likes = sum(likes) ,
            tot_views = sum(views) ,
            min_dt = min(video_date) ,
            max_dt = max(video_date)) %>%
  arrange(desc(tot_views))

head(channels, 20)
str(channels)

#---------- avg vids per channel ----------------------------
yt %>% 
  group_by(channel_title) %>%
  summarize(vid_cnt = n()) %>%
  summarize(avg_vids = mean(vid_cnt), sd_vids = sd(vid_cnt))

counts <- yt %>% 
  group_by(channel_title) %>%
  summarize(vid_cnt = n())

#---------- histogram of views by channel----------------------------------------------
hist(counts$vid_cnt, breaks =  20, xlab = "# of videos", main = "Videos Per Channel")

#----- top channels based on total views -------------------------------
channels %>% 
  select(channel_title, tot_views) %>%
  arrange(desc(tot_views)) %>%
  top_n(20)


#---------- plot of channels with top view counts -------------------------------------

channels %>% select(channel_title, tot_views) %>% top_n(20) %>%
  ggplot() +
  geom_point( aes(x = reorder(channel_title, desc(tot_views)), y = tot_views) ) +
#  geom_bar(aes(channel_title, fill = tot_views)) +
  ylim(0, 210000000) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "YouTube Views", x = "Channel", y = "View Count") +
  scale_x_discrete(labels = abbreviate)


#---------- searching channels by search string -----------------------------------------
# all channels with "smarter" in the channel_title
channels %>% filter(str_detect(channel_title, ("smarter")))
channels %>% filter(str_detect(channel_title, ignore.case("smarter")))
channels %>% filter(str_detect(channel_title, coll("smarter", ignore_case = TRUE)))
channels %>% filter(str_detect(channel_title, coll("photo", ignore_case = TRUE)))

channels %>% filter(str_detect(channel_title, regex("VEVO$")))

channels %>% filter(str_detect(channel_title, regex("^de", ignore_case = TRUE)))

