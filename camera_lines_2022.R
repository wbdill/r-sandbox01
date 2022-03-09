# 2022-03-09 @bdill analysis of camera lines over time
# https://docs.google.com/spreadsheets/d/1V8KOfTOap3068VdzyFFE1oisGPfrLEo9icfY9NeGDCA/edit#gid=1073613661

library(tidyverse)
#rm(list = ls())

d <- read_tsv("C:/Users/bdill/Downloads/camera_lines.tsv")
d <- janitor::clean_names(d)
d <- d %>% mutate(make_line = paste(make, line))
#str(d)
d$wifi <- as.factor(d$wifi)
#d %>% count(make)
#d %>% count(line)
ggplot(d, aes(x = year, y = m_pixels, color = make)) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~make_line) +
  labs(title = "Megapixels By Year by Camera Line",
       y = "Megapixels",
       x = "Year", 
       color = "Make",
       caption = "@bdill\nData: https://bit.ly/camera_db")

ggplot(d, aes(x = year, y = m_pixels)) +
  geom_jitter(size = 3, alpha = 0.6, aes(color = make)) +
  geom_smooth(method = loess, se = FALSE, size = 1, alpha = 0.3) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  scale_x_continuous(breaks = seq(1995,2025,5)) +
  labs(title = "Megapixels By Year by Camera Line",
     y = "Megapixels",
     x = "Year",
     color = "Make",
     caption = "(C) 2022 @bdill\nData: https://bit.ly/camera_db")


ggplot(d, aes(x = m_pixels)) +
  geom_histogram(bins = 40)

?geom_histogram
