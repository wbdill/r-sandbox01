library(tidyverse)
library(ggrepel)
#install.packages("datapasta")
df <- tibble::tribble(
              ~Country, ~Y1975, ~Y2016,
           "Argentina",    0.4,   0.63,
              "Brazil",   0.28,   0.57,
              "Canada",    0.4,   0.64,
             "Germany",   0.38,   0.57,
               "India",   0.05,    0.2,
               "Italy",    0.4,   0.59,
               "Japan",   0.15,   0.27,
              "Mexico",   0.37,   0.65,
             "Nigeria",   0.09,   0.29,
            "Pakistan",   0.08,   0.28,
              "Russia",   0.43,   0.57,
        "Saudi Arabia",   0.38,    0.7,
               "Spain",   0.41,   0.62,
              "Turkey",   0.35,   0.67,
                  "UK",    0.4,   0.64,
                 "USA",   0.41,   0.68
        )
#df %>% pivot_longer(cols = c(Y1976, Y2016))

#----- two period time series -----
df %>% 
  pivot_longer(cols = c(Y1975, Y2016)) %>% # assuming that's the name of your data
  #gather('stage', 'value', 2:3) %>% #rename 'stage', 'value', respectively to suit your case
  ggplot(aes(x = name, y = value, group = Country, color = Country)) +
  geom_line(size = 1) +
  geom_point() +
  geom_label_repel(aes(label = Country)) +
  labs(title = "Obesity 1975 vs 2016",
       x = "Year",
       y = "",
       caption = "Chart: @wbdill\nData: @stats_feed")

#----- One "row" per country -----
df %>% 
  ggplot(aes(x = Y1975, y = reorder(Country, desc(Y1975))), size = 3) +
  geom_point(size = 3, color = "blue") +
  geom_segment(aes(x = Y1975, xend = Y2016, yend = Country), color = "blue", size = 1.0) +
  geom_point(aes(x = Y2016 ), size = 3, color = "red") +
  labs(title = "Share of adults who are overweight or obese",
       subtitle = "1975 vs 2016",
       x = "Obesity Rate",
       y = "",
       caption = "Chart: @wbdill (threads)\nData: @stats_feed (threads)")
