library(tidyverse)


#----- Trump lies -----
# https://www.washingtonpost.com/graphics/politics/trump-claims-database/

trump_lies <- read_csv("D:/opendata/wapo_trumpclaims_export-012021.csv")
trump_lies$date <- lubridate::mdy(trump_lies$date)

trump_lies_tally <- trump_lies %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  tally() %>%                   # get tally by day
  mutate(total = cumsum(n),     # get cumulative sum
         day_in_office = as.integer(difftime(date, "2017-01-20 00:00:00", units = "days") + 2)) # get # days in office

#write_csv(trump_lies_tally, "D:/opendata/trump_lies_tally.csv")
# saved to: https://pastebin.com/xxKF26bb

trump_lies_tally %>% 
  ggplot(aes(x = date, y = total)) +
  geom_line() +
  labs(title = "Trump Lies While In Office",
       x = "",
       y = "Cumulative Total",
       caption = "Chart: @bdill\nData: https://www.washingtonpost.com/graphics/politics/trump-claims-database/")
