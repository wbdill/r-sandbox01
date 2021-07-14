
# https://www.disastercenter.com/crime/uscrime.htm
library(tidyverse)
path <- "C:/GitHub/r-sandbox01/data/uscrime_1960-2019.csv"
dt <- read_csv(path, skip = 1)

#glimpse(dt)
?pivot_longer
#dt2 <- pivot_longer(dt, cols = 3:12, names_to = "category", values_to = "val")
dt2 <- pivot_longer(dt, cols = c(3:7,9:11), names_to = "category", values_to = "val")

dt2 %>% 
  ggplot(aes(x = Year, y = val, group = category, color = category)) +
  geom_line(size = 1) +
  #scale_y_log10() +
  geom_vline(xintercept = 1966) +
  labs(title = "US Crime 1960 - 2019",
       y = "Crime per 100,000 people",
       caption = "Source: https://www.disastercenter.com/crime/uscrime.htm")



dt3 <- pivot_wider(dt2, names_from = category, values_from = val)
write_csv(dt3, "foo.csv")
getwd()
