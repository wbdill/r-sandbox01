# battery test
# https://www.youtube.com/watch?v=RWAjgZmnoZo

library(tidyverse)
library(lubridate)

dat <- read_tsv("C:/GitHub/r-sandbox01/data/battery_test.txt")


# create column with runtime in minutes
tmp <- hms(dat$runtime)
dat$runtime_mins <- hour(tmp)*60 + minute(tmp)
dat$runtime_hours <- hour(tmp) + minute(tmp) / 60
dat$cost_per_hour <- dat$cost / dat$runtime_hours


dat %>%
  group_by(brand) %>%
  summarize(avg_hours = mean(runtime_hours),
            sd_hours = sd(runtime_hours))
dat %>%
  group_by(fan_num) %>%
  summarize(avg_hours = mean(runtime_hours),
            sd_hours = sd(runtime_hours)) %>%
  arrange(avg_hours)

# which fan_num is the worst for each brand
dat %>%
  group_by(brand) %>%
  top_n(2, wt = -runtime_hours) %>%
  select(brand, round, fan_num, runtime_hours)

#----- Charts -----
dat2 <- dat %>%
  filter(fan_num %in% c(2,3,5,6))

dat2 %>%
  group_by(brand) %>%
  summarize(runtime_hours = mean(runtime_hours)) %>%
  ggplot(aes(x = reorder(brand, runtime_hours), y = runtime_hours)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Battery Brand",
    y = "Runtime (hours)",
    title = "Battery Runtimes",
    caption = "source: https://www.youtube.com/watch?v=RWAjgZmnoZo"
  )

dat2 %>%
  group_by(brand) %>%
  summarize(cost_per_hour = mean(cost_per_hour), runtime_hours = mean(runtime_hours)) %>%
  ggplot(aes(x = reorder(brand, cost_per_hour), y = cost_per_hour)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Battery Cost per hour",
    y = "Cost per hour",
    title = "Battery Cost/Hr",
    caption = "source: https://www.youtube.com/watch?v=RWAjgZmnoZo"
  )  
