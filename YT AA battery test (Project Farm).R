# YT AA battery test (Project Farm).R
# https://www.youtube.com/watch?v=V7-ghrTqA44
# https://www.youtube.com/watch?v=0A1GvQ40j0Q

dat <- read_tsv("C:/GitHub/r-sandbox01/data/YT_AA_battery_test_project_farm.txt")
dat

#----- basic bar chart -----
dat %>%
  filter(mAh_drain == 300) %>%
  group_by(brand) %>%
  summarize(minutes = mean(minutes)) %>%
  ggplot(aes(x = reorder(brand, -minutes), y = minutes)) +
  geom_bar(stat="identity") +
  scale_x_discrete(label=abbreviate)

#----- basic bar chart -----
dat %>%
  filter(mAh_drain == 100) %>%
  group_by(brand) %>%
  summarize(mAh_per_penny = mean(mAh_per_penny)) %>%
  ggplot(aes(x = reorder(brand, -mAh_per_penny), y = mAh_per_penny)) +
  geom_bar(stat="identity") +
  #scale_x_discrete()
  scale_x_discrete(label=abbreviate) +
  labs(
    x = "AA Battery",
    y = "mAh per penny",
    title = "AA Battery Test",
    subtitle = "100 mAh Drain",
    caption = "Project Farm https://www.youtube.com/watch?v=0A1GvQ40j0Q"
  )

dat %>%
  filter(mAh_drain == 300) %>%
  group_by(brand) %>%
  summarize(mAh_per_penny = mean(mAh_per_penny)) %>%
  ggplot(aes(x = reorder(brand, -mAh_per_penny), y = mAh_per_penny)) +
  geom_bar(stat="identity") +
  #scale_x_discrete()
  scale_x_discrete(label=abbreviate) +
  labs(
    x = "AA Battery",
    y = "mAh per penny",
    title = "AA Battery Test",
    subtitle = "300 mAh Drain",
    caption = "Project Farm https://www.youtube.com/watch?v=0A1GvQ40j0Q"
  )

# ----- stats by drain rate and battery tech -----
dat %>%
  group_by(mAh_drain, tech) %>%
  summarize(avg_minutes = mean(minutes, na.rm = TRUE),
            avg_mAh = mean(mAh),
            avg_mAhPenny = mean(mAh_per_penny)) %>%
  arrange(tech, mAh_drain)
