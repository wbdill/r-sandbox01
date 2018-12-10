# YT AA battery test (Project Farm).R
# https://www.youtube.com/watch?v=V7-ghrTqA44

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

# ----- stats by drain rate and battery tech -----
dat %>%
  group_by(mAh_drain, tech) %>%
  summarize(avg_minutes = mean(minutes),
            avg_mAh = mean(mAh),
            avg_mAhPenny = mean(mAh_per_penny)) %>%
  arrange(tech, mAh_drain)
