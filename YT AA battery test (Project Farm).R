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

# ----- rechargable batteries -----
recharge <- read_tsv("C:/GitHub/r-sandbox01/data/YT_AA_rechargable_test_Project_Farm_2019.txt")
recharge

recharge2 <- gather(recharge, key = mA_discharge, value = mAh, 4,7:9)
recharge2$mA_discharge <- gsub("mAh_", "", recharge2$mA_discharge)
recharge2$mA_discharge <- gsub("_mAh", "", recharge2$mA_discharge)
recharge2$mA_discharge <- gsub("mA_discharge", "", recharge2$mA_discharge)
recharge2$battery_name <- gsub(" \\({1}[0-9]*)", "", recharge2$battery_name)

#----- Weight
recharge2 %>%
  filter(mA_discharge == 300) %>%
  ggplot(aes(x =  reorder(battery_name, -weight_grams), y = weight_grams)) +
  geom_bar(stat="identity", fill = "blue") +
  geom_text(aes(label=weight_grams, y = weight_grams + 1)) +
  scale_x_discrete(label= function(x) abbreviate(x, minlength=9)) +
  labs(
    x = "AA Battery",
    y = "Weight (grams)",
    title = "AA Battery Test",
    subtitle = "Weight in Grams",
    caption = "Project Farm https://www.youtube.com/watch?v=0A1GvQ40j0Q"
  )


#----- mAh at Various Discharge Rates
plot1 <- recharge2 %>%
  group_by(battery_name) %>%
  mutate(mean_mAh = mean(mAh, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mAh = round(mAh)) %>%
  ggplot(aes(x = reorder(battery_name, -mean_mAh), y=mAh, fill=mA_discharge)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=mAh, y = mAh + 50), size=1.3, hjust=0.5, position=position_dodge(width=0.9)) +
  scale_x_discrete(label= function(x) abbreviate(x, minlength=8)) +
  labs(
    x = "AA Battery Brand",
    y = "milliamp hours",
    title = "Rechargable AA Battery Test",
    subtitle = "mAh at Various Discharge Rates",
    caption = "Project Farm https://www.youtube.com/watch?v=0A1GvQ40j0Q"
  )

png(filename=file.path("C:/Data/Dropbox/R/data/Rechargable_battery_AA.png"),  width = 8, height=4, units='in', res=200)
par(mar=c(4,4,0,1)+.1)
plot1
dev.off()
