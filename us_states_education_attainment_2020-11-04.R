library(tidyverse)
#install.packages("tribble")
rm(list = ls())

df <- read_tsv("D:/downloads/masters_by_state.csv")
df$masters_pct <- as.numeric(str_remove(df$masters_pct, "%"))
df$pct_r_2016 <- as.numeric(df$pct_r_2016)

str(df)

#----- Plot
df %>% 
  ggplot(aes(pct_r_2016, masters_pct)) +
  geom_point(aes(color = red_blue_2016), size = 2) +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = c(0,5,10,15,20)) +
  scale_color_manual(values = c("#5555ff", "#ff5555"), labels = c("Dem", "Rep"), name = "Won State") +
  labs(title = "US - Master's Degree (or higher) Attainment",
       x = "Percent Voted Republican (2016)",
       y = "Percent with Master's Degree or higher",
       caption = "data: https://www.zippia.com/advice/most-highly-educated-states-in-america/")


#----- linear model
mod <- lm(masters_pct ~ pct_r_2016, data = df)
summary(mod)

sjPlot::tab_model(mod)
