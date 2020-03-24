

library(tidyverse)
library(broom)

df <- tribble(~group, ~set, ~x, ~y,
              0, "training", 1, 2,
              0, "training", 2, 4,
              0, "training", 4, 9,
              0, "training", 5, 9,
              0, "training", 15, 29,
              0, "training", 20, 42,
              0, "training", 21, 42,
              0, "training", 25, 49,
              0, "test", 10, NA,
)
df %>%
  filter(set == "training") %>%
  group_by(group) %>%
  do(pm = lm(y ~ x, data = .)) %>%
  left_join(df, ., by = "group") 

?tribble


df <- tribble(~year, ~price,
              2015, 2.10,
              2016, 2.02,
              2017, 2.55,
              2018, 2.59)
ggplot(df, aes(x=year, y = price)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_x_reverse() +
  geom_text(df, mapping=aes(label=price, hjust=-0.2))
