

rm(list = ls())
library(tidyverse)

ed <- read_csv("https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv")

ed %>% 
  group_by(country) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  View()

ed %>% 
  filter(date >= "2019-01-01") %>% 
  select(country, date, excess_deaths) %>% 
  ggplot(aes(date, excess_deaths, group = country, color = country)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position="none",
        axis.text = element_text(size=7),
        axis.text.x = element_text(size=7, angle = 90)) +
  labs(title = "Excess Deaths",
       subtitle = "All Countries",
       y = "Excess Deaths",
       caption = "graph: @bdill   data: https://github.com/Financial-Times/coronavirus-excess-mortality-data")

ggsave(filename = "output/ft_covid19_excess_deaths_all_countries.png", width = 10, height = 6, dpi = 120)


ed %>% 
  filter(country %in% c("US", "Italy", "Spain", "UK", "France", "Germany")) %>% 
  filter(date >= "2019-01-01") %>% 
  select(country, date, excess_deaths) %>% 
  ggplot(aes(date, excess_deaths, group = country, color = country)) +
  geom_line() +
  facet_wrap(~country) +
  theme(legend.position="none",
        axis.text = element_text(size=12)) +
  labs(title = "Excess Deaths",
       subtitle = "Top Countries",
       y = "Excess Deaths",
       caption = "graph: @bdill   data: https://github.com/Financial-Times/coronavirus-excess-mortality-data")

ggsave(filename = "output/ft_covid19_excess_deaths_top_countries.png", width = 10, height = 6, dpi = 120)
