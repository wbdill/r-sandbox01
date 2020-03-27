# New York Times covid19 data
# Brian Dill 2020-03-27
# https://github.com/nytimes/covid-19-data
# https://en.wikipedia.org/wiki/FIPS_county_code


nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

nyt_counties %>%
  filter(state == "Tennessee" & county == "Davidson") %>%
  arrange(desc(date))

top7_tn_counties <- nyt_counties %>%
  filter(state == "Tennessee" & date == "2020-03-25") %>%
  top_n(7, cases) %>%
  arrange(desc(cases))

pull(top7_tn_counties, county)

nyt_counties %>%
  filter(state == "Tennessee" & county %in% pull(top7_tn_counties, county)) %>%
  ggplot(aes(date, cases, color = county)) +
  geom_line(size = .7) +
  labs(title = "Cumulative covid19 Cases - Tennessee",
      subtitle = "Top 7 Counties",
      y = "Cumulative Cases",
      caption = "graph: @bdill   data: https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv")
ggsave(filename = "output/nytimes_top7_tn_counties.png", width = 16, height = 10, units = "cm")
