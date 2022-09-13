# BLS.gov

#CUUR0000SETB01 = Gasoline (all types) in U.S. city average, all urban consumers, not seasonally adjusted

#bls_cuu <- read_tsv("D:/opendata/bls_cu.data.0.Current.txt")
bls_cuu <-  read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation")
max_y <- max(bls_cuu %>% filter(series_id == "CUUR0000SETB01")  %>% select((value)))

bls_cuu %>% 
  filter(series_id == "CUUR0000SETB01") %>% 
  filter(period != "M13" & year >= 1990) %>% 
  mutate(month = str_replace(period, "M", ""),
         year_month = as.Date(paste0(year, "-", month, "-01")),
         maxval = max(value)) %>% 
  ggplot(aes(x = year_month, y = value, group = 1)) +
  geom_point() +
  geom_line() +
  #  geom_smooth() +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(limits=c(0, max_y)) +
  labs(title = "Gas price index",
       subtitle = "Gasoline (all types) in U.S. city average, all urban consumers, not seasonally adjusted",
       x = "Month",
       y = "Index (1982-84=100)",
       caption = "@bdill\ndata: https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation\nSeries: CUUR0000SETB01")
