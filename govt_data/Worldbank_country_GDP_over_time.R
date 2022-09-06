# Worldbank GDP data
# 2022-09-05
# https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CD&country=#
#gdp <- read_csv("D:/opendata/GDP_by_country_1960_2021.csv")
gdp <- read_csv("https://pastebin.com/raw/A1wNeuYT")
gdp <- janitor::clean_names(gdp)

#gdpwide <- pivot_wider(gdp, names_from = year, values_from = value )
gdp_long <- pivot_longer(gdp, cols = starts_with("x"), names_to = "year", values_to = "gdp")
gdp_long$year <- str_remove(gdp_long$year, "x")
str(gdp_long)

write_csv(gdp_long, "D:/opendata/GDP_by_country_1960_2021_long.csv")

gdp_long %>% filter(year == 2020 & row_type == "country") %>% top_n(8, gdp) %>% 
  select(country_name)

# Top 6 countries
gdp_long %>% 
  #filter(row_type == "country") %>% 
  #filter(country_name %in% c("United States", "United Kingdom", "China", "European Union", "India", "Russian Federation")) %>%
  filter(country_name %in% c("United States", "China", "Germany", "India", "Japan", "United Kingdom")) %>%
  ggplot(aes(x = year, y = gdp / 1000000000, group = country_name, color = country_name)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(label = scales::comma) +
  labs(title = "Country GDP (current US$)"
       , subtitle = "Top 6 Countries"
       , x = "Year"
       , y = "GDP ($ Billion)"
       , color = "Country"
       , caption = "@bdill\ndata: databank.worldbank.org")

# Countries 3-8
gdp_long %>% 
  filter(country_name %in% c("Germany", "India", "Japan", "United Kingdom", "France", "Italy")) %>% 
  ggplot(aes(x = year, y = gdp / 1000000000, group = country_name, color = country_name)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(label = scales::comma) +
  labs(title = "Country GDP (current US$)"
       , subtitle = "Countries 2-8 (excl: US and China)"
       , x = "Year"
       , y = "GDP ($ Billion)"
       , color = "Country"
       , caption = "@bdill\ndata: databank.worldbank.org")


