# Countries
# 

#----- Country populations -----\
# https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes


countries <- read_tsv("D:/opendata/countries_iso3166b.tsv")
countries2 <- read_tsv("https://pastebin.com/raw/uy3tesm6")
population <- read_csv("D:/opendata/population.csv")

trouble_countries <- countries %>% 
  left_join(population, by = c("iso3" = "CountryCode")) %>% 
  select(Country, iso2, iso3, CountryName) %>% 
  distinct() %>% 
  filter(Country != CountryName | is.na(CountryName)) %>% 
  arrange(CountryName)


countries %>% 
  left_join(population, by = c("iso3" = "CountryCode")) %>% 
  filter(iso3 %in% c("USA", "CHN", "IND", "BRA", "RUS")) %>% 
  select(Country,CountryName, Year, Population = Value) %>% 
  mutate(popM = Population / 1000000) %>% 
  ggplot()+
  geom_line(aes(Year, popM, color=CountryName), size = .5) +
  #scale_y_log10() +
  labs(title = "Country Population",
       y = "Population (Million)",
       color = "Country",
       caption = "@bdill Data: World bank via https://datahub.io/core/population")
  
ggsave("D:/opendata/country_population.png", width = 8, height = 6, dpi = 150)

dat <- read_csv("https://datahub.io/core/country-list/r/0.csv")

#----- Country Aliases -----
aliases <- read_tsv("https://pastebin.com/raw/faA6c3ND")
aliases %>% 
  mutate(is_common = case_when (
    str_detect(AliasDescription, "common$") ~ 1,
    str_detect(AliasDescription, "common, English$") ~ 1,
    TRUE ~ 0
  ),
  is_official = case_when (
    str_detect(AliasDescription, "official$") ~ 1,
    str_detect(AliasDescription, "official, English") ~ 1,
    TRUE ~ 0
  )) %>% 
  filter(is_common == 1) %>% 
  View()

write_csv(aliases, "D:/opendata/country aliases (wiki-List of alternative country names).csv")
