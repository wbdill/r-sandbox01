library(tidyverse)
rm(list = ls())
#path <- "C:/Users/bdill/Downloads/DECENNIALPL2020.P1_2021-09-29T004217/DECENNIALPL2020.P1_data_with_overlays_2021-09-17T080619.csv"
path <- "http://briandill.com/data/census/DECENNIALPL2020.P1/DECENNIALPL2020.P1_data_with_overlays_2021-09-17T080619.csv"
df <- read_csv(path, skip = 2, col_names = FALSE)

df <- df[,1:11]
colnames(df) <- c("geoid", "name", "total", "one", "white", "black", "indian", "asian", "pacific", "other", "twoormore")
df <- separate(df, name, into = c("county", "state"), ", ")
df <- df %>% mutate(fips_county = substring(geoid, 10),
                    fips_state = substring(geoid, 10, 11),
                    white_pct = round(white*100 / total, 3),
                    black_pct = round(black*100 / total, 3),
                    indian_pct = round(indian*100 / total, 3),
                    asian_pct = round(asian*100 / total, 3),
                    pacific_pct = round(pacific*100 / total, 3),
                    other_pct = round(other*100 / total, 3),
                    twoormore_pct = round(twoormore*100 / total, 3) ) %>% 
  select(geoid, fips_state, fips_county, county, state, total, white, black, indian, asian, pacific, other, twoormore, one,
         white_pct, black_pct, indian_pct, asian_pct, pacific_pct, other_pct, twoormore_pct)

write_csv(df, "D:/opendata/census.gov/DECENNIALPL2020.P1_2021-09-29T004217/census_2020_race_by_county.csv")

df %>% arrange(desc(white_pct)) %>% select(county, state, total, white, white_pct) %>% head(10) #%>% group_by(state) %>% count() %>% arrange(desc(n))
df %>% arrange(desc(black_pct)) %>% select(county, state, total, black, black_pct) %>% head(10) #%>% group_by(state) %>% count() %>% arrange(desc(n))

df %>% arrange(desc(indian_pct)) %>% select(county, state, total, indian, indian_pct) %>% head(10)
df %>% arrange(desc(asian_pct)) %>% select(county, state, total, asian, asian_pct) %>% head(10)
df %>% arrange(desc(pacific_pct)) %>% select(county, state, total, pacific, pacific_pct) %>% head(10)
df %>% arrange(desc(other_pct)) %>% select(county, state, total, other, other_pct) %>% head(10)
df %>% arrange(desc(twoormore_pct)) %>% select(county, state, total, twoormore, twoormore_pct) %>% head(10)

df %>% filter(state == "Puerto Rico") %>% View()
