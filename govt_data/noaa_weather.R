# Gets historical weather for your nearest USGS weather station
# https://www.youtube.com/watch?v=V5Df6vw4-e8
# https://github.com/riffomonas/climate_viz/blob/b126cc31e7e4d13feb8b5047795e0a243d81f64a/code/local_weather.R

library(tidyverse)
library(glue)
library(lubridate)

# can get from Google maps by right clicking on any point
my_lat <- 35.988401731321595 * 2 * pi / 360  # convert to radians
my_lon <- -86.70469574776531 * 2 * pi / 360

my_lat <- 35.93565467227173 * 2 * pi / 360  # convert to radians
my_lon <- -86.90014983569537 * 2 * pi / 360


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"
inventory <- read_table(inventory_url, col_names = c("station", "lat", "lon", "variable", "start", "end"))

# Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
# The obtained distance, d, is in miles. If you want your value to be in units of kilometers, multiple d by 1.609344.
# d in kilometers = 1.609344 * d in miles

stations |> group_by(str_sub(station, 1, 2)) |> count() |> arrange(-n) |> View()

#----- Determine Station -----
stations <- inventory %>%
  group_by(station, lat, lon ) |> 
  summarize(start = min(start),
            end = max(end)) |> 
  mutate(lat_r = lat * 2 * pi / 360,
         lon_r = lon * 2 * pi / 360,
         miles_away =3963 * acos( (sin(lat_r) * sin(my_lat)) + cos(lat_r) * cos(my_lat) * cos(my_lon - lon_r) )
  ) |> 
  ungroup() |> 
  arrange(miles_away)
  

#stations |> filter(start < 1960 & end > 2020) |> head(5) |> View()

my_station <- stations %>%
  filter(start < 1960 & end >= 2022) %>%
  slice_min(miles_away, n = 1) %>%
  distinct(station) %>%
  pull(station)
#"USW00013897" # airport
#"USC00401039" # B'wood fire station 2
#"USC00403280" # Founder's Pointe

?top_n
mtcars %>% slice(1L)
mtcars %>% slice_min(mpg, n = 3)
str(stations)
stations |> slice_min(miles_away, n=3)

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

#---- Pull weather for my station -----
local_weather <- read_csv(station_daily,
                          col_names = c("station", "date", "variable", "value", "a", "b", "c", "d")) %>%
  select(date, variable, value) %>%
  pivot_wider(names_from = "variable", values_from="value",
              values_fill = 0) %>%
  select(date, TMAX, TMIN, PRCP, SNOW) %>%
  mutate(date = ymd(date))

local_weather2 <- local_weather |> 
  mutate(MaxTempF = (TMAX/10) * 9/5 + 32 ,
         MinTempF = (TMIN/10) * 9/5 + 32 ,
         PrecipInch = (PRCP / 10) * 0.0394,
         SnowInch = SNOW * 0.0394,
         year = year(date),
         yearmonth = str_sub(date, 1, 7),
         month = str_sub(date, 6,7)
         ) |> 
  relocate(c(year, month, yearmonth), .after = date) |> 
  select(-TMAX, -TMIN, -PRCP, -SNOW)

local_weather_tidy <- local_weather2 |> 
  group_by(year, yearmonth, month) |> 
  summarize(AvgHighF = mean(MaxTempF),
            AvgLowF = mean(MinTempF)) |> 
  mutate(date = as.Date(paste0(yearmonth, "-01"))) |> 
  pivot_longer(cols = AvgHighF:AvgLowF, names_to = "element", values_to = "value") 

#----- Plot -----

local_weather2 |> pivot_longer(cols = MaxTempF:SnowInch, names_to = "element", values_to = "value") |> 
  filter(year %in% c(2022)) |> 
  filter(element %in% c("MaxTempF", "MinTempF")) |> 
  ggplot(aes(x = date, y = value, group = element, color = element)) +
  geom_hline(yintercept = 32) +
  geom_line(size = 1) +
  geom_point()
  #geom_bar(stat = "identity", position = "dodge", aes(fill = element))



#----- Avg monthly hi/low 1990 - present facet -----
local_weather_tidy |> 
  #filter(year >= 1990) |>
  filter(year %in% c(1970, 2022)) |> 
  ggplot(aes(x = date, y = value, group = element, color = element)) +
  geom_hline(yintercept = 32) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_date(date_breaks = "1 months", date_labels = "%m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(. ~ year, scales = "free_x")

#-----
local_weather2 |> 
  group_by(year, month, yearmonth) |> 
  summarize(AvgTempHiF = mean(MaxTempF),
            AvgTempLoF = mean(MinTempF)) |> 
  filter(month == "07") |> 
  pivot_longer(cols = AvgTempHiF:AvgTempLoF, names_to = "element", values_to = "value") |> 
  mutate(date = as.Date(paste0(yearmonth, "-01"))) |> 
  ggplot(aes(x = yearmonth, y = value, color = element, group = element)) +
  geom_line( size = 1) +
  geom_point(size = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(
      plot.title = element_text(size = 20, hjust = .5)   #this sets the size of the title at the top
  ) +
  labs(title = "Avg Apr Temps for Nashville Airport",
       x = "Year",
       y = "Temperature (F)",
       color = "Measurement",
       caption = "Chart: @bdill\nData: noaa.gov\nWx Station: USW00013897")

local_weather2 |> 
  group_by(year) |> 
  filter(year < 2023) |> 
  summarize(AvgTempHiF = mean(MaxTempF),
            AvgTempLoF = mean(MinTempF)) |> 
  #filter(month == "07") |> 
  pivot_longer(cols = AvgTempHiF:AvgTempLoF, names_to = "element", values_to = "value") |> 
  #mutate(date = as.Date(paste0(yearmonth, "-01"))) |> 
  ggplot(aes(x = year, y = value, color = element, group = element)) +
  geom_line( size = 1) +
  geom_point(size = 1.5) +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks = c(1950,1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(
    plot.title = element_text(size = 20, hjust = .5)   #this sets the size of the title at the top
  ) +
  labs(title = paste0("Avg Annual Temps for ", my_station),
       x = "Year",
       y = "Temperature (F)",
       color = "Measurement",
       caption = "Chart: @bdill\nData: noaa.gov\nWx Station: USW00013897")

