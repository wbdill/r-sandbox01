# COVID vaccination rates by county over time
# COVID19_case_surveillance_public_use_data
library(tidyverse)
#rm(list = ls())

# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
# https://bit.ly/3oAMW7l
path <- "C:/Users/bdill/Downloads/COVID-19_Vaccinations_in_the_United_States_County_2021-10-05.csv"
df <- read_csv(path)
df$Date <- lubridate::mdy(df$Date)
str(df)

names(df)

#df2 <- data.table::fread(path)
#str(df2)

plot_vax <- function(df, statename) {
  ggplot(df, aes(x = Date, y = Series_Complete_Pop_Pct, group = Recip_County)) +
  geom_line(aes(color = Recip_County), size = 1.2) +
  labs(title = paste("Total Vaccination Rate:", statename),
       subtitle = "Data: CDC",
       caption = "chart: @bdill\nData: CDC.  https://bit.ly/3oAMW7l")
}

#----- Mississippi -----
ms <- df %>% 
  filter(Recip_State == "MS", Recip_County %in% c("Lowndes County","Oktibbeha County", "Warren County", "Hinds County", "Jefferson County", "Smith County" ) ) %>% 
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct, Series_Complete_Yes)
plot_vax(ms, "Mississippi")

#----- Tennessee  -----
tn <- df %>% 
  filter(Recip_State == "TN", Recip_County %in% c("Davidson County","Williamson County", "Rutherford County", "Shelby County", "Wilson County", "Moore County", "Meigs County" ) ) %>% 
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct, Series_Complete_Yes)
plot_vax(tn, "Tennessee")

#----- Louisiana -----
la <- df %>% 
  filter(Recip_State == "LA", Recip_County %in% c("West Feliciana Parish","Orleans Parish", "Jefferson Parish", "Cameron Parish" ) ) %>% 
  select(Date, FIPS, Recip_County, Recip_State, Series_Complete_Pop_Pct, Series_Complete_Yes)
plot_vax(la, "Louisiana")


df %>% filter(Recip_State == "TN", Date == "2021-10-04") %>% 
  select(Recip_State, Recip_County, Series_Complete_Pop_Pct) %>% 
  arrange(desc(Series_Complete_Pop_Pct))

df %>% filter(Recip_State == "LA", Date == "2021-10-04") %>% 
  select(Recip_State, Recip_County, Series_Complete_Pop_Pct) %>% 
  arrange(desc(Series_Complete_Pop_Pct))
