library(tidyverse)

# CDC Data: https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Demographics-in-the-United-St/km4m-vcsb

df <- read_csv("C:/Users/bdill/Downloads/COVID-19_Vaccination_Demographics_in_the_United_States_National.csv")
str(df)
df$Date <- lubridate::mdy(df$Date)
df %>% group_by(Demographic_category) %>% count() %>% View()


df %>% filter(Demographic_category %in% c("Sex_Male", "Sex_Female")) %>% 
  select(Date, Demographic_category, Series_Complete_Pop_pct_agegroup) %>% 
  ggplot(aes(x = Date, y = Series_Complete_Pop_pct_agegroup)) +
  geom_line(aes(group = Demographic_category, color = Demographic_category), size = 1) +
  scale_x_date( breaks = scales::date_breaks("2 months"), date_labels = "%b %Y") +
  labs(title = "CDC Vaccinations",
       caption = "chart: @bdill\nData: https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Demographics-in-the-United-St/km4m-vcsb")




plotit <- function(df, subt = "") {
  df %>% 
    ggplot(aes(x = Date, y = Series_Complete_Pop_pct_agegroup)) +
    geom_line(aes(group = Demographic_category, color = Demographic_category), size = 1.2) +
    scale_x_date( breaks = scales::date_breaks("2 months"), date_labels = "%b %Y") +
    labs(title = "Fully Vaccinated (CDC)",
         subtitle = subt,
         caption = "chart: @bdill\nData: https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Demographics-in-the-United-St/km4m-vcsb")  

}
df1 <- df %>% filter(Demographic_category %in% c("Sex_Male", "Sex_Female")) %>% 
  select(Date, Demographic_category, Series_Complete_Pop_pct_agegroup) 

plotit(df1, "By Sex")

df2 <- df %>% filter(Demographic_category %in% c("Race_eth_NHAsian", "Race_eth_NHBlack", "Race_eth_Hispanic", "Race_eth_NHAIAN", "Race_eth_NHWhite", "Race_eth_NHNHOPI")) %>% 
  select(Date, Demographic_category, Series_Complete_Pop_pct_agegroup) 

plotit(df2)

df3 <- df %>% filter(Demographic_category %in% c("Ages_12-15_yrs","Ages_16-17_yrs","Ages_18-24_yrs","Ages_25-39_yrs","Ages_40-49_yrs","Ages_50-64_yrs","Ages_65-74_yrs","Ages_75+_yrs")) %>% 
  select(Date, Demographic_category, Series_Complete_Pop_pct_agegroup) 

plotit(df3, "By Age Group")
  








