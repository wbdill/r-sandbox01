library(tidyverse)
library(rvest)
#install.packages("janitor")

# https://www.gunviolencearchive.org/reports

#html <- rvest::read_html(GetUrl(0))
#html <- xml2::read_html(GetUrl(0))

lst_tables <- list()
page <- 0

repeat {
  html <- rvest::read_html( paste0("https://www.gunviolencearchive.org/mass-shooting?page=", page, "&sort=asc&order=Incident%20Date"))
  curr_table <-  html %>% html_node("table") %>% html_table(header = T, fill=T)
  lst_tables[[length(lst_tables) + 1]] <- curr_table

  page <- page + 1
  if(page == 80) { break }  # if no more data is returned exit the loop
  Sys.sleep(1) # pause for a sec in between calls
}


append_csv_to_list <- function(filename) {
  df <- read_csv(paste0("D:/opendata/gunviolencearchive.org/", filename))  
  lst_tables[[length(lst_tables) + 1]] <<- df
}
#read_csv(paste0("D:/opendata/gunviolencearchive.org/", "mass_shootings_2019.csv")) 
append_csv_to_list("mass_shootings_2019.csv")
append_csv_to_list("mass_shootings_2020.csv")
append_csv_to_list("mass_shootings_2021.csv")
append_csv_to_list("mass_shootings_2022.csv")
append_csv_to_list("mass_shootings_2023.csv")

df_all <- data.table::rbindlist(lst_tables)
df_all_clean <- janitor::clean_names(df_all)
df_all_clean$incident_date <- lubridate::mdy(df_all_clean$incident_date)
str(df_all_clean)

df_all_clean <- distinct(df_all_clean, incident_id, .keep_all = TRUE) #dedupe
df_all_clean <- df_all_clean %>% 
  mutate(incident_year = year(incident_date)
         , incident_month = month(incident_date, label=TRUE, abbr = TRUE)
         , incident_weekday = wday(incident_date, label=TRUE, abbr=TRUE)
         , incident_day_of_month = day(incident_date))

df_all_clean %>% filter(state == "Tennessee") %>% View()
#df_all_clean <- read_csv(paste0("D:/opendata/gunviolencearchive.org/", "mass_shootings_2023-08-18.csv")) 
df_all_clean %>% 
  group_by(state, city_or_county) %>% 
  summarize(killed = sum(victims_killed), shootings = n()) %>% 
  filter(killed > 0) %>% 
  View()

df_all_clean %>% group_by(incident_year) %>% tally() %>% filter(incident_year >= 2013, incident_year < 2023) %>% 
  ggplot(aes(x = as.factor(incident_year), y = n)) + geom_col(fill="#9999ff") +
  labs(title = "US Mass Shooting Incidents by Year", x = "year", y = "Number of Incidents", caption = "@bdill\ndata: gunviolencearchive.org")

df_all_clean %>% group_by(incident_month) %>% tally() %>% 
  ggplot(aes(x = incident_month, y = n)) + geom_col(fill="#9999ff") +
  labs(title = "US Mass Shooting Incidents by Month", subtitle = "2013-2022", x = "", y = "Number of Incidents", caption = "@bdill\ndata: gunviolencearchive.org")

df_all_clean %>% group_by(incident_weekday) %>% tally() %>% 
  ggplot(aes(x = incident_weekday, y = n)) + geom_col(fill="#9999ff") +
  labs(title = "US Mass Shooting Incidents by Day of Week", subtitle = "2013-2022", x = "", y = "Number of Incidents", caption = "@bdill\ndata: gunviolencearchive.org")

df_all_clean %>% group_by(incident_day_of_month) %>% tally() %>% 
  ggplot(aes(x = incident_day_of_month, y = n)) + geom_col(fill="#9999ff") +
  labs(title = "US Mass Shooting Incidents by Day of Month", subtitle = "2013-2022", x = "", y = "Number of Incidents", caption = "@bdill\ndata: gunviolencearchive.org")


df_all_clean %>% #select(-operations) %>% 
  write_csv(paste0("D:/opendata/gunviolencearchive.org/", "mass_shootings_2023-08-18.csv"))  
#df_all_clean %>% group_by(incident_id) %>% count() %>% filter(n > 1) %>% View()

