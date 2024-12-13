
# #R.version
#install.packages("blscrapeR")
#install.packages("fredr")
#devtools::install_github("keberwein/blscrapeR")
#install.packages("tidyverse")
library(googlesheets4)
library(fredr)
library(blscrapeR)  # https://github.com/keberwein/blscrapeR
library(tidyverse)
#readRenviron("~/.Renviron")
#Sys.getenv("BLS_KEY")

#----- Read my Google Sheets -----
#Read google sheets data into R
?read_sheet
govpartycontrol <- read_sheet('https://docs.google.com/spreadsheets/d/1F2cM7c4QZ9SztkIjE9JX-l9no9p1gZj0CadSPZ8fRY0/edit#gid=1000754199', col_types="c")
#str(govpartycontrol)
govpartycontrol$Year <- as.integer(govpartycontrol$Year)

recessions <- read_sheet('https://docs.google.com/spreadsheets/d/1VqPOJmeD6nUs_y3J1PWwnyJqO0XSn-SM_reUyhgVzgA/edit#gid=0') %>%
  janitor::clean_names() %>%
  mutate(start_date = as_date(start_date)
         ,end_date = as_date(end_date))
#str(recessions)

potus <- read_sheet("https://docs.google.com/spreadsheets/d/1IME6v6dK1BxMbKI5stX28ehja4o4KglGMzd15ejeDmQ/edit#gid=0") %>%
  janitor::clean_names() %>%
  mutate(term_start = as_date(term_start), 
         term_end = as_date(term_end), 
         hex_color = case_when(party == "Democratic" ~ "#9999ff",
                               party == "Republican" ~ "#ffaaaa")
         )
#str(potus)

#----- trim recession and potus data to BLS df data -----
recessions_trim <- subset(recessions, start_date >= "1967-01-01") %>% select(start_date, end_date)
potus_trim <- subset(potus, term_end >= min(recessions_trim$start_date))

#----- Get  Unemployment data from BLS API -----
#ids <- search_ids(keyword = c("cpi"), periodicity_code = "M")
tbl0 <- bls_api(c("LNS14000000", "LNS13327708", "LNS13327709"), startyear = 2005, endyear = 2025, Sys.getenv("BLS_KEY"))  # max of 20 years per call
tbl1 <- bls_api(c("LNS14000000", "LNS13327708", "LNS13327709"), startyear = 1985, endyear = 2005, Sys.getenv("BLS_KEY"))
tbl2 <- bls_api(c("LNS14000000", "LNS13327708", "LNS13327709"), startyear = 1965, endyear = 1985, Sys.getenv("BLS_KEY"))
tbl3 <- bls_api(c("LNS14000000", "LNS13327708", "LNS13327709"), startyear = 1945, endyear = 1965, Sys.getenv("BLS_KEY"))
bls_unem <- rbind(tbl0 %>% select(-latest), tbl1, tbl2, tbl3) %>% mutate(value = value / 100)
bls_unem %>% group_by(seriesID) %>% count()
#bls_unem <- rbind(tbl0, tbl1, tbl2) %>% mutate(value = value / 100)
rm(list = c("tbl0", "tbl1", "tbl2", "tbl3"))  #cleanout tmp vars

bls_unem_wide <- bls_unem %>%
  pivot_wider(names_from = seriesID, values_from = value) %>%
  dateCast() %>%
  rename(u3_unemployment = LNS14000000, u5_unemployment = LNS13327708, u6_unemployment = LNS13327709)

bls_unem_trim <- bls_unem_wide %>% filter(date >= "1967-01-01") 

#----- Get  inflation data from FRED API -----
#tbl0 <- bls_api(c("CUSR0000SA0"), startyear = 2005, endyear = 2025, Sys.getenv("BLS_KEY"))  # max of 20 years per call (Jan 1948)
#tbl1 <- bls_api(c("CUSR0000SA0"), startyear = 1985, endyear = 2005, Sys.getenv("BLS_KEY"))  # Jan 1994
#tbl2 <- bls_api(c("CUSR0000SA0"), startyear = 1965, endyear = 1985, Sys.getenv("BLS_KEY"))  # Jan 1994
#bls_infl <- rbind(tbl0 %>% select(-latest), tbl1, tbl2) %>% mutate(value = value / 100)
#bls_infl %>% mutate(date = as.Date(paste0(year,"-", str_replace(period, "M", ""), "-01")))
#bls_infl_adj <- blscrapeR::inflation_adjust("1967-01-01") %>% 
#  arrange(desc(date))

#rm(list = c("cpi", "cpi_pctyoy"))  #cleanout tmp vars
# headline inflation percent year over year
# https://fred.stlouisfed.org/docs/api/fred/series_observations.html#units
infl_cpi_pctyoy <- fredr::fredr(series_id = "CPIAUCSL"
            , observation_start = as.Date("1947-01-01")
            , observation_end = as.Date("2024-11-01")
            , units = "pc1" # pc1 = percent change from a year ago
            # lin = Levels (No transformation)
            # chg = Change
            # ch1 = Change from Year Ago
            # pch = Percent Change
            # pc1 = Percent Change from Year Ago
            # pca = Compounded Annual Rate of Change
            # cch = Continuously Compounded Rate of Change
            # cca = Continuously Compounded Annual Rate of Change
            # log = Natural Log
      )
# core inflation percent year over year
infl_pcepi_pctyoy <- fredr(series_id = "CPILFESL"
                    , observation_start = as.Date("1947-01-01")
                    , observation_end = as.Date("9999-12-31")
                    , units = "pc1" # pc1 = percent change from a year ago
)
infl_pctyoy <- rbind(infl_cpi_pctyoy, infl_pcepi_pctyoy) %>% 
  mutate(series_name = case_when(
    series_id == "CPIAUCSL" ~ "Headline Inflation"
    , series_id == "CPILFESL" ~ "Core Inflation"
  ))
# Net new jobs (PAYEMS)
jobs_payems_val <- fredr(series_id = "PAYEMS"
                         , observation_start = as.Date("1940-01-01")
                         , observation_end = as.Date("9999-12-31")
                         , units = "lin" # lin = levels (no transformation)
)
# Net new jobs (PAYEMS) change from previous month
jobs_payems_chg <- fredr(series_id = "PAYEMS"
                            , observation_start = as.Date("1940-01-01")
                            , observation_end = as.Date("9999-12-31")
                            , units = "chg" # pc1 = percent change from a year ago
                          )
gdpc1_val <- fredr(series_id = "GDPC1"
                   , observation_start = as.Date("1940-01-01")
                   , observation_end = as.Date("9999-12-31")
                   , units = "lin" # lin = Levels (No transformation)
)

gdpc1_compounded_annual_rateofchg <- fredr(series_id = "GDPC1"
                         , observation_start = as.Date("1940-01-01")
                         , observation_end = as.Date("9999-12-31")
                         , units = "pca" # pca = Compounded Annual Rate of Change
)

#----- Write to CSV files ------
bls_unem_wide %>% write_csv("D:/opendata/FRED/bls_unem_wide.csv", na = "")
infl_cpi_pctyoy %>% write_csv("D:/opendata/FRED/infl_cpi_pctyoy.csv", na = "")
infl_pcepi_pctyoy %>% write_csv("D:/opendata/FRED/infl_pcepi_pctyoy.csv", na = "")
jobs_payems_val %>% write_csv("D:/opendata/FRED/jobs_payems_val.csv", na = "")
jobs_payems_chg %>% write_csv("D:/opendata/FRED/jobs_payems_chg.csv", na = "")
gdpc1_val %>% write_csv("D:/opendata/FRED/gdpc1_val.csv", na = "")
gdpc1_compounded_annual_rateofchg %>% write_csv("D:/opendata/FRED/gdpc1_compounded_annual_rateofchg.csv", na = "")

#rm(list = c("infl_cpi_pctyoy", "infl_pcepi_pctyoy"))  #cleanout tmp vars

#---- Plot Inflation (D)/(R) highlights and recession highlights ----
ggplot() + #infl_pctyoy
  geom_rect(data = recessions_trim, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), fill='gray', alpha=0.7) +
  geom_rect(data = potus_trim, aes(xmin = term_start , xmax = term_end, ymin = -Inf, ymax = Inf, fill = party), alpha=0.5) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 2, alpha = 0.7, size = 2, color = "#77cc77") +
  geom_hline(yintercept = 4, alpha = 0.5) +
  geom_line(data=infl_cpi_pctyoy, aes(x = date, y = value, color = "Headline Inflation"), linewidth = 1.5) +
  geom_line(data=infl_pcepi_pctyoy, aes(x = date, y = value, color = "Core Inflation"), linewidth = 1.0) +

  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  #scale_y_continuous(labels = scales::percent, breaks = seq(0, .24, .02)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1967-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "Inflation Rates"
       , subtitle = "Percent Change from a Year Ago"
       , x = "Date"
       , y = "Percent"
       , fill = "President"
       , color='Data Series'
       , caption = "Chart: @bdill (X/Twitter)   @wbdill (threads.net)\nData: FRED.  Series: Headline Inflation (CPIAUCSL), Core Inflation (CPILFESL)")


#---- Plut U3 and U6 with (D)/(R) highlights and recession highlights ----
ggplot(bls_unem_trim) + 
  geom_rect(data = recessions_trim, aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = Inf), fill='gray', alpha=0.7) +
  geom_rect(data = potus_trim, aes(xmin = term_start , xmax = term_end, ymin = 0, ymax = Inf, fill = party), alpha=0.5) +
  
  geom_line( aes(x = date, y = u3_unemployment, color = "U-3 Unemployment"), linewidth = 1.0) +
  #geom_line(aes(y = u5_unemployment, color = "U-5 Unemployment")) + 
  geom_line(aes(x = date, y = u6_unemployment, color = "U-6 Unemployment"), linewidth = .8) + 
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, .24, .02)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1977-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
    theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "Monthly Unemployment Rates"
       , subtitle = "U3 = Headline Unemployment\nU6 = Unemployment incl. those giving up looking"
       , x = "Date"
       , y = "Percent"
       , fill = "President"
       , color='Data Series'
       , caption = "@bdill (X/Twitter)  @wbdill (threads.net)\nData: BLS - Series: U3 (LNS14000000), U6 (LNS13327709)")


#----- Federal Annual Deficit -----
deficit_annual <- fredr::fredr("FYFSD", frequency = "a")
#str(potus_trim)

deficit_annual |> filter(date >= "1967-01-01") |> 
ggplot(aes(x = date, y = value)) +
  geom_rect(data=recessions_trim, inherit.aes=FALSE, aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.3) +
  geom_rect(data = potus_trim, inherit.aes=FALSE, aes(xmin = term_start , xmax = term_end, ymin = -Inf, ymax = Inf, fill = party), alpha=0.5) +
  
  geom_line( size = 1.5, color = "#4499CC") +
  geom_point(size = 1.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_hline(yintercept = 0, size = 1.2, alpha = 0.5) +
  geom_hline(yintercept = -1000000, alpha = 0.2) +
  geom_hline(yintercept = -2000000, alpha = 0.2) +
  geom_hline(yintercept = -3000000, alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "US Federal Deficit/Surplus",
       subtitle = "Higher is better",
       x = "",
       y = "Deficit/Suplus (M)",
       fill = "President",
       caption = "@bdill (X/Twitter)\n@wbdill (threads.net)\nData: FRED series FYFSD")

jobs_payems_chg %>% filter(date >= "1967-01-01") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_rect(data=recessions_trim, inherit.aes=FALSE, aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.3) +
  geom_rect(data = potus_trim, inherit.aes=FALSE, aes(xmin = term_start , xmax = term_end, ymin = -Inf, ymax = Inf, fill = party), alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_line( size = 1.0, color = "#4499CC") +
  #geom_point(size = .5) +
  #geom_hline(yintercept = 0, size = 1.2, alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  #scale_y_log10() +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "US New Jobs",
       #subtitle = "",
       x = "",
       y = "Jobs Created",
       fill = "President",
       caption = "@bdill (X/Twitter)\n@wbdill (threads.net)\nData: FRED series PAYEMS - monthly change")

gdpc1_compounded_annual_rateofchg %>% 
  ggplot(aes(x = date, y = value)) +
  geom_rect(data=recessions_trim, inherit.aes=FALSE, aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.3) +
  geom_rect(data = potus_trim, inherit.aes=FALSE, aes(xmin = term_start , xmax = term_end, ymin = -Inf, ymax = Inf, fill = party), alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_line( size = 1.0, color = "#4499CC") +
  #geom_point(size = .5) +
  geom_hline(yintercept = 0, alpha = 0.2) +
  geom_hline(yintercept = 2, alpha = 0.2) +
  scale_y_continuous(labels = scales::comma) +
  #scale_y_log10() +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "Real GDP",
       #subtitle = "",
       x = "",
       y = "",
       fill = "President",
       caption = "@bdill (X/Twitter)\n@wbdill (threads.net)\nData: FRED series GDPC1 - compounded annual rate of change")


#-----
str(gdpc1_compounded_annual_rateofchg)
str(govpartycontrol)
#govpartycontrol$Year = as.numeric(govpartycontrol$Year)
gdp_join <- gdpc1_compounded_annual_rateofchg %>% 
  mutate(Year = year(date)) %>% 
  inner_join(govpartycontrol, join_by(Year == Year) ) 

# since Reagan - (D) > (R)
gdp_join %>% 
  filter(date >= "1981-01-01") %>% 
  select(date, value, PresidentParty) %>% 
  group_by(PresidentParty) %>% 
  summarize(value = sum(value, na.rm = TRUE)
            , quarters = n()
            , avg = value / quarters) 

gdp_join %>% 
  #filter(date >= "1981-01-01") %>% 
  select(date, value, PresidentParty, President) %>% 
  group_by(President, PresidentParty) %>% 
  summarize(value = sum(value, na.rm = TRUE)
            , quarters = n()
            , avg = value / quarters
            , firstdate = min(date)) %>% 
  arrange(firstdate) %>% 
  ggplot(aes(x=reorder(President, firstdate), y = avg, group = PresidentParty, fill = PresidentParty)) +
  geom_col() +
  geom_text(aes(label = quarters), nudge_y = .25) +
  scale_fill_manual(values = c("#aaaaff", "#ffaaaa")) +
  labs(  title = "Average GDP by POTUS"
      , x = "President"
      , y = "Avg Real GDP Growth"
      , caption = "@bdill (X/Twitter)\n@wbdill (threads.net)\nData: FRED series GDPC1 - compounded annual rate of change")
