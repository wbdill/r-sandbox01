# FRED data via API
# 2023-01-16 bdill

#install.packages("fredr")
library(tidyverse)
library(fredr)


recessions <- tribble(
  ~start, ~end,
  "1969-04-01","1970-11-01",
  "1973-11-01","1975-03-01",
  "1980-01-01","1980-07-01",
  "1981-07-01","1982-11-01",
  "1990-07-01","1991-03-01",
  "2001-03-01","2001-11-01",
  "2007-12-01","2009-06-01",
  "2020-02-01","2020-04-01"
)
recessions$start <- as.Date(recessions$start)
recessions$end <- as.Date(recessions$end)
#str(recessions)

presidents <- tribble(
  ~name, ~start, ~end, ~hexcolor, ~party,
  "Nixon",    "1969-01-20", "1974-08-08", "#ffcccc", "R",
  "Ford",     "1974-08-08", "1977-01-20", "#ffcccc", "R",
  "Carter ",  "1977-01-20", "1981-01-20", "#ccccff", "D",
  "Reagan",   "1981-01-20", "1989-01-20", "#ffcccc", "R",
  "Bush, HW", "1989-01-20", "1993-01-20", "#ffcccc", "R",
  "Clinton",  "1993-01-20", "2001-01-20", "#ccccff", "D",
  "Bush, W",  "2001-01-20", "2009-01-20", "#ffcccc", "R",
  "Obama",    "2009-01-20"," 2017-01-20", "#ccccff", "D",
  "Trump",    "2017-01-20"," 2021-01-20", "#ffcccc", "R",
  "Biden",    "2021-01-20", "2025-01-20", "#ccccff", "D",
)
presidents$start <- as.Date(presidents$start)
presidents$end <- as.Date(presidents$end)
presidents$party <- as.factor(presidents$party)
#str(presidents)

#===== Deficit (Annual) =====
#series <- fredr_series("FYFSD")
deficit_annual <- fredr("FYFSD", frequency = "a")
deficit_monthly <- fredr("MTSDS133FMS")

deficit_annual |> filter(date >= "1967-01-01") |> 
ggplot(aes(x = date, y = value)) +
  geom_rect(data=presidents, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=party),  alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_rect(data=recessions, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.1) +
  geom_line( size = 1.5, color = "#4499CC") +
  geom_point(size = 2) +
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
       #subtitle = "Source: FRED  -  Series: FYFSD",
       x = "",
       y = "Deficit/Suplus (M)",
       fill = "President",
       caption = "@bdill\nData: FRED series FYFSD")

#===== Deficit as % of GDP =====
deficit_gdp <- fredr("GFDEGDQ188S")
deficit_gdp |> filter(date >= "1968-01-01") |> 
  ggplot(aes(x = date, y = value)) +
  geom_rect(data=presidents, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=party),  alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_rect(data=recessions, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.1) +
  geom_line( size = 1.2, color = "#4499CC") +
  geom_point(size = 1.2) +
  geom_hline(yintercept = 100, size = 1.2, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 150) ) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "US Federal Deficit as % of GDP",
       #subtitle = "Source: FRED  -  Series: GFDEGDQ188S",
       x = "",
       y = "% of GDP",
       fill = "President",
       caption = "@bdill\nData: FRED series GFDEGDQ188S")

#=====  GDP =====
gdp <- fredr("GDP")
gdp |> filter(date >= "1968-01-01") |> 
  ggplot(aes(x = date, y = value)) +
  geom_rect(data=presidents, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=party),  alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_rect(data=recessions, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.1) +
  geom_line( size = 1.2, color = "#4499CC") +
  geom_point(size = 1.2) +
  #geom_hline(yintercept = 100, size = 1.2, alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "US GDP",
       #subtitle = "Source: FRED  -  Series: GDP (Seasonally Adjusted Annual Rate)",
       x = "",
       y = "GDP (Billion)",
       fill = "President",
       caption = "@bdill\nData: FRED series GDP (Seasonally Adjusted Annual Rate)")

#=====  GDP =====
gdp_real <- fredr("GDPC1")
gdp_real |> filter(date >= "1968-01-01") |> 
  ggplot(aes(x = date, y = value)) +
  geom_rect(data=presidents, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=party),  alpha=0.5) +
  scale_fill_manual(values = c("#ccccff", "#ffcccc")) +
  geom_rect(data=recessions, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="#556655", alpha=0.1) +
  geom_line( size = 1.2, color = "#4499CC") +
  geom_point(size = 1.2) +
  #geom_hline(yintercept = 100, size = 1.2, alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(as.Date("1968-01-01"), as.Date("2025-01-21")), expand = c(0,0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 22, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12)) +
  labs(title = "US Real GDP",
       #subtitle = "(2012 Dollars)",
       x = "",
       y = "GDP (Billion)",
       fill = "President",
       caption = "@bdill\nData: FRED series GDPC1 (2012 Dollars)")
