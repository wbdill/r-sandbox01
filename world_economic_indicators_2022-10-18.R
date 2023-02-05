# world_economic_indicators_2022-10-18
# bdill 2022-10-18

library(tidyverse)
library(rvest)
library(countrycode)  # https://www.rdocumentation.org/packages/countrycode/versions/1.4.0/topics/countrycode

#----- World GDP/capita -----
gdp_cap <- read_html("https://tradingeconomics.com/country-list/gdp-per-capita?continent=world") |>
    html_elements("table") |>
    html_table() |>
    bind_rows() |>
    mutate(Reference = lubridate::my(Reference))

# ----- iso3 country codes from countrycode pkg. (takes several seconds) -----
# names(countrycode::codelist)
# gdp_cap <- gdp_cap |>
#     mutate(iso3 = countrycode::countrycode(
#         sourcevar = Country,
#         origin = "country.name",
#         destination = "iso3c")
#     )
# countrycode::countrycode("France", "country.name", "un.region.name")
countrycode::countrycode("Japan", "country.name", "iso3c")
countrycode::countrycode("United Kingdom", "country.name", "currency")
countrycode::countrycode("Great Britain", "country.name", "currency")
countrycode::countrycode("United States", "country.name", "iso3c")
countrycode::countrycode("The United States of America", "country.name", "iso3c")
# countrycode::codelist |> count(un.region.name)
# countrycode::codelist |> count(un.regionsub.name)
# countrycode::codelist |> count(continent)
# countrycode::codelist |> count(region)

#----- G20 Gasoline -----
gas <- read_html("https://tradingeconomics.com/country-list/gasoline-prices") |>
    html_elements("table") |>
    html_table() |>
    bind_rows() |>
    mutate(Reference = lubridate::my(Reference))

gas |>
    ggplot(aes(x = reorder(Country, Last), y = Last,
               fill = factor(ifelse(Country == "United States", "Highlighted", "Normal"))  # highlight one or more columns
               ) ) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none") +
    #theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(name = "Country", values=c("red","#98a5fb")) +
    labs(title = "Gas Prices in G20 Countries"
         , subtitle = "USD/liter"
         , x = "Country"
         , y = "$ / liter"
         , caption = "@bdill\ndata: https://tradingeconomics.com/country-list/gasoline-prices")

#----- G20 inflation -----
infl_g20 <- read_html("https://tradingeconomics.com/country-list/inflation-rate") |>
  html_elements("table") |>
  html_table() |>
  bind_rows() |>
  mutate(Reference = lubridate::my(Reference))

infl_g20 |>
  ggplot(aes(x = reorder(Country, Last), y = Last,
             fill = factor(ifelse(Country == "United States", "Highlighted", "Normal"))  # highlight one or more columns
  ) ) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  #theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(name = "Country", values=c("red","#98a5fb")) +
  labs(title = "Inflation in G20 Countries"
       , subtitle = ""
       , x = "Country"
       , y = "Inflation Rate"
       , caption = "@bdill\ndata: https://tradingeconomics.com/country-list/inflation-rate")
