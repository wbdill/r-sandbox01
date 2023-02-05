# World Maps
# https://www.youtube.com/watch?v=FoqiFR5ZCic

#install.packages("rnaturalearth")
#install.packages("countrycode")
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggrepel)

world <- ne_countries(scale = "small", returnclass = "sf")

# https://proj.org/operations/projections
world |> 
  st_transform(crs = "+proj=robin") |>   # "+proj=igh"  "+proj=robin"  "+proj=ortho"
  ggplot() +
  geom_sf() +
  #coord_sf(datum = NA) +  # wintri some projections don't work unless you remove the datum
  theme_minimal()

#----- James Bond csv file -----
#https://www.kaggle.com/datasets/dreb87/jamesbond
data_raw <- read_csv("C:/Users/bdill/Downloads/jamesbond.csv")

data <- data_raw |> 
  select(Movie, Bond, Depicted_Film_Loc) |> 
  separate_rows(Depicted_Film_Loc, sep = ", ") |> 
  mutate(Depicted_Film_Loc = recode(Depicted_Film_Loc,
                                    "England" = "United Kingdom",
                                    "Scotland" = "United Kingdom")) |> 
  mutate(Visited = TRUE)


data_with_iso <- data |> 
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Depicted_Film_Loc,
    origin = "country.name",
    destination = "iso3c")
    )
data_with_iso


countries_visited <- world |> 
  select(geometry, name, iso_a3) |> 
  left_join(data_with_iso, by = c("iso_a3" = "Iso3")) |> 
  filter(Visited == TRUE)

countries_visited

#----- First Plot -----
world |> 
  filter(admin != "Antarctica") |> 
  st_transform(crs = "+proj=robin") |> 
  ggplot() +
  geom_sf(color = "darkgray")+
  geom_sf(data = countries_visited, aes(fill = Visited)) +
  scale_fill_manual(values = "royalblue")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
    #axis.text.x = element_blank(),
    legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "All countries visited in the James Bond movies",
       x = NULL, y = NULL,
       caption = "@bdill")


#----- Plot single movie with labels -----
countries_single_movie <- countries_visited |> filter(Movie == "Octopussy")

world |> 
  filter(admin != "Antarctica") |> 
  st_transform(crs = "+proj=robin") |> 
  ggplot() +
  geom_sf(color = "darkgray")+
  geom_sf(data = countries_single_movie, aes(fill = Visited)) +
  ggrepel::geom_label_repel(
      data = countries_single_movie,
      aes(label = name, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0
    
  ) +
  scale_fill_manual(values = "royalblue")+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        #axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "Countries visited in James Bond: Octopussy",
       x = NULL, y = NULL,
       caption = "@bdill")


#----- Plot single actor facet wrap -----
countries_single_actor <- countries_visited |> filter(Bond == "Daniel Craig")

world |> 
  filter(admin != "Antarctica") |> 
  st_transform(crs = "+proj=merc") |> 
  ggplot() +
  geom_sf(color = "darkgray")+
  geom_sf(data = countries_single_actor, aes(fill = Visited)) +
  # ggrepel::geom_label_repel(
  #   data = countries_single_actor,
  #   aes(label = name, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0
  # ) +
  scale_fill_manual(values = "royalblue")+
  facet_wrap(~ Movie) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "James Bond Tourism",
       subtitle = "Countries visited in Roger Moore's Bond Movies",
       x = NULL, y = NULL,
       caption = "@bdill")


#----- Inflation -----
#https://tradingeconomics.com/country-list/inflation-rate-
infl <- read_tsv("D:/opendata/inflation_by_country_2022-10-17.tsv")
str(infl)

names(countrycode::codelist)
infl_iso <- infl |> 
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c")
    , continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent")
    , inflation = Last
  ) |> 
  select(Country, continent, Reference, Iso3, inflation)

world_infl <- world |> 
  select(geometry, name, iso_a3) |> 
  left_join(infl_iso, by = c("iso_a3" = "Iso3"))

?scale_fill_steps2
world_infl |> 
  filter(name != "Antarctica") |> 
  st_transform(crs = "+proj=robin") |> 
  ggplot() +
  geom_sf(aes(fill = inflation)) +
  #scale_fill_gradient(low = "#99EEBB", high = "#003311") +
  scale_fill_steps2(
    n.breaks = 10,
    low = scales::muted("blue"),
    mid = "white",
    high = scales::muted("red"),
    midpoint = 5,
    guide = "colorsteps"
  ) +  
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        #axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "Worldwide Inflation",
       subtitle = "",
       x = NULL, y = NULL,
       caption = "@bdill")

#----- World Infl Bar chart -----
infl_iso |>
  #top_n(80, desc(inflation)) |> 
  filter(continent %in% c("Europe") | Country %in% c("United States", "Canada", "Mexico"))  |> 
  #arrange(inflation) |> 
  #View()
  ggplot(aes(x = reorder(Country, inflation), y = inflation)) +
  geom_col(aes(fill = continent)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "US vs Europe Inflation"
       , subtitle = "as of Sept  2022",
       x = "Country",
       caption = "@bdill\ndata: https://tradingeconomics.com/country-list/inflation-rate-")


infl_iso |>
  #top_n(80, desc(inflation)) |> 
  filter(continent %in% c("Europe") | Country %in% c("United States", "Canada", "Mexico")) |> View()

#----- Inflation G20 countries -----
infl_g20 <- read_tsv("D:/opendata/inflation_by_country_g20_2022-10-17.tsv")
infl_g20_iso <- infl_g20 |> 
  mutate(Iso3 = countrycode::countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c")
    , continent = countrycode::countrycode(
      sourcevar = Country,
      origin = "country.name",
      destination = "continent")
    , inflation = Last
  ) |> 
  select(Country, continent, Reference, Iso3, inflation)

infl_g20_iso |> 
  filter(!Country %in% c("Argentina", "Turkey")) |> 
  ggplot(aes(x = reorder(Country, inflation), y = inflation)) +
  geom_col(aes(fill = continent)) +
  geom_text(aes(label = round(inflation, 2), hjust = -.1), size = 4) +
  #scale_y_log10() +
  scale_y_continuous(limits = c(0, 17)) +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12)) +
  theme_minimal() +
  labs(title = "Inflation in G20 Countries"
       , subtitle = "As of Sept  2022 (Excluding Argentina, Turkey with 80%+)",
       x = "Country",
       y = "Inflation %",
       caption = "@bdill\ndata: https://tradingeconomics.com/country-list/inflation-rate-")
