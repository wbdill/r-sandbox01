# DC Course Communicating with data in the Tidyverse
# author's github: https://srfdata.github.io/
rm(list = ls())
install.packages('showtext', dependencies = TRUE)
library(tidyverse)
library(showtext)


load("C:/GitHub/r-sandbox01/DataCamp/data/ilo_working_hours.RData")
load("C:/GitHub/r-sandbox01/DataCamp/data/ilo_hourly_compensation.RData")

glimpse(ilo_working_hours)
glimpse(ilo_hourly_compensation)

#----- Chapter 1 -----
ilo_data <- ilo_hourly_compensation %>%
  inner_join(ilo_working_hours, by = c("country", "year"))

ilo_data %>%
  count()


# Turn year and country into a factor
ilo_data_corrected <- ilo_data %>%
  mutate(year = as.factor(as.numeric(year)),
         country = as.factor(country))

# Only retain European countries
european_countries = c("Finland", "France", "Italy", "Norway", "Spain",
                       "Sweden", "Switzerland", "United Kingdom", "Belgium",
                       "Ireland", "Luxembourg", "Portugal", "Netherlands",
                       "Germany", "Hungary", "Austria", "Czech Rep.") 
ilo_data <- ilo_data_corrected %>%
  filter(country %in% european_countries)

ilo_data %>%
  group_by(year) %>%
  summarize(mean_hourly_compensation = mean(hourly_compensation),
            mean_working_hours = mean(working_hours))  

# Filter for 2006
plot_data <- ilo_data %>%
  filter(year == 2006)

# Create the scatter plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation))

# use labs to make it prettier
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )

?theme

# element_* function family:
# element_text()
# element_rect()
# element_line()
# element_blank()

# Save your current plot into a variable: ilo_plot
ilo_plot <- ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  )


#https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
ilo_plot +
  theme_minimal() +
  # Customize the "minimal" theme with another custom "theme" call
  theme(
    text = element_text(family = "serif"),
    title = element_text(color = "gray25"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units="mm")
  
    )


#----- Chapter 2 -----
ilo_data <- ilo_data %>%
  filter(year %in% c(1996, 2006))

ilo_plot <- ggplot(ilo_data, aes(x = working_hours, y = hourly_compensation)) +
  geom_point() +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017"
  ) +
  # Add facets here
  facet_grid(.~year)

ilo_plot

theme_ilo <- function() {
    theme_minimal() +
    theme(
      text = element_text(family = "Bookman", color = "gray25"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}

# apply theme to plot "permanently"
ilo_plot <- ilo_plot +
  theme_ilo()

ilo_plot +
  theme(
    strip.background = element_rect(fill = "gray60", color = "gray95"),
    strip.text = element_text(color = "white")
  )

#dot plot with geom_path
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country))
            
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type="closed"))
#?arrow

# add text labels (but not placed well)
ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type="closed")) +
  geom_text(aes(x = working_hours, y = country, label = round(working_hours,1)))

library(forcats)
ilo_data <- ilo_data %>%
  # Arrange data frame
  arrange(year) %>%
  # Reorder countries by working hours in 2006
  mutate(country = fct_reorder(country, working_hours, last))
#                                 1          2           3
# arrange 1 by using the summary function 3 on variable 2

ilo_dot_plot <- ggplot(ilo_data) +
  geom_path(aes(x = working_hours, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type="closed")) +
  geom_text(aes(x = working_hours, y = country, 
                label = round(working_hours,1),
                hjust = ifelse(year == "2006", 1.4, -0.4) ),
            label.size = 3,
            family = "Bookman",
            color = "gray25")

# Reuse ilo_dot_plot
ilo_dot_plot <- ilo_dot_plot +
  # Add labels to the plot
  labs(
    x = "Working hours per week",
    y = "Country",
    title = "People work less in 2006 compared to 1996",
    subtitle = "Working hours in European countries, development since 1996",
    caption = "Data source: ILO, 2017"
  ) +
  # Apply your theme
  theme_ilo() +
  # Change the viewport
  coord_cartesian(xlim = c(25, 41))

# View the plot
ilo_dot_plot
#----- Chapter 3 R Markdown -----


#----- Chapter 4 -----


