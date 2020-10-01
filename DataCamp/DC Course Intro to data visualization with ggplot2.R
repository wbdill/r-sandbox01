# https://learn.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2
# 2020-08-03
rm(list = ls())
library(tidyverse)
#library(ggplot2)

#----- Ch1: Intro -----
# Change the color aesthetic to a size aesthetic
ggplot(mtcars, aes(wt, mpg, size = disp)) +
  geom_point()

ggplot(diamonds, aes(carat, price)) +
  geom_point(alpha = 0.1) +
  geom_smooth()

# using variables
plt_price_vs_carat <- ggplot(diamonds, aes(carat, price))
# Edit this to map color to clarity, Assign the updated plot to a new object
plt_price_vs_carat_by_clarity <- plt_price_vs_carat + geom_point(aes(color = clarity), alpha = 0.1)
plt_price_vs_carat_by_clarity   # See the plot

#----- Ch2: Aesthetics -----
# Aesthetics: x, y, fill, color (points outlines of bars), size, alpha, linetype, labels, shape

mtcars <- mtcars %>% 
  mutate(fcyl = as.factor(cyl),
         fam = case_when(am == 1 ~ "automatic",
                                  am == 0 ~ "manual",
                                  TRUE ~ ""))

# Map x to wt, y to mpg and color to fcyl
ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  geom_point(size = 2)
  #geom_point(shape = 1, size = 4)

ggplot(mtcars, aes(wt, mpg, fill = fcyl, color = fam)) +
  geom_point(shape = 21, size = 4, alpha = .6)


plt_mpg_vs_wt <- ggplot(mtcars, aes(wt, mpg))  # Base layer
plt_mpg_vs_wt +
  geom_text(aes(label = fcyl))  # Use text layer and map fcyl to label

my_blue <- "#4ABEFF"
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +  # Change the color mapping to a fill mapping
  geom_point(color = my_blue, size = 5, shape = 1)  # Set point size and shape

ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  geom_text(label = rownames(mtcars), color = "red")  # Add text layer with label rownames(mtcars) and color red

ggplot(mtcars, aes(mpg, qsec, color = fcyl, shape = fam, size = hp/wt)) +
  geom_point()

# Position adjustments:
# identity, dodge, stack, fill, jitter, jitterdodge, nudge

# Scale functions:  * can = continuous, discrete
# scale_x_*()
# scale_y_*()
# scale_color_*()
# scale_fill_*()
# scale_shape_*()
# scale_linetype_*()
# scale_size_*()

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length",
                     limits = c(2,8),
                     breaks = seq(2,8,3),
                     expand = c(0,0)
                     ) +
  scale_color_discrete("Species", labels = c("Setosa", "Versicolor", "Virginica"))


# Modifying aesthetics video has an error.  The labels = c("Setosa", "Versicolor", "Virginica") should be in the scale_color_discrete() function call rather than the scale_x_continuous()
# https://campus.datacamp.com/courses/introduction-to-data-visualization-with-ggplot2/aesthetics?ex=10

palette <- c(automatic = "#377EB8", manual = "#E41A1C")
ggplot(mtcars, aes(fcyl, fill = fam)) +
  #geom_bar() +
  geom_bar(position = "dodge") +
  labs(x = "Number of Cylinders", y = "Count") +
  scale_fill_manual("Transmission", values = palette) # Set the fill color scale
  

#----- Ch3: Geometries  -----
# geomitries: geom_*
# abline, area, bar, bin2d, blank, boxplot, col, countour, count, crossbar, curve, density, density2d
# dotplot, errorbar, freqpoly, hex, histogram, hline, jitter, label, line, map, path, point
# polygon, qq, qq_line, quantile, raster, rect, ribbon, rug, segment, sf, smooth, smoke, step, text, tile, violin, vline

plt_price_vs_carat_by_clarity <- ggplot(diamonds, aes(carat, price, color = clarity))
plt_price_vs_carat_by_clarity + geom_point(alpha = 0.5, shape = ".")  # Add a point layer with tiny points

# Plot base
plt_mpg_vs_fcyl_by_fam <- ggplot(mtcars, aes(fcyl, mpg, color = fam))
plt_mpg_vs_fcyl_by_fam + geom_point()  # Default points are shown for comparison
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.3)) # Now jitter and dodge the point positions

# Plot type Possible Geoms
# scatter   points, jitter, abline, smooth, count
# bar       histogram, bar, col, errorbar
# line      line, path

ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 1)  # Set the binwidth to 1

ggplot(mtcars, aes(mpg, fill = fam)) +
  #geom_histogram(binwidth = 1, position = "dodge")  # Change the position to dodge
  #geom_histogram(binwidth = 1, position = "fill")  
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.3)  

ggplot(mtcars, aes(fcyl, fill = fam)) +
  #geom_bar()
  #geom_bar(position = "fill")
  geom_bar(position = "dodge")

ggplot(mtcars, aes(cyl, fill = fam)) +
  geom_bar(position = position_dodge(width = 0.2), alpha = 0.6)


ggplot(economics, aes(date, unemploy)) +
  geom_line()

load("data/fish.RData")



#----- Ch4: Themes -----

# Themes control all the non-data ink.
# 3 types element_*()  text, line, rect(angle)

# remove legend: + theme(legend.position = "none")
# legend inside graph: theme(legend.position = c(0.6, 0.1))
theme(
  rect = element_rect(fill = "grey92"),   # For all rectangles, set the fill color to grey92
  legend.key = element_rect(color = NA),   # For the legend key, turn off the outline
  axis.ticks = element_blank(),    # Turn off axis ticks
  panel.grid = element_blank(),     # Turn off the panel grid
  panel.grid.major.y = element_line(
    color = "white",
    size = 0.5,
    linetype = "dotted"
  ),
  axis.text = element_text(color = "grey25"),   # Set the axis text color to grey25
  plot.title = element_text(size = 16, face = "italic")    # Set the plot title font face to italic and font size to 16
)

theme(
  axis.ticks.length = unit(2, "lines"),  # Set the axis tick length to 2 lines
  legend.key.size = unit(3, "cm"), 
  legend.margin = margin(20, 30, 40, 50, "pt"),
  plot.margin = margin(10,30, 50, 70, "mm")
)


install.packages("ggthemes")
library(ggthemes)
theme_bw()
theme_classic()
theme_tufte()
theme_fivethirtyeight()
theme_wsj()

# combining themes and setting as default theme
# Theme layer saved as an object, theme_recession
theme_recession <- theme(
  rect = element_rect(fill = "grey92"),
  legend.key = element_rect(color = NA),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(color = "white", size = 0.5, linetype = "dotted"),
  axis.text = element_text(color = "grey25"),
  plot.title = element_text(face = "italic", size = 16),
  legend.position = c(0.6, 0.1)
)
theme_tufte_recession <- theme_tufte() + theme_recession   # Combine the Tufte theme with theme_recession
theme_set(theme_tufte_recession)  # Set theme_tufte_recession as the default theme


plt_prop_unemployed_over_time +
  theme_tufte() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(color = "grey60"),  # Set the axis title's text color to grey60
    axis.text = element_text(color = "grey60"),   # Set the axis text's text color to grey60
    panel.grid.major.y = element_line(               # Set the panel gridlines major y values
      color = "grey60",    # Set the color to grey60
      size = 0.25,         # Set the size to 0.25
      linetype = "dotted"  # Set the linetype to dotted
      )
  )

#----- Pretty graph exercise -----
install.packages("gapminder")
library(gapminder)

# establish datasets
gm2007_full <- filter(gapminder, year == "2007")
gm2007 <- gm2007_full %>% 
  top_n(10, lifeExp) %>% 
  rbind(gm2007_full %>% 
          top_n(10, desc(lifeExp))
          )

global_mean <- mean(gm2007_full$lifeExp)
x_start <- global_mean + 4
y_start <- 5.5
x_end <- global_mean
y_end <- 7.5

plt_country_vs_lifeExp <- ggplot(gm2007, aes(x = lifeExp, y = reorder(country, lifeExp), color = lifeExp)) +
  geom_point(size = 10) +
  geom_segment(aes(xend = 0, yend = country), size = 2) +
  geom_text(aes(label = lifeExp), color = "white", size = 2.0) +
  scale_x_continuous("", expand = c(0,0), limits = c(0, 90), position = "top") +
  scale_color_gradientn(colors = palette) +
  labs(
    title = "Highest and lowest life expectancies, 2007",
    caption = "Source: gapminder"
)

plt_country_vs_lifeExp +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept = global_mean, color = "grey40", linetype = 3) +
  annotate(
    "text",
    x = x_start, y = y_start,
    label = "The\nglobal\naverage",
    vjust = 1, size = 3, color = "grey40"
  ) +
  annotate(
    "curve",
    x = x_start, y = y_start,
    xend = x_end, yend = y_end,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )

