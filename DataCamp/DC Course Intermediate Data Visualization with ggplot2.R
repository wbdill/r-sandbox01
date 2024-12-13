# DC Course: intermediate data visualization with ggplot2
# https://campus.datacamp.com/courses/intermediate-data-visualization-with-ggplot2
# 2023-08-23

#----- Ch 1 -----
diamonds %>% filter(cut %in% c("Premium", "Good"), carat < 2) %>% 
ggplot( aes(x = carat, y = price, color=cut)) +
  geom_jitter(alpha = 0.05, ) +
  stat_quantile(quantiles = c(0.05, 0.5, 0.95))

# Amend the stat to use proportion sizes
ggplot(Vocab, aes(x = education, y = vocabulary)) +
  stat_sum(aes(size = ..prop..))

# If a few data points overlap, jittering is great. 
#When you have lots of overlaps (particularly where continuous data has been rounded), using stat_sum() to count the overlaps is more useful. 

mean_sdl(iris$Sepal.Length)

ggplot(iris, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1))

ggplot(iris, aes(Species, Sepal.Length)) +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.1) +
  stat_summary(fun = mean, geom = "point")

#install.packages("MASS")
library(MASS)
# histogram with a normal overlay for comparison
mam.new <- data.frame(body = log10(mammals$body))
ggplot(mam.new, aes(x=body)) +
  geom_histogram(aes(y = ..density..)) +
  geom_rug() +
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(mam.new$body),
                            sd = sd(mam.new$body)))

# QQ plot to verify normal distribution
ggplot(mam.new, aes(sample = body )) +
  stat_qq() +
  geom_qq_line(col = "red")

#---
# Define position objects
# 1. Jitter with width 0.2
posn_j <- position_jitter(width = 0.2)

# 2. Dodge with width 0.1
posn_d <- position_dodge(width = 0.1)

# 3. Jitter-dodge with jitter.width 0.2 and dodge.width 0.1
posn_jd <- position_jitterdodge(jitter.width = 0.2, dodge.width = 0.1)

mtcars$fcyl <- as.factor(mtcars$cyl)
mtcars$fam <- as.factor(mtcars$am)
p_wt_vs_fcyl_by_fam <- ggplot(mtcars, aes(fcyl, wt, color = fam))

p_wt_vs_fcyl_by_fam + geom_point()
p_wt_vs_fcyl_by_fam + geom_point(position = posn_j)
p_wt_vs_fcyl_by_fam + geom_point(position = posn_d)
p_wt_vs_fcyl_by_fam + geom_point(position = posn_jd)

p_wt_vs_fcyl_by_fam_jit <- p_wt_vs_fcyl_by_fam + geom_point(position = posn_j)
p_wt_vs_fcyl_by_fam_jit +
  stat_summary(position = posn_d,
               fun.data = mean_sdl,
               fun.args = list(mult=1),
               geom = "errorbar")

p_wt_vs_fcyl_by_fam_jit +
    stat_summary(fun.data = mean_cl_normal,
               position = posn_d)
?mean_sdl

#----- Ch 2 -----
mtcars$fcyl <- as.factor(mtcars$cyl)
mtcars$fam <- as.factor(mtcars$am)

# scale functions change the underlying dataset, which affects calculations made by computed geoms (like histograms or smooth trend lines), 
# whereas coordinate functions make no changes to the dataset.
ggplot(mtcars, aes(x = wt, y = hp, color = fam)) +
  geom_point() +
  geom_smooth() +
  #scale_x_continuous(limits = c(3,6))
  coord_cartesian(xlim = c(3,6))

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed(ratio = 1)  # Fix the coordinate ratio

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(size = 2) +
  coord_cartesian(expand = 0, clip = "off") + # Add Cartesian coordinates with zero expansion
  #theme_classic()  +
  theme(axis.line = element_blank())

data(msleep)
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  ggtitle("Raw Values")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Scale_ functions")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10") +
  ggtitle("coord_trans")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Scale_ functions")

ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_trans(x = "log10", y = "log10") +  # coord_trans jacks up geom_smooth b/c smooth is calc'd after scale Xform
  ggtitle("coord_trans")

data("airquality")
airquality$Date <- lubridate::mdy((paste0(airquality$Month, "/", airquality$Day, "/1973")))
ggplot(airquality, aes(Date, Temp)) +
  geom_line()

#---
# Define breaks (Fahrenheit)
y_breaks <- c(59, 68, 77, 86, 95, 104)
y_labels <- (y_breaks - 32) * 5 / 9  # Convert y_breaks from Fahrenheit to Celsius

# Create a secondary x-axis
secondary_y_axis <- sec_axis(
  trans = identity, # Use identity transformation
  name = "Celsius",
  breaks = y_breaks,
  labels = y_labels
)
secondary_y_axis # Examine the object

ggplot(airquality, aes(Date, Temp)) +
  geom_line() +
  scale_y_continuous(sec.axis = secondary_y_axis) +
  labs(x = "Date (1973)", y = "Fahrenheit")

scale_y_continuous(sec.axis = secondary_y_axis) +
  labs(x = "Date (1973)", y = "Fahrenheit")

# Plot fcyl bars, filled by fam
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = position_dodge(width = 0.5)) +
  coord_flip()

# Flip the axes to set car to the y axis
library(tidyverse)
#mtcars %>% remove_rownames  %>% tibble::column_to_rownames(var="names")
mtcars <- mtcars %>% rownames_to_column(var = "car")
ggplot(mtcars, aes(car, wt)) +
  geom_point() +
  labs(x = "car", y = "weight") +
  coord_flip()

ggplot(mtcars, aes(x = 1, fill = fcyl)) +
  geom_bar() +
  # Add a polar coordinate system
  coord_polar(theta = "y")


ggplot(mtcars, aes(x = 1, fill = fcyl)) +
  geom_bar(width = .1) +
  # Add a polar coordinate system
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(.5, 1.5))

ggplot(wind, aes(wd, fill = ws)) +
  geom_bar(width = 1) +
  coord_polar(start = -pi/16)

#===== Ch 3 =====
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(rows = vars(am)) # Facet rows by am

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(cols = vars(cyl))  # Facet columns by cyl

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  facet_grid(rows =  vars(am), cols = vars(cyl))  # Facet rows by am and columns by cyl
