# Intro to Writing Functions in R
# https://campus.datacamp.com/courses/introduction-to-writing-functions-in-r/how-to-write-a-function?ex=2
install.packages("assertive")
install.packages("zeallot")
library(assertive)
library(tidyverse)
library(broom)
library(zeallot)

snake_river_visits <- readRDS("data/snake_river_visits.rds")
std_and_poor500 <- readRDS("data/std_and_poor500_with_pe_2019-06-21.rds")
 
#-----

# Update the function to return n coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, n_flips, replace = TRUE)
}

toss_coin(10)   # Generate 10 coin tosses

#-----

# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head = 0.5) {
  coin_sides <- c("head", "tail")
  weights <- c(p_head, 1-p_head)  # Define a vector of weights
  sample(coin_sides, n_flips, replace = TRUE, weights)  # Modify the sampling to be weighted
}

toss_coin(10, .4)  # Generate 10 coin tosses

#-----
install.packages("tidyverse")
library(tidyverse)
snake_river_visits <- readRDS("data/snake_river_visits.rds")

# create wrapper for glm with data first to play nice with pipes
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_visits %>%
  mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  arrange(desc(predicted_n_visits))

#-----
# Set the categories for interval_type to "(lo, hi]" and "[lo, hi)"
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels = NULL, 
                            interval_type = c("(lo, hi]", "[lo, hi)")) {
  interval_type <- match.arg(interval_type)
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the interval_type argument from the call
cut_by_quantile(n_visits)

#----- passing args to inner functions -----
std_and_poor500 <- readRDS("data/std_and_poor500_with_pe_2019-06-21.rds")
glimpse(std_and_poor500)

get_reciprocal <- function(x) {  # Write a function to calculate the reciprocal
  1 / x
}

calc_harmonic_mean <- function(x, na.rm = FALSE) {   # Write a function to calculate the harmonic mean
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  group_by(sector) %>% 
  summarise(hmean_pe_ratio = calc_harmonic_mean(pe_ratio, na.rm = TRUE))   # Summarize, calculating harmonic mean of P/E ratio

#Alternate method of passing args to inner functions
# Easier to type, but less intuitive to use b/c it's not explicit.
calc_harmonic_mean <- function(x, ...) {
  x %>%
    get_reciprocal() %>%
    mean(...) %>%
    get_reciprocal()
}
#-----
install.packages("assertive")
library(assertive)

do_foo <- function(x) {
assert_is_numeric(x)
  1/x
}
do_foo(5)
#-----
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)) {    # Check if any values of x are non-positive
    stop("x contains non-positive values, so the harmonic mean makes no sense.")  # Throw an error
  }
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it negative numbers
calc_harmonic_mean(std_and_poor500$pe_ratio - 20)
calc_harmonic_mean(std_and_poor500$pe_ratio, na.rm = TRUE)

#----- Ch 3 - return values and scope -----
is_leap_year <- function(year) {
  # If year is div. by 400 return TRUE
  if(year %% 400 == 0) {
    return(TRUE)
  }
  # If year is div. by 100 return FALSE
  if(year %% 100 == 0) {
    return(FALSE)
  }  
  # If year is div. by 4 return TRUE
  if(year %% 4 == 0) {
    return(TRUE)
  }
  # Otherwise return FALSE
  return(FALSE)
}
is_leap_year(2009)

#-----
library(tidyverse)
plt_dist_vs_speed <- plot(dist ~ speed, data = cars)

# wrapper function with data as first param so it is pipeable
pipeable_plot <- function(data, formula) {
  plot(formula = formula, data = data)  # Call plot() with the formula interface
  invisible(data)   # Invisibly return the input dataset
}

# Draw the scatter plot of dist vs. speed again
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)

# Now the plot object has a value
plt_dist_vs_speed

R.version.string
memory.size()
shell("cls")
sessionInfo()

#----- returning multiple values -----
# packages: zeallot and broom
# Look at the structure of model (it's a mess!)
model <- glm(n_visits ~ gender + income, family = "poisson", data = snake_river_visits)
str(model)

# Use broom tools to get a list of 3 data frames
list(
  model = glance(model),         # Get model-level values
  coefficients = tidy(model),    # Get coefficient-level values
  observations = augment(model)  # Get observation-level values
)
?broom::glance

#-----
groom_model <- function(model) {
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}

# Call groom_model on model, assigning to 3 variables
c(mdl, cff, obs) %<-% groom_model(model)

# See these individual variables
mdl; cff; obs

#----- defining attributes
pipeable_plot <- function(data, formula) {
  plot(formula, data)
  attr(data, "formula") <- formula  # Add a "formula" attribute to data
  invisible(data)
}

# From previous exercise
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)

str(plt_dist_vs_speed)   # Examine the structure of the result
#-----
# Add capitals, national_parks, & population to a named list
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)

# List the structure of each element of rsa_lst
ls.str(rsa_lst)

# Convert the list to an environment
rsa_env <- list2env(rsa_lst)

# List the structure of each variable
ls.str(rsa_env)

# Find the parent environment of rsa_env
parent <- parent.env(rsa_env)

# Print its name
environmentName(parent)

# Compare the contents of the global environment and rsa_env
ls.str(globalenv())
ls.str(rsa_env)

# Does population exist in rsa_env?
exists("population", envir = rsa_env)

# Does population exist in rsa_env, ignoring inheritance?
exists("population", envir = rsa_env, inherits = FALSE)


#----- Ch 4: Case study on grain yields -----
library(magrittr)
rm(list = ls())
barley <- readRDS("data/nass.barley.rds")
wheat <- readRDS("data/nass.wheat.rds")
corn <- readRDS("data/nass.corn.rds")

get_reciprocal <- function(x) {  # Write a function to calculate the reciprocal
  1 / x
}

# Write a function to convert acres to sq. yards
acres_to_sq_yards <- function(acres) {
  acres * 4840
}
# Write a function to convert yards to meters
yards_to_meters <- function(yards) {
  yards * 36 * .0254
}
# Write a function to convert sq. meters to hectares
sq_meters_to_hectares <- function(sq_meters) {
  sq_meters / 10000
}

sq_yards_to_sq_meters <- function(sq_yards) {
  sq_yards %>%
    sqrt() %>%            # Take the square root
    yards_to_meters() %>% # Convert yards to meters
    raise_to_power(2)     # Square it
}
# Write a function to convert acres to hectares
acres_to_hectares <- function(acres) {
  acres %>%
    acres_to_sq_yards() %>%      # Convert acres to sq yards
    sq_yards_to_sq_meters() %>%  # Convert sq yards to sq meters
    sq_meters_to_hectares()      # Convert sq meters to hectares
}
# Define a harmonic acres to hectares function
harmonic_acres_to_hectares <- function(acres) {
  acres %>% 
    get_reciprocal() %>%   # Get the reciprocal
    acres_to_hectares() %>%       # Convert acres to hectares
    get_reciprocal()
}
get_reciprocal(5)
# Write a function to convert lb to kg
lbs_to_kgs <- function(lbs) {
  lbs * .45359237
}
# Write a function to convert bushels to lbs
bushels_to_lbs <- function(bushels, crop) {
  c(barley = 48, corn = 56, wheat = 60) %>%  # Define a lookup table of scale factors
    extract(crop) %>%      # Extract the value for the crop
    multiply_by(bushels)    # Multiply by the no. of bushels
}
# Write a function to convert bushels to kg
bushels_to_kgs <- function(bushels, crop) {
  bushels %>%
    bushels_to_lbs(crop) %>%      # Convert bushels to lbs for this crop
    lbs_to_kgs()  # Convert lbs to kgs
}
# Write a function to convert bushels/acre to kg/ha
bushels_per_acre_to_kgs_per_hectare <- function(bushels_per_acre, crop = c("barley", "corn", "wheat")) {
  crop <- match.arg(crop)    # Match the crop argument
  bushels_per_acre %>%
    bushels_to_kgs(crop) %>%  # Convert bushels to kgs for this crop
    harmonic_acres_to_hectares()   # Convert harmonic acres to ha
}
# View the corn dataset
glimpse(corn)

corn %>%
  # Add some columns
  mutate(
    farmed_area_ha = acres_to_hectares(farmed_area_acres),  # Convert farmed area from acres to ha
    yield_kg_per_ha = bushels_per_acre_to_kgs_per_hectare(      # Convert yield from bushels/acre to kg/ha
      yield_bushels_per_acre,
      crop = "corn"
    )
  )
# Wrap this code into a function
fortify_with_metric_units <- function(data, crop) {
  data %>%
    mutate(
      farmed_area_ha = acres_to_hectares(farmed_area_acres),
      yield_kg_per_ha = bushels_per_acre_to_kgs_per_hectare(
        yield_bushels_per_acre, 
        crop = crop
      )
    )
}

# Try it on the wheat dataset
wheat <- fortify_with_metric_units(wheat, "wheat")
corn <- fortify_with_metric_units(corn, "corn")
barley <- fortify_with_metric_units(barley, "barley")


ggplot(corn, aes(year, yield_kg_per_ha)) +
  geom_line(aes(group = state)) +    # Add a line layer, grouped by state
  geom_smooth() # Add a smooth trend layer

# Wrap this plotting code into a function
plot_yield_vs_year <- function(data) {
  ggplot(data, aes(year, yield_kg_per_ha)) +
    geom_line(aes(group = state)) +
    geom_smooth()
}

# Test it on the wheat dataset
plot_yield_vs_year(wheat)

usa_census_regions <- read_csv("data/state_populations_2019.csv")
usa_census_regions <- usa_census_regions %>%
  select(state, census_region = census_division)

# Inner join the corn dataset to usa_census_regions by state
corn %>%
  inner_join(usa_census_regions, by = "state")

# Wrap this code into a function
fortify_with_census_region <- function(data) {
  data %>%
    inner_join(usa_census_regions, by = "state")
}

# Try it on the wheat dataset
fortify_with_census_region(wheat) +
  facet_wrap(vars(census_region)) # Facet, wrapped by census region

# add census_region to data sets (for some reason)
corn <- fortify_with_census_region(corn)
wheat <- fortify_with_census_region(wheat)
barley <- fortify_with_census_region(barley)

plot_yield_vs_year(corn) +
 facet_wrap(vars(census_region)) # Facet, wrapped by census region

# Wrap this code into a function
plot_yield_vs_year_by_region <- function(data) {
  plot_yield_vs_year(data) +
    facet_wrap(vars(census_region))
}

# Try it on the wheat dataset
plot_yield_vs_year_by_region(wheat)
plot_yield_vs_year_by_region(corn)

#----- modeling -----
# Generalized additive model
library(mgcv)
lm(response_var ~ exp_var1 + exp_var2, data = data)   # basic lm for comparison
gam(response_var ~ s(exp_var1) + exp_var2, data = data)  # s is a smoothing function

predict_this <- data.frame(
  exp_var1 = c("some", "values"),
  exp_var2 = c("more", "values")
)
predicted_responses <- predict(model, predict_this, type = "response")

predict_this %>%
  mutate(predicted_responses = predicted_responses)  # put them into the original data frame for easy graphing
