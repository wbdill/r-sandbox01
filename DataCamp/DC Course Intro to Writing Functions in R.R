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

#----- Ch 3 -----
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
?
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
#----- Ch 4 -----
