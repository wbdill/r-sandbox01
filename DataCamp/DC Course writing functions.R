#--------------------------------------------------------------------------------
# Writing Functions in R - Hadley Wickham
#--------------------------------------------------------------------------------
rm(list=ls())

#---------- Chapter 1 ----------

ratio <- function(x, y) {
  x/y
}
ratio(3,4)

# for loop
df <- data.frame(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
for (i in 1:ncol(df)) {
  print(median(df[[i]]))
}
for (i in seq_along(df)) {
  print(median(df[[i]]))
}

# store output into a variable
output <- vector("double", ncol(df))

for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}
output


#---------- Chapter 2 ----------



#---------- Chapter 3 ----------
# function to return a vector of the median of each col in the dataframe
col_median <- function(df) {
  output <- numeric(ncol(df))
  for (i in seq_along(df)) {            
    output[[i]] <- median(df[[i]])      
  }
  return(output)
}
col_median(cars)

col_mean <- function(df) {
  output <- numeric(ncol(df))
  for (i in seq_along(df)) {            
    output[[i]] <- mean(df[[i]])      
  }
  return(output)
}

col_summary <- function(df, fun) {
  output <- numeric(ncol(df))
  for (i in seq_along(df)) {            
    output[[i]] <- fun(df[[i]])      
  }
  return(output)
}
col_summary(cars, mean)
col_summary(cars, median)
col_summary(cars, sd)
col_summary(cars, IQR)

#-----
library(purrr)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

coefs <- map(models, coef)
map(coefs, "wt")



#---------- Chapter 4 ----------
#purrr
library(purrr)
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)
# safely() 
# map2() iterate over 2 arguments.  map2( list(1,5,10), list(22, 33, 44), rnorm )
# pmap() iterate over many arguments
# invoke_map() iterate over functions   invoke_map(.f, .x)

# walk(.f, .x) call to functions for their side effects rather than their return value.  Ex: to plot, or save to disk
# walk2(.f, .x, .y) walk with 2 parameter lists
# pwalk()

#-----
funs <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")   # Define list of functions

params <- list(                       # Define params
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)

sims <- invoke_map(funs, params, n = 50)

walk(sims, hist)           # Use walk() to make a histogram of each element in sims


#-----
# Define list of functions
funs <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")

# Define params
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)

sims <- invoke_map(funs, params, n = 50)  # Assign the simulated samples to sims
find_breaks <- function(x) {
  rng <- range(x, na.rm = TRUE)
  seq(rng[1], rng[2], length.out = 30)
}
nice_breaks <- map(sims, find_breaks)
walk2(sims, nice_breaks, hist)

#----- prettify the labels using pwalk()
funs <- list(Normal = "rnorm", Uniform = "runif", Exp = "rexp")
params <- list(
  Normal = list(mean = 10),
  Uniform = list(min = 0, max = 5),
  Exp = list(rate = 5)
)
sims <- invoke_map(funs, params, n = 1000)
nice_breaks <- map(sims, find_breaks)
nice_titles <- c("Normal(10, 1)", "Uniform(0, 5)", "Exp(5)")
pwalk(list(x = sims, breaks = nice_breaks, main = nice_titles), hist, xlab = "")

#---------- Chapter 5: Robust functions ----------

if(condition) {
  stop("error message", call. = FALSE)
}
# avoid type-inconsistent functions in your own functions
# avoid non-standard evaluation functions in your own functions
# Never rely on global options for computational details

options()
getOption("digits")
?options