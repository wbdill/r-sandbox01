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



#---------- Chapter 4 ----------
#purrr
library(purrr)
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

#---------- Chapter 5 ----------





