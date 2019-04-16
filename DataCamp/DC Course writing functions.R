#--------------------------------------------------------------------------------
# Writing Functions in R - Hadley Wickham
#--------------------------------------------------------------------------------


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

#---------- Chapter 4 ----------

#---------- Chapter 5 ----------