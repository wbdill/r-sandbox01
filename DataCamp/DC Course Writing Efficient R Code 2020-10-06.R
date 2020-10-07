# DC Course Writing Efficient R Code (Colin Gillespie)
# https://campus.datacamp.com/courses/writing-efficient-r-code
# 2020-10-06
library(microbenchmark)
install.packages("benchmarkme")
library(benchmarkme)
install.packages("profvis")
library(profvis)

movies <- readRDS("data/movies.rds")

#----- Ch 1 The Art of Benchmarking -----
# Premature optimization is the root of all evil - Donald Knuth


version  # Print the R version details using version
major <- version$major  # Assign the variable major to the major component
minor <- version$minor  # Assign the variable minor to the minor component

colon <- function(n) 1:n
seq_default <- function(n) seq(1, n)
seq_by <- function(n) seq(1, n, by = 1)
system.time(colon(1e8))
system.time(seq_default(1e8))
system.time(seq_by(1e8))

n <- 1e8
microbenchmark(colon(n),
               seq_default(n),
               seq_by(n),
               times = 10)

system.time(read.csv("data/movies.csv"))  # How long does it take to read movies from CSV?
system.time(readRDS("data/movies.rds"))  # How long does it take to read movies from RDS?

compare <- microbenchmark(read.csv("data/movies.csv"),   # Compare the two functions
                          readRDS("data/movies.rds"), 
                          times = 10)
compare  # Compare the two functions

library(benchmarkme)
res <- benchmark_std(runs = 1)
upload_results(res)
plot(res)

ram <- get_ram()  # Assign the variable ram to the amount of RAM on this machine
ram
cpu <- get_cpu()  # Assign the variable cpu to the cpu specs
cpu
res <- benchmark_io(runs = 1, size = 5)
plot(res)


#----- Ch 2 Fine Tuning: Efficient Base R -----
rm(list = ls())
# first rule of R club: never grow a vector
# second rule of R club: use a vectorized solution wherever possible.

#vectorized code: multiplication
# Initial code
n <- 100
total <- 0
x <- runif(n)
for(i in 1:n) 
  total <- total + log(x[i])

# Rewrite in a single line. Store the result in log_sum
log_sum <- sum(log(x))

# third rule of R club: use a matrix whenever feasible
# returning a row in a df is 100x slower than a row in a matrix

#----- Ch 3 Diagnosing Problems: Code Profiling -----
rm(list= ls())
library(profvis)
Rprof()  # base R function, but tricky to use

install.packages("ggplot2movies")
library(ggplot2movies)
profvis({
  data(movies, package = "ggplot2movies")
  braveheart <- movies[7288,]
  movies <- movies[movies$Action==1, ]
  plot(movies$year, movies$rating, xlab = "Year", ylab = "Rating")
  model <- loess(rating ~ year, data = movies)
  j <- order(movies$year)
  lines(movies$year[j], model$fitted[j], col = "forestgreen", lwd = 2)
  points(braveheart$year, braveheart$rating, pch = 21, bg = "steelblue", cex = 3)
})


data(movies, package = "ggplot2movies")   # Load the data set
library(profvis)  # Load the profvis package
profvis({  # Profile the following code with the profvis function
  comedies <- movies[movies$Comedy == 1, ]  # Load and select data
  plot(comedies$year, comedies$rating)  # Plot data of interest
  model <- loess(rating ~ year, data = comedies) # Loess regression line
  j <- order(comedies$year)
  lines(comedies$year[j], model$fitted[j], col = "red")  # Add fitted line to the plot
})


library(microbenchmark)  # Load the microbenchmark package
d <- function() {  # d() Simulates 6 dices rolls
  data.frame(
    d1 = sample(1:6, 3, replace = TRUE),
    d2 = sample(1:6, 3, replace = TRUE)
  )
}
m <- function() {
  matrix(sample(1:6, 6, replace = TRUE), ncol = 2)  # Complete the matrix solution
}
microbenchmark(   # Use microbenchmark to time m() and d()
  data.frame_solution = d(),
  matrix_solution     = m()
)



rolls <- m()  # Example data
app <- function(x) {  # Define the previous solution 
  apply(x, 1, sum)
}
r_sum <- function(x) {  # Define the new solution
  rowSums(x)
}
microbenchmark(  # Compare the methods
  app_sol = app(rolls),
  r_sum_sol = r_sum(rolls)
)


is_double <- c(FALSE, TRUE, TRUE)  # Example data
move <- function(is_double) {   # Define the previous solution
  if (is_double[1] & is_double[2] & is_double[3]) {
    current <- 11 # Go To Jail
  }
}
improved_move <- function(is_double) {  # Define the improved solution
  if (is_double[1] && is_double[2] && is_double[3]) {
    current <- 11 # Go To Jail
  }
}
# microbenchmark both solutions  # Very occassionally the improved solution is actually a little slower
microbenchmark(move(is_double), improved_move(is_double), times = 1e5)


#----- Ch 4 Turbo Charged Code: Parallel Programming -----
rm(list = ls())
library(parallel)  # part of R since 2011
no_of_cores <- detectCores()
no_of_cores
# if you can write your for loop in reverse, there is a good chance you can run in parallel

m <- matrix(rnorm(10000), ncol = 10)
res <- apply(m, 1, median)

cl <- makeCluster(3) # choose # of cores
parApply(cl, m, 1, median)
stopCluster(cl)
# sometines interprocess communication negates the benefit of parallelism

# apply() -> parApply()
# sapply() -> parSapply()  # applying a function to a vector
# lapply() -> parLapply()  # applying a function to a list


library("parallel")
cl <- makeCluster(2)  # Create a cluster via makeCluster (2 cores)
clusterExport("play")  # Export the play() function to the cluster
res <- parSapply(cl, 1:100, function(i) play())  # Re-write sapply as parSapply
stopCluster(cl)  # Stop the cluster

no_of_games <- 1e5  # Set the number of games to play
system.time(serial <- sapply(1:no_of_games, function(i) play()))  ## Time serial version
cl <- makeCluster(4)  ## Set up cluster
clusterExport(cl, "play")
system.time(par <- parSapply(cl, 1:no_of_games, function(i) play()))  ## Time parallel version
stopCluster(cl)  ## Stop cluster
