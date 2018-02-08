# tarduno section 3 --------------------------------------

setwd("/Users/matthewtarduno/Desktop/212/Section03")


# Review (more on types, vectors) ------------------------------------------
# Does as.numeric() create integers or doubles? (doubles!)
is.double(as.numeric(1))

is.integer(as.numeric(1))

# Are integers and doubles numeric?
is.numeric(as.double(1))

is.numeric(as.integer(1))


# Define the vector
vec <- 1:5
# Square the elements of the vector
vec2 <- vec^2
# Look at the result
vec2
## [1]  1  4  9 16 25

# misc (knitr, NA types) -------------------------------------------

# this saves things in cache until they are changed -- helps speed up the knitting! 
library(knitr)
opts_chunk$set(cache = T)


# NA: 

# Class of NA
class(NA)

# Class of NA from a vector of numbers 
class(c(1, NA)[2])

# Class of NA from a vector of characters (like a chameleon, I guess)
class(c("hi", NA)[2])

# Class of NA from a vector of logicals
class(c(T, NA)[2])

# Demonstrate is.na()
is.na(NA)
is.na(1)
is.na(T)

# What is NaN? (not a number)
0 / 0

# Is NaN NA? It is... wild! 
is.na(NaN)

# Are NaN and NA identical? No! (because different class)
identical(NA, NaN)

# Are they equal?
NA == NaN

# Create the data frame
test_df <- data.frame(
  x = c(NA, "A", "B", NA, "A"),
  y = c(1:4, NA))
# Print test_df to the screen
test_df

# Subset to x == A
dplyr::filter(test_df, x == "A")

# Subset to x != A (Watch out, this is not true complement, because NA's will not be included here)
dplyr::filter(test_df, x != "A")

# x equal to A or NA
dplyr::filter(test_df, x == "A" | is.na(x))

# This is how you read data in as NA: 
# wb_df <- read_csv(
#   file = "world_bank.csv",
#   na = c("", "NA", ".."))

#removing things

item1<-test_df
item2<-test_df
item3<-test_df

# Remove a single object from memory
rm(item1)
# Remove to (or more) objects from memory
rm(list = c("item2", "item3"))
rm(item2, item3)
# Remove everything from memory
rm(list = ls())


#






# functions ---------------------------------------

# Basic syntax 

# foo <- function(arg1, arg2) {
#   ...
#   return(final_stuff)
# }

# Define the function (named 'triple_prod')
triple_prod <- function(x, y, z) {
  # Take the product of the three arguments
  tmp_prod <- x * y * z
  # Return 'tmp_prod'
  return(tmp_prod)
}

# Test the function
triple_prod(x = 2, y = 3, z = 5)
## [1] 30


# Setup ----
# Options
options(stringsAsFactors = F)
# Packages
library(pacman)
p_load(haven, dplyr)
# Define directories
dir_class <- "/Users/matthewtarduno/Desktop/212/"
dir_sec3 <- paste0(dir_class, "Section01/")

# OLS function ----
cars <- read_dta(file = paste0(dir_sec3, "auto.dta"))

# The function require() is the standard way to have a function make sure a package is loaded. 
# select vs. select_ might be a thing of the past 

b_ols <- function(data, y, X, intercept=NULL) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Select y variable data from 'data'
  y_data <- subset(data, select=c(y))

  # Select X variable data from 'data'
  X_data <- select_(data, .dots = X)
  
  
  if(is.null(intercept)) {
    # Convert y_data to matrices
    y_data <- as.matrix(y_data)
    
    # Convert X_data to matrices
    X_data <- as.matrix(X_data)
    
    # Calculate beta hat
    beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
    
    y_hat<-X_data%*%beta_hat #Predicted values
    
    SST<-sum((y_data)^2) #Total sum of squares 
    SST_demean <- sum((y_data - mean(y_hat))^2)
    SSM<-sum((y_hat)^2) #Regression sum of squares
    SSR<-sum((y_data-y_hat)^2) #Error sum of squares
    n <- dim(X_data)[1]
    k <- dim(X_data)[2]
    dof <- n - k 
    R_uc <- 1 - (SSR/SST)
    R <- 1-(SSR/SST_demean) 
    R_adj <- 1 - (1 - R) * ((n-1)/(n-k))
    AIC <- log(SSR/n) + (2*k)/n 
    SIC <- log(SSR/n) + (k/n) * log(n) 
    
    
    # Return beta_hat
    return(c(beta_hat, R_squared, AIC, SIC, R, R_adj))
  }
  
  else{
    # Convert y_data to matrices
    y_data <- as.matrix(y_data)
    
    # Add a column of ones to X_data (could also just make new df var manually)
    X_data <- mutate_(X_data, "ones" = 1)
    
    # Move the intercept column to the front (this is cool)
    X_data <- select_(X_data, "ones", .dots = X)
    
    # Convert X_data to matrices
    X_data <- as.matrix(X_data)
    
    # Calculate beta hat
    beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
    
    # Change the name of 'ones' to 'intercept'
    rownames(beta_hat) <- c("intercept", X)
    
    y_hat<-X_data%*%beta_hat #Predicted values
    
    SST<-sum((y_data)^2) #Total sum of squares 
    SST_demean <- sum((y_data - mean(y_hat))^2)
    SSM<-sum((y_hat)^2) #Regression sum of squares
    SSR<-sum((y_data-y_hat)^2) #Error sum of squares
    n <- dim(X_data)[1]
    k <- dim(X_data)[2]
    dof <- n - k 
    R_uc <- 1 - (SSR/SST)
    R <- 1-(SSR/SST_demean) 
    R_adj <- 1 - (1 - R) * ((n-1)/(n-k))
    AIC <- log(SSR/n) + (2*k)/n 
    SIC <- log(SSR/n) + (k/n) * log(n) 
  
    
    # Return beta_hat
    return(c(beta_hat, R, R_uc, R_adj,  AIC, SIC, R, R_adj))
  }
}

# One little regression 
b_ols(cars, "mpg", c("price"))
lm(cars$mpg ~ cars$price)

#Another one
b_ols(data = cars, y = "price", X = c("mpg", "weight"))

# try to figure out how to put the one's stuff in an if statement. (w/o intercept matricies won't work)
# maybe try just making a new object that is everything but the row of ones from the matrix??
# Ceci n'est pas une pipe ----

# This will allow us to un-nest the nest

# take preceding object, feed it to next function


# Select the variables
tmp_data <- select(cars, price, mpg, weight)
# Summarize the selected variables
summary(tmp_data)

# PIPE
cars %>% select(price, mpg, weight) %>% summary()


#comparing to canned regression ----
p_load(lfe)

# Run the regression with 'felm'
canned_ols <- felm(formula = price ~ mpg + weight, data = cars)
# Summary of the regression
canned_ols %>% summary()
#also contains a bunch of stuff on top of regression estimates 

b_ols(data = cars, y = "price", X = c("mpg", "weight"))
