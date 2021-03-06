# setup ------

library(pacman)
p_load(dplyr, lfe, readr)
setwd("/Users/matthewtarduno/Desktop/212/Section04")

# Saving plots ------

# (Probably useless because this is not the syntax for ggplot)

# Start the .jpeg driver
jpeg("your_plot.jpeg")
# Make the plot
plot(x = 1:10, y = 1:10)
# Turn off the driver
dev.off()

# Start the .pdf driver
pdf("your_plot.pdf")
# Make the plot
plot(x = 1:10, y = 1:10)
# Turn off the driver
dev.off()

# Overfull \hbox ------

# I guess break up lines of code 

library(magrittr)
x <- rnorm(100) %>% matrix(ncol = 4) %>% tbl_df() %>% mutate(V5 = V1 * V2)

# watch those pipes ! 

x <- rnorm(100) %>% matrix(ncol = 4) %>%
  tbl_df() %>% mutate(V5 = V1 * V2)

# Logical operators ------

# I'm not lying
T == TRUE
identical(T, TRUE)


# A nuance
TRUE == "TRUE"
identical(TRUE, "TRUE")

# Greater/less than
1 > 3
1 < 1
1 >= 3
1 <= 1

# Alphabetization
"Ed" < "Everyone" # :(
"A" < "B"

# NA is weird
NA == NA
NA > 3
NA == T
is.na(NA)

# Equals
T == F
(pi > 1) == T

# And
(3 > 2) & (2 > 3)

# Or
(3 > 2) | (2 > 3)

# Not (gives the opposite)
! T








# using logical operators in function options ------

b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Create the y matrix
  y <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y_var) %>%
    # Convert y_data to matrices
    as.matrix()
  
  # Create the X matrix
  X <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X_vars)
  
  # If 'intercept' is TRUE, then add a column of ones
  # and move the column of ones to the front of the matrix
  if (intercept == T) {
    # Bind on a column of ones
    X <- cbind(1, X)
    # Name the column of ones
    names(X) <- c("ones", X_vars)
  }
  
  # Convert X_data to a matrix
  X <- as.matrix(X)
  
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # If 'intercept' is TRUE:
  # change the name of 'ones' to 'intercept'
  if (intercept == T) rownames(beta_hat) <- c("intercept", X_vars)
  
  # Return beta_hat
  return(beta_hat)
}

# Load the 'dplyr' package
library(dplyr)

# Set the seed (make reproducable!)
set.seed(12345)
# Set the sample size
n <- 100
# Generate the x and error data from N(0,1)
the_data <- tibble(
  x = rnorm(n),
  e = rnorm(n))
# Calculate y = 3 + 1.5 x + e
the_data <- mutate(the_data, y = 3 + 1.5 * x + e)
# Plot to make sure things are going well.
plot(
  # The variables for the plot
  x = the_data$x, y = the_data$y,
  # Labels and title
  xlab = "x", ylab = "y", main = "Our generated data")


# Run b_ols with and intercept
b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = T)

# And without 
b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = F)

# testing 

# With an intercept:
felm(y ~ x, data = the_data) %>% summary()

# Without an intercept:
felm(y ~ x - 1, data = the_data) %>% summary()

# Comparing estimations graphically ------

# The estimates
b_w <- b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = T)
b_wo <- b_ols(data = the_data, y_var = "y", X_vars = "x", intercept = F)
# Plot the points
plot(
  # The variables for the plot
  x = the_data$x, y = the_data$y,
  # Labels and title
  xlab = "x", ylab = "y", main = "Our generated data")
# Plot the line from the 'with intercept' regression in yellow
abline(a = b_w[1], b = b_w[2], col = "lightblue", lwd = 3)
# Plot the line from the 'without intercept' regression in purple
abline(a = 0, b = b_w[1], col = "purple4", lwd = 2)
# Add a legend
legend(x = min(the_data$x), y = max(the_data$y),
       legend = c("with intercept", "w/o intercept"),
       # Line widths
       lwd = c(3, 2),
       # Colors
       col = c("lightblue", "purple4"),
       # No box around the legend
       bty = "n")


#FWL Theorem 
# Setup ------
# Options
options(stringsAsFactors = F)
# Load the packages
library(pacman)
p_load(dplyr, lfe, readr, MASS)
# Set the working directory
dir_data <- "/Users/matthewtarduno/Desktop/212/Section04/"
# Load the dataset from CSV
cars <- paste0(dir_data, "auto.csv") %>% read_csv()

# A function to turn dataframes into matricies ------

to_matrix <- function(the_df, vars) {
  # Create a matrix from variables in var
  new_mat <- the_df %>%
    # Select the columns given in 'vars'
    select_(.dots = vars) %>%
    # Convert to matrix
    as.matrix()
  # Return 'new_mat'
  return(new_mat)
}

# This helps us clean up our OLS function 

b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # If 'intercept' is TRUE, then add a column of ones
  if (intercept == T) {
    # Bind a column of ones to X
    X <- cbind(1, X)
    # Name the new column "intercept"
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}

# a test 
b_ols(the_data, y_var = "y", X_vars = "x", intercept = T)
b_ols(the_data, y_var = "y", X_vars = "x", intercept = F)

# a residual function 
# (why not just save residuals as global vars in our ols function)

resid_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  # Require the 'dplyr' package
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # If 'intercept' is TRUE, then add a column of ones
  if (intercept == T) {
    # Bind a column of ones to X
    X <- cbind(1, X)
    # Name the new column "intercept"
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate the sample size, n
  n <- nrow(X)
  # Calculate the residuals
  resids <- (diag(n) - X %*% solve(t(X) %*% X) %*% t(X)) %*% y
  # Return 'resids'
  return(resids)
}



# Steps 1 and 2: Residualize 'price' on 'weight' and an intercept
e_yx <- resid_ols(data = cars, y_var = "price",
                  X_vars = "weight", intercept = T)
# Steps 3 and 4: Residualize 'mpg' on 'weight' and an intercept
e_xx <- resid_ols(data = cars, y_var = "mpg",
                  X_vars = "weight", intercept = T)
# Combine the two sets of residuals into a data.frame
e_df <- data.frame(e_yx = e_yx[,1], e_xx = e_xx[,1])
# Step 5: Regress e_yx on e_xx without an intercept
b_ols(data = e_df, y_var = "e_yx",
      X_vars = "e_xx", intercept = F)


# does it work? 
b_ols(data = cars, y_var = "price", X_vars = c("mpg", "weight"))
felm(price ~ mpg + weight, data = cars) %>% summary()


# Omitted variable bias ------

# Don't need to worry about 1) vars that aren't related to y or 2) vars aren't related to x1 

# Set the seed
set.seed(12345)
# Set the sample size
n <- 1e5
# Generate x1, x2, and error from ind. N(0,1)
the_data <- tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  e = rnorm(n))
# Calculate y = 1.5 x1 + 3 x2 + e
the_data <- mutate(the_data, y = 1.5 * x1 + 3 * x2 + e)

# Regression omitting 'x2'
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = F)

# Regression including 'x2'
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = F)

#est. doesn't change much, because X1 and X2 are (nearly) orthagonal 

# correlated data ------

# Load the MASS package
library(MASS)
# Create a var-covar matrix
v_cov <- matrix(data = c(1, 0.95, 0.95, 1), nrow = 2)
# Create the means vector
means <- c(5, 10)
# Define our sample size
n <- 1e5
# Set the seed
set.seed(12345)
# Generate x1 and x2
X <- mvrnorm(n = n, mu = means, Sigma = v_cov, empirical = T)
# Create a tibble for our data, add generate error from N(0,1)
the_data <- tbl_df(X) %>% mutate(e = rnorm(n))
# Set the names
names(the_data) <- c("x1", "x2", "e")
# The data-generating process
the_data <- the_data %>% mutate(y = 1 + 2 * x1 + 3 * x2 + e)


# Regression 1: y on int, x1, and x2
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = T)

# Regression 2: y on int and x1
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = T)
# yikes! 

# Regression 3: y on int and x2
b_ols(the_data, y_var = "y", X_vars = "x2", intercept = T)
# double yikes! 


# what happened? 
#   " First, the regression that matches the true data-generating process 
#   produces coefficient estimates that are pretty much spot on 
#   (we have a fairly large sample). " 
#
#   Second, omitting one of the xx variables really 
#   messes up the coefficient estimate on the other variable.


# bad controls ------

# what happens when we add variables that aren't part of the DGP? 

#let's drop x2 from the DPG: 

# Create a var-covar matrix
v_cov <- matrix(data = c(1, 1 - 1e-6, 1 - 1e-6, 1), nrow = 2)
# Create the means vector
means <- c(5, 5)
# Define our sample size
n <- 1e5
# Set the seed
set.seed(12345)
# Generate x1 and x2
X <- mvrnorm(n = n, mu = means, Sigma = v_cov, empirical = T)
# Create a tibble for our data, add generate error from N(0,1)
the_data <- tbl_df(X) %>% mutate(e = rnorm(n))
# Set the names
names(the_data) <- c("x1", "x2", "e")
# The data-generating process
the_data <- the_data %>% mutate(y = 1 + 2 * x1 + e)


#regressions:  

# Regression 1: y on int and x1 (DGP)
b_ols(the_data, y_var = "y", X_vars = "x1", intercept = T)
#yay! 

# Regression 2: y on int, x1, and x2
b_ols(the_data, y_var = "y", X_vars = c("x1", "x2"), intercept = T)
# yikes 

# Regression 3: y on int and x2
b_ols(the_data, y_var = "y", X_vars = "x2", intercept = T)

# what happened: 
#   "because x1x1 and x2x2 are so strongly correlated,
#   when we “control for” x2x2, we remove meaningful 
#   variation—misattributing some of x1x1’s variation to x2"



