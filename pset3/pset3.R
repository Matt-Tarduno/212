rm(list=ls())

#  Setup ----------------------------------------------------------------

# Sebastien Annan-Phan, Alejandro Favela, Matthew Tadruno 

# Packages 
library(pacman)
library(ggpubr)
library(formattable)

# p_load examples
p_load(dplyr, haven, readr)
p_load(ggplot2,extrafont,Matrix, reshape)

setwd("/Users/matthewtarduno/Desktop/212/pset3")
dir_data<-"/Users/matthewtarduno/Desktop/212/pset3/"




# To MATRIX ############################################################################
# Function to convert tibble, data.frame, or tbl_df to matrix ----
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

# Simple OLS ----

b_ols <- function(data, y_var, X_vars) {
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}

# OLS + stats----

ols <- function(data, y_var, X_vars, absorb) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Update s2 to numeric
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  t <- b / se
  # Adjusted R-sq
  y_hat <- y - e
  
  SSM_demean <- sum((y_hat - mean(y)) * (y_hat - mean(y)))
  SST_demean <- sum((y - mean(y)) * (y - mean(y)))
  SSR <- sum(e * e)
  R <- 1 - SSR/SST_demean  
  R <- formattable(R, digits = 3, format = "f")
  R_adj <- 1 - ((1 - R) * (n-1)/(n-k-absorb))
  R_adj <- formattable(R_adj, digits = 3, format = "f")
  
  # Nice table (data.frame) of results
  results <- data.frame(
    # The rows have the coef. names
    effect = rownames(b),
    # Estimated coefficients
    coef = formattable(as.vector(b), digits = 3, format = "f"),
    # Standard errors
    std_error = formattable(as.vector(se), digits = 3, format = "f"),
    # T-stat for beta = 0
    tstat0 = formattable(as.vector(t), digits = 3, format = "f")
  )
  
  rsq <- data.frame(
    effect = c("R", "R_adj"),
    coef = c(R,R_adj),
    std_error = as.numeric(c("","")),
    tstat0 = as.numeric(c("",""))
  )
  
  results<- rbind(results, rsq)
  e2<-e^2 #squared residuals
  assign("e2", e2, envir = .GlobalEnv)
  
  
  # Return the results
  return(results)
}

# t stats ----
t_stat <- function(data, y_var, X_vars, gamma) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept if requested
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Force s2 to numeric
  s2 %<>% as.numeric()
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - gamma) / se
  # Return the _t_ statistics
  return(t_stats)
}




# Q.1 "Read the data into R. Plot the series and make sure your data are read in correctly" ----
data <- read_csv(paste0(dir_data, "nls80.csv"))

clist <- colnames(data)

ggplot(data=data, aes(data$wage)) + geom_histogram()


clist <- colnames(data)

#plot data 
plot_list = list()
for (var in clist){
  attach(data)
  xvar = get(var)
  plot<-ggplot(data=data, aes(xvar)) + geom_histogram() +xlab(var)
  print(plot)
}


# Question 2 (a) ---- 

# Estimate the model above via least squares. 

data$int<-1
b_ols(data, "lwage", c("int", "exper", "tenure", "married", "south", "urban", "black", "educ"))
lm(lwage ~ exper + tenure + married + south + urban + black + educ, data=data)


# The (really smart) test devised by Hal goes as follows. One regresses the squared
# residuals on a constant, all variables in X, squares of all variables in X and all cross products. nR˙ 2
# from this regression is distributed as a Chi-squared (p−1), where p is the number of regressors in this equation including
# the constant. The null in this test is homoskedastic disturbances.

#run out fancy ols function 
ols(data, "lwage", c("int", "exper", "tenure", "married", "south", "urban", "black", "educ"), absorb=F)

#save the squared residuals from our fancy ols function 
data$e2<-e2

#white test: 

#squares of (non dummy) variables ----
data$exper2<-data$exper^2
data$educ2<-data$educ^2
data$tenure2<-data$tenure^2

#all cross products ----
data$cp1<-data$exper*data$tenure
data$cp2<-data$exper*data$married
data$cp3<-data$exper*data$south
data$cp4<-data$exper*data$urban
data$cp5<-data$exper*data$black
data$cp6<-data$exper*data$educ
data$cp7<-data$tenure*data$married
data$cp8<-data$tenure*data$south
data$cp9<-data$tenure*data$urban
data$cp10<-data$tenure*data$black
data$cp11<-data$tenure*data$educ
data$cp12<-data$married*data$south
data$cp13<-data$married*data$urban
data$cp14<-data$married*data$black
data$cp15<-data$married*data$educ
data$cp16<-data$south*data$urban
data$cp17<-data$south*data$black
data$cp18<-data$south*data$educ
data$cp19<-data$urban*data$black
data$cp20<-data$urban*data$educ
data$cp21<-data$black*data$educ


l<-colnames(data)
l<-l[grep("^[cp].*", l)]

vars<-c(c("int", "exper", "tenure", "married", "south", "urban", "black", "educ", "exper2", "tenure2", "educ2"), l)

# white test ----

ols(data, "e2", vars, absorb=F)

#32 vars
#R = .284 
#nR= 265.54




