# Part 1
# Setup ----


# Packages 
library(pacman)
library(knitr)
# p_load examples
p_load(dplyr, haven, readr, xtable, psych, magrittr)
p_load(ggplot2, extrafont, Matrix)

# Loading data 
directory<-"/Users/matthewtarduno/Desktop/212/midterm/"
raw <-read_csv(paste0(directory, "data.csv"), col_types=cols())


# Cleaning Data ---- 

# note, I added a period dummy when I put the data into csv form. 

# Using the dummy.code() function of psych (found using R search function)
dummies<-as.data.frame(dummy.code(raw$state))
# Clean dummy names 
names(dummies)<-gsub(' ', '\\.', names(dummies))


# Combine the two dataframes 
data<-as.data.frame(c(raw, dummies))

#remove % sign 
data$emmigrant.ratio<-substr(data$emmigrant.ratio, 1, nchar(as.character(data$emmigrant.ratio))-1)
data$emmigrant.ratio<-as.numeric(data$emmigrant.ratio)
data$emmigrant.ratio<-data$emmigrant.ratio/100


# Functions ----

to_matrix <- function(the_df, vars) {
  new_mat <- the_df %>%
    select_(.dots = vars) %>%
    as.matrix()
  return(new_mat)
}


# Function for OLS coefficient estimates
b_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  require(dplyr)
  # Create the y matrix
  y <- to_matrix(the_df = data, vars = y_var)
  
  # Create the X matrix
  X <- to_matrix(the_df = data, vars = X_vars)
  
  # Intercept
  if (intercept == T) {
    X <- cbind(1, X)
    colnames(X) <- c("intercept", X_vars)
  }
  # Calculate, return beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}



# Function for OLS table

ols <- function(data, y_var, X_vars, intercept = T) {
  # Turn data into matrices
  y <- to_matrix(data, y_var)
  X <- to_matrix(data, X_vars)
  # Add intercept if requested
  if (intercept == T) X <- cbind(1, X)
  # Calculate n and k for degrees of freedom
  n <- nrow(X)
  k <- ncol(X)
  # Estimate coefficients
  b <- b_ols(data, y_var, X_vars, intercept)
  # Calculate OLS residuals
  e <- y - X %*% b
  # Calculate s^2
  s2 <- (t(e) %*% e) / (n-k)
  # Inverse of X'X
  XX_inv <- solve(t(X) %*% X)
  # Standard error
  se <- sqrt(s2 * diag(XX_inv))
  # Vector of _t_ statistics
  t_stats <- (b - 0) / se
  # Calculate the p-values
  p_values = pt(q = abs(t_stats), df = n-k, lower.tail = F) * 2
  # Nice table (data.frame) of results
  results <- data.frame(
    # The rows have the coef. names
    effect = rownames(b),
    # Estimated coefficients
    coef = as.vector(b) %>% round(3),
    # Standard errors
    std_error = as.vector(se) %>% round(3),
    # t statistics
    t_stat = as.vector(t_stats) %>% round(3),
    # p-values
    p_value = as.vector(p_values) %>% round(4)
  )
  # Return the results
  return(results)
}




# R2 Functions ---- 


demeaner <- function(N) {
  i <- matrix(data = 1, nrow = N)
  A <- diag(N) - (1/N) * i %*% t(i)
  return(A)
}


resid_ols <- function(data, y_var, X_vars, intercept = TRUE) {
  require(dplyr)
  y <- to_matrix(the_df = data, vars = y_var)
  X <- to_matrix(the_df = data, vars = X_vars)
  if (intercept == T) {
    X <- cbind(1, X)
    colnames(X) <- c("intercept", X_vars)
  }
  n <- nrow(X)
  resids <- (diag(n) - X %*% solve(t(X) %*% X) %*% t(X)) %*% y
  return(resids)
}


r2_ols <- function(data, y_var, X_vars) {
  
  y <- to_matrix(data, vars = y_var)
  X <- to_matrix(data, vars = X_vars)
  # Add intercept column to X
  X <- cbind(1, X)
  
  N <- nrow(X)
  K <- ncol(X)
  
  # Calculate the OLS residuals
  e <- resid_ols(data, y_var, X_vars)
  # Calculate the y_star (demeaned y)
  y_star <- demeaner(N) %*% y
  
  # Calculate r-squared values
  r2_uc  <- 1 - t(e) %*% e / (t(y) %*% y)
  r2     <- 1 - t(e) %*% e / (t(y_star) %*% y_star)
  r2_adj <- 1 - (N-1) / (N-K) * (1 - r2)

  return(c("r2_uc" = r2_uc, "r2" = r2, "r2_adj" = r2_adj))
}

# Question 1 ----

ols(data = data, y = "emmigrant.ratio", X = c("log.corn", "period"))
r2_ols(data = data, y = "emmigrant.ratio", X = c("log.corn", "period"))
lm(data$emmigrant.ratio ~ data$log.corn + data$period)

ols(data = data, y = "emmigrant.ratio", X = c("log.corn.wheat", "period"))
r2_ols(data = data, y = "emmigrant.ratio", X = c("log.corn.wheat", "period"))
lm(data$emmigrant.ratio ~ data$log.corn.wheat + data$period)

# Does not replicate column 1 

# Question 1 Part 2 ----


# so first regress log.corn on all of those things, save the results (X2_transformed)
# then regress emmigrant.ratio on all of those things, save the results (y_transformed)
# then run a new regression of y_transformed on X2_transformed

X1_vars<-c("period", colnames(dummies)[2:32]) 
#   index only 31, because we want to leave out one dummy
#   Aguascalientes will be the baseline 

X2_transformed <- resid_ols(data, "log.corn", X1_vars)
y_transformed <- resid_ols(data, "emmigrant.ratio", X1_vars)

# Combine the two sets of residuals into a data.frame
Q2_data <- data.frame(y_transformed = y_transformed[,1], X2_transformed = X2_transformed[,1])

b_ols(data = Q2_data, y_var = "y_transformed", X_vars = "X2_transformed", intercept = F)
lm(data$emmigrant.ratio ~ data$log.corn + data$period + data$state)



#now with wheat... 

X2_transformed <- resid_ols(data, "log.corn.wheat", X1_vars)
y_transformed <- resid_ols(data, "emmigrant.ratio", X1_vars)

# Combine the two sets of residuals into a data.frame
Q2_data <- data.frame(y_transformed = y_transformed[,1], X2_transformed = X2_transformed[,1])


b_ols(data = Q2_data, y_var = "y_transformed", X_vars = "X2_transformed", intercept = F)
lm(data$emmigrant.ratio ~ data$log.corn.wheat + data$period + data$state)


# Again, this does not match the paper results 

# ----
# Question 1 Part 3 ----

# Now leaving out the period fixed effect...

ols(data = data, y = "emmigrant.ratio", X = "log.corn")
r2_ols(data = data, y = "emmigrant.ratio", X = "log.corn")
lm(data$emmigrant.ratio ~ data$log.corn)

ols(data = data, y = "emmigrant.ratio", X = "log.corn.wheat")
r2_ols(data = data, y = "emmigrant.ratio", X = "log.corn.wheat")
lm(data$emmigrant.ratio ~ data$log.corn.wheat)

# These results are different that before, and now match the paper. 

# Question 1 Part 4 ----


# so first regress log.corn on all of those things, save the results (X2_transformed)
# then regress emmigrant.ratio on all of those things, save the results (y_transformed)
# then run a new regression of y_transformed on X2_transformed

X1_vars<-colnames(dummies)[2:32]
#   index only 31, because we want to leave out one dummy
#   Aguascalientes will be the baseline 
#   This time we leave out period 

X2_transformed <- resid_ols(data, "log.corn", X1_vars)
y_transformed <- resid_ols(data, "emmigrant.ratio", X1_vars)

# Combine the two sets of residuals into a data.frame
Q4_data <- data.frame(y_transformed = y_transformed[,1], X2_transformed = X2_transformed[,1])

b_ols(data = Q4_data, y_var = "y_transformed", X_vars = "X2_transformed", intercept = F)
lm(data$emmigrant.ratio ~ data$log.corn + data$state)



#now with wheat... 

X2_transformed <- resid_ols(data, "log.corn.wheat", X1_vars)
y_transformed <- resid_ols(data, "emmigrant.ratio", X1_vars)

# Combine the two sets of residuals into a data.frame
Q4_data <- data.frame(y_transformed = y_transformed[,1], X2_transformed = X2_transformed[,1])

b_ols(data = Q4_data, y_var = "y_transformed", X_vars = "X2_transformed", intercept = F)
lm(data$emmigrant.ratio ~ data$log.corn.wheat + data$state)

# These results are different than in part 2, and now match the paper. 



# Question 1 Part 5 ----

# 5. What do you think happened here? And what are the consequences?

# They left out the period fixed effects in the peper version of the model. 


# ----
# ----
# ----
# Part 2 (Normality of OLS) ----
# yi = βo + β1x1i + β2x2i + εi
#(or replace with eta)

p_load(dplyr, lfe, readr, magrittr, parallel, lfe, ggplot2, ggthemes, viridis)


# βo = 3, β1 = 1, and β2 = −2
# Repeat the following exercise for sample sizes of n=[10, 100, 1000, 10000, 20000]
# For each parameterization of the problem run 1,000 repetitions

# Generate an X matrix with 100,000 elements which has ones in the first column. 
# For the second and third column you should generate x1 and x2 from a multivariate 
# normal with mean µ=[0,0]. Let each variable have variance 25 and the covariance be zero. 
# Make sure that each x hasexactly mean zero (use sweep).


# theme ----

tarduno_theme <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank())



# Part A ----

#generate the population 

set.seed(22092008)
sample_size=100000
data_df <- data.frame(
  i = 1,
  x1 = rnorm(sample_size, 0, 5),
  x2 = rnorm(sample_size, 0, 5),
  e = rnorm(sample_size, 0, 5), 
  eta = runif(sample_size, -8.66, 8.66))
# Calculate y = 7 + 0.5 x + e; drop 'e'
data_df %<>% mutate(y_a = 3 + 1*x1  - 2*x2 + e) 
data_df %<>% mutate(y_b = 3 + 1*x1  - 2*x2 + eta)


# Functions ----

# Function to convert tibble, data.frame, or tbl_df to matrix
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

# Function for OLS coefficient estimates
b_ols <- function(y, X) {
  # Calculate beta hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # Return beta_hat
  return(beta_hat)
}


# Function for a single iteration of the simulation
one_run <- function(iter, population, size) {
  sample_df <- sample_n(tbl = population, size)
  
  # Calculate the OLS coef. (using unweighted variables)
  coef_ols <- b_ols(
    y = to_matrix(sample_df, "y_a"),
    X = to_matrix(sample_df, c("i", "x1", "x2")))
  
  # Create a data.frame to return
  coef_df <- data.frame(
    est    = as.vector(coef_ols),
    param  = c("int", "b1", "b2"),
    iter   = iter
  )
  # Return the data.frame
  print(coef_ols)
  return(coef_df)
}



# Make the cluster
cl <- makeCluster(4)
# Load functions on the cluster
clusterEvalQ(cl, {
  library(dplyr)
  library(magrittr)
})
# Export our data and functions to the cluster
clusterExport(cl, "data_df")
clusterExport(cl, c("to_matrix", "b_ols", "one_run"))
# Set seed in parallel
clusterSetRNGStream(cl, 12345)


# this is for sample size 10: 
# for this sample size I: 
#     - Have my population 
#     - Draw a sample size of 10 from my population, at random, 1000 times: 


# repeating for other sample sizes: 
n_list<-c(10, 100, 1000, 10000, 20000)
for (n in n_list) {
  sim_df <- parLapply(
    cl = cl,
    X = 1:1000,
    fun = one_run,
    population = data_df, size=n) %>% bind_rows() %>% tbl_df()
  
  # Plotting ---- 
  
  # Plot a separate histogram of the estimated β2’s for each sample size: 
  
  plot_data<-subset(sim_df,  param == "b2")
  plot<-ggplot(plot_data, aes(est)) + 
    geom_histogram(aes(y = ..density..), binwidth = 1/(n^(2/3))) + 
    stat_function(fun = dnorm, args = list(mean = -2, sd = 1/sqrt(n))) + 
    xlab("Estimate of B2") +
    ylab("Density") +
    ggtitle(paste("Sampling distribution for n =", as.character(n)))+
    tarduno_theme
  
  plot
  print(plot)
}


# Part B ---- 

# Just need to change up what we feed the ols function! 
# Also note that the asymptotic variance is also different: 
# v is going to be 1/12(8.66^2)=6.24963
# z1/v2 is 5/6.24963 = 0.8

one_run <- function(iter, population, size) {
  sample_df <- sample_n(tbl = population, size)
  
  # Calculate the OLS coef. (using unweighted variables)
  coef_ols <- b_ols(
    y = to_matrix(sample_df, "y_b"),
    X = to_matrix(sample_df, c("i", "x1", "x2")))
  
  # Create a data.frame to return
  coef_df <- data.frame(
    est    = as.vector(coef_ols),
    param  = c("int", "b1", "b2"),
    iter   = iter
  )
  # Return the data.frame
  print(coef_ols)
  return(coef_df)
}


# repeating for other sample sizes: 
n_list<-c(10, 100, 1000, 10000, 20000)
for (n in n_list) {
  sim_df <- parLapply(
    cl = cl,
    X = 1:1000,
    fun = one_run,
    population = data_df, size=n) %>% bind_rows() %>% tbl_df()
  
  # Plotting ---- 
  
  # Plot a separate histogram of the estimated β2’s for each sample size: 
  
  plot_data<-subset(sim_df,  param == "b2")
  plot<-ggplot(plot_data, aes(est)) + 
    geom_histogram(aes(y = ..density..), binwidth = 1/(n^(2/3))) + 
    stat_function(fun = dnorm, args = list(mean = -2, sd = .8/sqrt(n))) + 
    xlab("Estimate of B2") +
    ylab("Density") +
    ggtitle(paste("Sampling distribution for n =", as.character(n)))+
    tarduno_theme
  
  plot
  print(plot)
}


# Overlay a density of what the theoretical distribution of β2 should look like. What do you notice?


# very similar to the part above -- still asymptotically normally distributed 
# The speed of convergence, though, is different -- At 100 is seems that the uniform if further from the theoretical distribution than is the normal case in part A. 
