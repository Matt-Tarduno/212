# Tarduno version of section 1 notes ----------------------------

# Install the package named "dplyr." Note the s. 
install.packages("dplyr")
# Install two packages at once. "c" is for concatenate. 
install.packages(c("haven", "readr"))

#Loading packages 
library(dplyr)
library(haven)
library(readr)

# Install packman (v handy)
install.packages("pacman")
library(pacman)

# thsoe three call above now become: 
p_load(dplyr, haven, readr)
# it will also get stuff for you if you haven't already done it!
p_load(ggplot2)

# Directories ----------------------------------------------------
setwd("/Users/matthewtarduno/Desktop/212/Section01")
#eventually won't use this

# The path to my ARE 212 folder (212)
dir_class <- "/Users/matthewtarduno/Desktop/212/"
# The path to my section 1 folder (Section01), which is inside my ARE 212 folder
dir_section1 <- paste0(dir_class, "Section01/")

#check out folders with dir() (instead of ls in terminal). 
dir(dir_section1) #list things in this directory 

# Load Data ------------------------------------------------------
# Load the .dta file
car_data <- read_dta(paste0(dir_section1, "auto.dta"))

#check it out like this: 
car_data 

# Load the .csv file
car_data <- read_csv(paste0(dir_section1, "auto.csv"))

#can also just use the fact that we are in section 1 folder: 
read_csv("auto.csv")

# Looking at data -------------------------------------------------

#checking out names, what the data look like: 
names(car_data)
head(car_data)

#can specify what parts of the data we want to look at: 
head(car_data, n = 11)

#Last 7 rows of the dataset 
tail(car_data, n = 7)

# Summarizing the data ---------------------------------------------
summary(car_data)
#index to a single var: gives summary of this var. 
# Notice typing car_data$ ... opens tab of options
summary(car_data$price)

# Using dplyr finctions (verbs!)

# Select our desired variables; define as car_sub 
#like keep 
car_sub <- select(car_data, price, mpg, weight, length)
carsub #print the dataset

select(car_data, -price, -mpg, -weight, -length)
#notice this prints out what this dataframe would be, but we aren't assigning it to anything 
 
car_sub <- select(car_data, price, mpg, weight, length)
#and then arrange the variables in the way we want 
arrange(car_sub, price, mpg)
#we can also go backwards 
arrange(car_sub, desc(price), mpg)

#for more specific stats, use summarize 
summarize(car_sub, mean(price), sd(price))
#can create names for these new variables, too: 
summarize(car_sub, price_mean = mean(price), price_sd = sd(price))

#... or just type these: 
mean(car_sub$price)
sd(car_sub$price)

# Plotting the data ------------------------------------------------------------

?hist #gives information about histograms 
??plot #this searches for functions

#A simple histogram:  
hist(car_sub$mpg)

#A little bit more: 
# The histogram function
hist(
  # The variable for the histogram
  x = car_sub$mpg,
  # The main title
  main = "Distribution of fuel economy",
  # The x-axis label
  xlab = "MPG (miles per gallon)")
# The blue vertical line at the median MPG (lwd is line width)
abline(v = median(car_sub$mpg), col = "blue", lwd = 3)


# Scatter plot
plot(
  x = car_sub$mpg,
  y = car_sub$price,
  xlab = "Fuel economy (MPG)",
  ylab = "Price")

# Indexing ---------------------------------------------------------------------
# Create a vector

x <- c(3, 5, 7, 9)
# Grab the second element of x
x[2]
# Grab the second and third elements of x
x[c(2, 3)]
# Grab the second and third elements of x
x[2:3]
# See what 2:3 does
2:3
# Indexing the car subset: We use [row, column]. 
car_sub[1, ] #gets the first row
car_sub[, 1] #gets the first column

# Index using the name of a column as its index
car_sub[, "price"]

# Linear algebra puzzles -----------------------------------------------

# Puzzle one ------------------------------------------------------------

#built-in functions
i<-diag(5)
i%*%i #show the i is indempotent
t(i) #show matrix is symetric 

# A function to invert matricies. 

invert.matrix <- function(mat) {
M<-c() #a blank vector
for (column in 1:dim(mat)[2]){
  for (row in 1:dim(mat)[1]){
    new_element<-mat[row,column]
    M<-c(M, new_element)
  }
}
M<-matrix(M, nrow = dim(mat)[2], ncol = dim(mat)[1], byrow='True')
return(M)
}

# Puzzle 2 ---------------------------------------------------------------

#Should be an idempotent matrix: 
#[1/2, 1/4]
#[1, 1/2]

#Simple test, using built-in functions: 
A<-matrix(c(.5, .25, 1, .5), nrow = 2, ncol = 2, byrow = 'True')
A%*%A #note the % for matrix multiplication 

# Puzzle 3 ---------------------------------------------------------------

#Generate two random variables, x and e, of dimension n = 1000 such that x, e ∼ N(0, 1). 

generate.rv <- function(dimension) {
  v<-c() #a blank vector
  for (i in 1:dimension){
    v<-c(v, rnorm(1, mean = 0, sd = 1))
  }
return(v)  
}

#gernerate random variables 
e<-generate.rv(1000)
x<-generate.rv(1000)

#make y variable
y<- x+ e 

#run regression 
lm(y ~ x)

# ------------------------------------------------------------------------







