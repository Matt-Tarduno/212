# tarduno section 2 

# setup --------------------------------

# not much to see here 

setwd("/Users/matthewtarduno/Desktop/212/Section02")


# types -------------------------------

class(2)
class("Max Auffhammer")

# vectors -------------------------------

# Create a vector called 'vec'
vec <- c(1, 2, 3, 4, 5)
# Print the vector named 'vec'
vec

# Create a vector of the sequence from 1 to 5 and store it as 'vec2'
vec2 <- 1:5
# Print 'vec2'
vec2

# Are the two vectors' elements equal?
vec2 == vec

# Are the two vectors equal?
all.equal(vec, vec2)

# Create another numeric vector
vec3 <- c(1, 2, 8:10)
# Print it
vec3

# Check element-wise equality
vec == vec3

# Check overall equality
all.equal(vec, vec3)

# Create a new vector by combining 'vec2' and 'vec3'
vec23 <- c(vec2, vec3)
# Print the new vector
vec23

# The class function
class(vec23)


# The is.vector function
is.vector(vec23)

# The dimension function
dim(vec23)

# The length function
length(vec23)

vec23[7]

# Character vectors ------------------------

# Create the string vector using quotation marks
str_vec <- c("Aren't", "vectors", "exciting", "?")
# Print it
str_vec

# Check its class
class(str_vec)

str_vec[3]


# Create a vector of the numeric vector 'vec' and the character vector 'str_vec'
mix_vec <- c(vec, str_vec)
# Print the result
mix_vec


# Check the class of the new vector (defaults to character when mixed)
class(mix_vec)


# Matrices -----------------------------

# Create a 3x2 matrix filled with the sequence 1:6
A <- matrix(data = 1:6, ncol = 2)
# Print it
A


# The dimension of A
dim(A)


# The length of A
length(A)


# Check if A is a vector
is.vector(A)

# Check if A is a matrix
is.matrix(A)

# Print the second row of A
A[2,]

# Print the second and third rows of A
A[2:3,]

# Print the first column of A
A[,1]

# Print the element in the second row and first column of A
A[2,1]

# transpose --------------------------------------------

# Using the byrow option
B <- matrix(data = 1:6, ncol = 3, byrow = TRUE)
# Print B
B

# Check element by element
t(A) == B

# Check if all the element-by-element comparisons are TRUE
all(t(A) == B)

# Check if the transpose of A is identical to B
identical(t(A), B)

# matrix multiplication --------------------------------

# Elementwise multiplication of A and A
A * A

# Matrix multiplication of A and itself
A %*% A

#scalar
A * 3

# Create C
C <- matrix(data = 1:4, ncol = 2)
# Multiply A and C
A %*% C

diag(5)

# Grab the diagonal elements of the matrix C (interesting that it both makes and operates on matricies)
diag(C)

# Take the inverse of C
solve(C)

# Multiply C by its inverse
C %*% solve(C)

# The determinant of a matrix
det(C)


# The trace of a matrix using the psych package's tr()
library(psych)
tr(C)

sum(diag(C))


# Bind a column of ones in front of the matrix A (probably useful...)
cbind(c(1, 1, 1), A)

#also works, but be careful, CJ 
cbind(1, A)

#just binding other stuff, I guess... 
rbind(A, c(3, 6))

#can also bind matricies, if their dimensions match!
rbind(A, C)

