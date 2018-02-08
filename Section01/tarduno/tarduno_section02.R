# Tarduno section 2 (Data Structures) --------------------------------------

#check out the class of a number, a string
class(2)
class("Max Auffhammer")

# Vectors ------------------------------------------------------------------

# Create a vector called 'vec' (c is for combine, or concatenate)
vec <- c(1, 2, 3, 4, 5)
# Print the vector named 'vec'
vec

# Create a vector of the sequence from 1 to 5 and store it as 'vec2'
vec2 <- 1:5
# Print 'vec2'
vec2

#element-wise equal? 
vec2 == vec
# Are the two vectors equal?
all.equal(vec, vec2)


# Create another numeric vector
vec3 <- c(1, 2, 8:10)
# Print it
vec3

# Check overall equality (returns mean relative difference)
all.equal(vec, vec3)

#Combining vectors: 
vec23 <- c(vec2, vec3)

#class of vector is it's contents?
class(vec23)

#interestingly this will be character (mixed always character):
v1<-c("a", "b", 2)
class(v1)

#to test if object is a vector: 
is.vector(vec23)

dim(vec23) #null
length(vec23) #returns 10 
vec23[7]

#Matricies ----------------------------------------------------------------

#Creating matricies: 
# Create a 3x2 matrix filled with the sequence 1:6
A <- matrix(data = 1:6, ncol = 2) 
B <- matrix(data = 1:6, nrow = 2) #can also do nrow, but not intuitive 
# Print it
A
B

#check out this martix: 
dim(A)
length(A) #interestingly, still has length 
is.vector(A)
is.matrix(A)

# Print the second row of A
A[2,]
# Print the second and third rows of A
A[2:3,]
# Print the first column of A
A[,1]
# Print the element in the second row and first column of A
A[2,1]

# Transpose ---------------------------------------------------------------

#This will make transpose of matrix A
B <- matrix(data = 1:6, ncol = 3, byrow = TRUE)
t(A) == B
all(t(A) == B)
#or this 
identical(t(A), B)


# Multiplication ----------------------------------------------------------

# Elementwise multiplication of A and A
A * A

# Matrix multiplication of A and itself
A %*% A

#scalar mult. 
A * 3

--











