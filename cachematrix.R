
### ASSIGNMENT 2, WEEK 3 IS BELOW

## Put comments here that give an overall description of what your
## functions do

# In this pair of functions a matrix is entered as an argument. The first function returns an object of type list() 
# which allows access to other objects defined in the environment of the function. Thus, the second function can 
# access the values of the matrix from the first function through use of getters and setters of the first function. The 
# inverse of the matix is calculated in the cachemean() function. 


## Write a short comment describing this function

## makeCacheMatrix

# In the first two lines of the function the objects X and INVVAR are initialized. 
# X is a function argument. The default value of X is an empty matrix.

# The set function is defined within the function makeCacheMatrix. It takes an argument Y.The <<- assignment
# operator is used so that the Y value will be assigned to X in the parent environment when set is called. 
# Similarly, NULL will be assigned to INVVAR in the parent environment; this clears any previously cached value 
# of INVVAR when the set function is later called. The set function can be called later to reset the value of the matrix X. 
# Set also makes the value of INVVAR become NULL, so that a new value of INVVAR will be calculated in the cacheSolve function. 
# Using the set function makes calling makeCacheMatrix unnecessary when there is a new matrix to evaluate.

# With the get function the value of X is retrieved from the parent environment as X is not defined within get().

# With the setinver function the <<- assignment operator assigns the argument INVER to INVVAR in the parent environment.

# With the getinver function the value of INVVAR is retrieved from the parent environment.

# At the end of the code the four functions (set, get, setinver and getinver) are assigned to a list and returned
# to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        invvar <- NULL
        set <- function(y) {
                x <<- y
                invvar <<- NULL
        }
        get <- function() x
        setinver <- function(inver) invvar <<- inver
        getinver <- function() invvar
        list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## Write a short comment describing this function

# The object from makeCacheMatrix is passed to the cacheSolve function. The object from makeCacheMatrix not only
# has specific function in the list, but it also has access to the environment defined by makeCacheMatrix(), 
# including the matrix X.

# With the first line, getinver tries to obtain the inverse from the object passed as the argument. 
# Next cacheSolve checks to see when INVVAr is NULL. If it is not NULL then there is a valid cached inverse that can 
# be returned to the parent environment. The message 'getting cached data' is printed, and the cached inverse is returned.
# If INVVAR is NULL then the matrix is taken from the object input to the function, the inverse is solved for, and 
# the function setinver function sets the inverse in the input object, and the value of the inverse is returned
# to the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invvar <- x$getinver()
        if(!is.null(invvar)) {
                message("getting cached data")
                return(invvar)
        }
        data <- x$get()
        invvar <- solve(data, ...)
        x$setinver(invvar)
        invvar
}

## The functions were tested with the following code

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) # create matrix
m1# print matrix
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2) # create identity matrix
I2 # print identity matrix
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2) # make the inverse matrix for m1
n1 # print inverse matrix
m1 %*% n1 # prove that m1 * n1 yields identity matrix
n1 %*% m1 # same as above
solve(m1) # get inverse of m1 -- should be n1
solve(n1) # get inverse of n1 -- should be m1

myMatrix_object <- makeCacheMatrix(m1) # test the functions in the script above -- should solve and yield n1
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object) # running command again retrieves the inverse without recalculating it

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2) # use th set function to put in a new matrix
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object) # running command again retrieves the inverse without recalculating it
