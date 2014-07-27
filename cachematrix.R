## Put comments here that give an overall description of what your
## functions do

# The following two functions are used to calculate and cache the inverse of a
# matrix. As matrix inversion is usually a costly computation, it would 
# benefit the user to cache the inverse of the matrix, rather than computing 
# it repeadtedly.

# Method to use the functions:
# 1. Store the invertible matrix for which the inverse is to be determined 
#    in a variable x.
# 2. Run y <- makeCacheMatrix(x)
# 3. Run cacheSolve(y), to get the result.

# The result:
# 1. If the matrix inverse is already cached, the inverse will be read from
#    the cached memory and printed along with the message "getting cached data"
# 2. Else, the matrix inverse will be calculated, cached for further usage and
#    printed without any message.


## Write a short comment describing this function

# makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse. 
# It has a list containing a function to
# set: set the value of the "matrix" object.
# get: get the value of the "matrix" object.
# setinverse: set the value of the inverse.
# getinverse: get the value of the inverse.  
# The <<- operator is used to assign a value to an object in an environment
# that is different from the current environment.


makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# cacheSolve:
# This function computes the inverse of the special "matrix" returned by the 
# the function 'makeCacheMatrix'.
# The inverse of a square matrix is computed using the function 'solve'.
# This function first checks if the inverse of the matrix is already cached.
# If it is, then it prints a message and return the inverse. If it isn't ,
# then it calculates the inverse, caches it and prints the inverse.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m	  ## Return a matrix that is the inverse of 'x'
}
