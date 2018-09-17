rm(list=ls())

# My first function creates a matrix (in fact a list consisting of four elements). 
# My second function retrieves the inverse of the matrix from the cache. If there is
# no such information in the cache, it calculates the inverse of the matrix.


# This function creates a matrix (in fact a list consisting of four elements)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               #this creates an empty object
        set <- function(y) {                    #this function sets the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                     # this function retrieves the value of the matrix 
        setinverse <- function(inverse) m <<- inverse # this function sets the inverse of the matrix
        getinverse <- function() m              # this function retrieves the inverse of the matrix
        list(set = set, get = get,              # this is the output produced by the function
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function retrieves the inverse from the cache. If this information is not available
# it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                     # retrieving object "m"
        if(!is.null(m)) {                       # testing if the object "m" is empty
                message("getting cached data")
                return(m)                       # returning the object "m" if it is not empty
        }
        data <- x$get()                         
        m <- solve(data, ...)                   # calculating the inverse of the matrix
        x$setinverse(m)
        m                                       # returning the object "m"
}

# Testing the functions

A <- matrix(rnorm(9),3,3)                       # creating a matrix
B <- makeCacheMatrix(A)                         # calculating the inverse and storing it in the cache
C <- cacheSolve(B)                              # retrieving inverse of matrix from the cache

