## This code follows the same pattern as the example code provided to cache the mean
## of a vector. 
## The makeCacheMatrix function returns a list of functions with getters and setters for
## the matrix itself and for its inverse. 
## The cacheSolve function computes the inverse of the matrix if it needs to - otherwise
## it will return the cached version.

## makeCacheMatrix returns a list of functions to get and set both the matrix and to get 
## and set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(pinv) inv <<- pinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve takes a makeCacheMatrix "special matrix" and calls the solve
## function to compute the inverse if the "special matrix" does not have the
## cached version of the inverse.

cacheSolve <- function(x, ...) {
    
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv  
    
}
