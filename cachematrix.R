## This is a pair of functions that cache the inverse of a matrix
## The first function caches the matrix and solves. The second returns it
##  if it has been solved already.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    ##Cache the solved matrix
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(setinv) inv <<- setinv
    getinverse <- function() inv
   
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
