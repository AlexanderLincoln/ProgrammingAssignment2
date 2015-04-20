## This is a pair of functions that cache the inverse of a matrix
## The first function caches the matrix and its inverse. The second returns it
##  if it has been solved already or solves if the solution is not solved.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    
## set inverse to null when called(original matrix could change)
    inv <- NULL
    
## stores original matrix
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
## returns the original matrix
    get <- function() m
    
## allows cacheSolve() to store inverse in makeCacheMatrix() environment
    setinverse <- function(setinv) inv <<- setinv
    
## returns the cached matrix
    getinverse <- function() inv
   
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}


## cacheSolve takes the function x and tries to get the inverse using x$getinv(). 
## If the inverse has not been calculated, it uses x$get() to pass the original 
## matrix to solve(). The result is stored in x using x$setinverse(). 

cacheSolve <- function(x, ...) {
    
## get cached inverse matrix from x()        
        inv <- x$getinv()
        
## return the inverse if already cached.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
## if not cached, get the original matrix, solve, cache in x() and return 
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
