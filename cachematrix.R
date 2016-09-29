## The first function creates a "CacheMatrix object" from an R matrix.
## The second function calculates the inverse of the matrix contained in the
## "CacheMatrix object". If the inverse was already cached, this function 
## returns the value contained in the "CacheMatrix object"

## Provided a matrix, it creates a "matrix object" that caches its own inverse. 
## This object has getters and setter for both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    ##Return list of get/set methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of x and stores it 
## into the "cacheMatrix object" x

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
        
}
