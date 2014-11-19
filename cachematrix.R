## This is a code which is handy when you need to repeatedly compute inversion of the matrix. 
## The inversion may be costly operation, so we're trying to reduce this cost by caching.
## These two functions work in tandem. 

## This function creates a special caching object contaninig the original matrix as well as the inversion. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes this special object and returns inverse matrix from cache, 
## if it's been already computed, or computes the inversion and puts in into cache.
cacheSolve <- function(x, ...) {    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
