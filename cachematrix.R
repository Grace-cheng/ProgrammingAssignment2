## The overall purpose of the functions is to cache the inverse of a special
## matrix to save all the trouble of repeated calculation as well as RAM/space

## This function creates a special matrix object in order to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special matrix created 
## by makeCacheMatrix above.
## First check if the inverse has already been computed
## if so, get the result and return
## if not, compute the reverse, then use setInverse to set the value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
