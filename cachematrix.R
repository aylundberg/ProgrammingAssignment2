## This pair of functions allow for the inverse of a matrix to be cached

## The makeCachedMatrix function creates a special "matrix" object that allows
## its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inver) inv <<- inver
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function computes an inverse of a special "matrix" object
## created using the makeCachedMatrix function.  If the both the inverse
## of the "matrix" object has been calculated and the "matrix" object itself
## has not changed, the function will pull the cached inverse.  Otherwise
## the function will calculate the inverse of the "matrix" object.

cacheSolve <- function(x, ...) {
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
