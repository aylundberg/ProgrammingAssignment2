## This pair of functions allow for the inverse of a matrix to be cached

## The makeCachedMatrix function creates a special "matrix" object that allows
## its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initializes the inverse
        set <- function(y) {
                x <<- y ## sets a new matrix value for the object
                inv <<- NULL ## reinitializes the inverse (since matrix value
                             ## for the object has changed)
        } 
        get <- function() x ## returns the matrix value of the object
        setinv <- function(inver) inv <<- inver ##sets the inverse of the matrix
        getinv <- function() inv ## returns the inverse of the matrix
        
        ## returns a list containing the functions for the special "matrix"
        ## object with cacheable inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function computes an inverse of a special "matrix" object
## created using the makeCachedMatrix function.  If the both the inverse
## of the "matrix" object has been calculated and the "matrix" object itself
## has not changed, the function will pull the cached inverse.  Otherwise
## the function will calculate the inverse of the "matrix" object.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() ## pulls cached inverse from "matrix" object
        
        ## if the cached inverse of the "matrix" object exists (i.e. is not
        ## null, return the cached inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } 
        
        ## else calculate the inverse of the matrix value of the object and
        ## store within the object
        data <- x$get() ## retrieve matrix value of the object
        inv <- solve(data, ...) ## calculate the inverse of the matrix value
        x$setinv(inv) ## store the calculated inverse in the matrix object cache
        inv ## return the inverse of the matrix value
}
