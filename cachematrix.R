# Functions to compute the inverse of a matrix and
# cache the result instead of having to recompute it.

# Creates a special "matrix", which is really a list
# containing the following functions:
#  - set the value of the matrix
#  - get the value of the matrix
#  - setInverse the value of the inverse
#  - getInverse the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Calculates the inverse of the special "matrix" created
# with makeCacheMatrix(). However, it first checks to see
# if the inverse has already been calculated. If so, it
# gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse and sets the value
# in the cache via setInverse().
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
