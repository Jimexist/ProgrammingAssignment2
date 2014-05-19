
# returns a matrix that can remember its own inverse
makeCacheMatrix <- function(x = matrix()) {
    # set up initial value to be NULL
    inv <- NULL

    # get / set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    # get / set the cached inverse
    getinverse <- function() inv
    setinverse <- function(val) inv <<- val

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# solve the inverse of the matrix, by firstly
# checking if the matrix already has a cached
# inverse and returns it if available, else
# calculate the inverse, set the value to be
# cached and return the calculated value
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }

    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
