## Can:
##  1.  Create a cache of a matrix.
##  2.  Get a matrix from memory.
##  Will store both a matrix and its inverse as a cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { m <<- inverse }
    getinverse <- function() { m }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks to see if the inverse of our matrix is chached.
## If it is, returns the inverse matrix.
## If not, calculates the inverse, caches it, then returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message('getting cached inverse matrix')
        return(m)
    }
    message('creating inverse matrix')
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
