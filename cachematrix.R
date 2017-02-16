
## A pair of functions that computes and stores the inverse of amatrix on cache.


## Creates a special "matrix" that stores its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of the special "matrix" created with the above 
## function. It first checks to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

# Checking...
x <- matrix(c(1,2,2,6), byrow = T, nrow = 2, ncol = 2)
m <- makeCacheMatrix(x)
m$get()
cacheSolve(m)
