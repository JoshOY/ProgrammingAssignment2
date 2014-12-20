## function makeCacheMatrix (x = matrix()):
## returns a list, which includes several methods to cache the solve of this matrix.
## @arguments 
##    x: A optional matrix data to initialize the special 'matrix'.
##       Default value is matrix().

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() {
        x
    }
    setSolve <- function(solve) {
        s <<- solve
    }
    getSolve <- function() {
        s
    }
    list ( set = set,
           get = get,
           setSolve = setSolve,
           getSolve = getSolve )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if (!is.null(s)) {
        message("Getting cached data...")
        return(s)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
