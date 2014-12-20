## function makeCacheMatrix(x = matrix()):
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


## function cacheSolve(x, ...)
## returns the inverse matrix of X we are using.
## @arguments
##    x: the 'matrix' spanned by function makeCacheMatrix.
##       Must include these attributes: get, set, setSolve, getSolve

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    ## If we have already execute 'cacheSolve(X)' and we didn't change the value of X,
    ## there will be an cache in X and we can get it.
    if (!is.null(s)) {
        message("Getting cached data...")
        return(s)
    }
    ## If this is the first time we execute this function since the last time we change the value of X,
    ## then execute these blocks...
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
