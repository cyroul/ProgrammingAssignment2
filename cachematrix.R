##-----------------------------------------------------------------------
##  This function creates a list of 4 functions that can interact
##  with a cache frame containing a matrix 'x' and its inverse 'xinv'.
##  It also inits cached data : 'x' to numeric() and 'xinv' to NULL.
##-----------------------------------------------------------------------
##  USAGE :  cache <- makeCacheMatrix()  : to make a list named 'cache'
##-----------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) { x <<- y ; xinv <<- NULL }
    get <- function() x
    setsolve <- function(mat) xinv <<- mat
    getsolve <- function() xinv
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
##-----------------------------------------------------------------------
##  This function returns the inverse of a given matrix.
##  It simply returns the cached matrix 'xinv' if available (not NULL),
##  or computes it as solve('x'), and then stores it in 'xinv'.
##-----------------------------------------------------------------------
##  USAGE :  cache$set(a),       caches matrix 'a' in 'x'
##  USAGE :  cache$get(),        returns the cached matrix 'x'
##  USAGE :  cacheSolve(cache),  returns inverse of the cached matrix
##-----------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    xinv <- x$getsolve()
    if(!is.null(xinv)) {message("getting cached data"); return(xinv) }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setsolve(xinv)
    xinv
}