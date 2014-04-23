## Put comments here that give an overall description of what your
## functions do

## This function creates 4 functions and places them in a list. Essentially each item in the list is an attribute of the makeCacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks to see if there is a solution to the inverse of a matrix. If so, it will get the cached data, if not, then it will find the inverse of the matrix.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
