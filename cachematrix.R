## R Programming, Coursera
## Instructor: R.D. Peng
## Programming Assignment 2: Lexical Scoping
## June 21, 2015
##
## Objective: To learn how lexical scoping can be used for caching to
## avoid unnecessary repeated operations.  Note that <- assigns a
## variable only in the local environment while <<- modifies an
## existing variable found by walking up the parent environment.  See 
## http://adv-r.had.co.nz/Environments.html for more on environments.

## makeCacheMatrix() takes a matrix as its input and caches the
## inverse of a matrix when cacheSolve() is called.  It stores 4
## functions: set (which provides the option of setting a new matrix), 
## get (which allows the user to get the current matrix), getinv, and
## setinv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_m) inv <<- inv_m
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve() finds the inverse of a matrix but checks first to see
## whether the inverse has been cached.  If so, it retrieves it.

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
