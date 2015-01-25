##### CACHED INVERSE OF MATRIX #####
##
## The first function "makeCacheMatrix" creates a special "matrix" that is able to cache its own inverse
## The second function "cacheSolve" returns inverse of the special "matrix", either the cached
##      one (if it available) or a newly computed one.

## function makeCacheMatrix
#       - creates a matrix that is able to cache its own inverse
#       - it actually returns a list of four functions:
#               set(A) -> sets the matrix to new values B
#               get() -> returns the matrix itself
#               setinv(inverse) -> saves the inverse into cache
#               getinv() -> returns the inverse
#       - functions setinv, getinv are not supposed to be called by an user,
#           they are called by the function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    # initialization - set the inverse to NULL
    inv <- NULL
    # change the value of matrix - save the new matrix into X and reset the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # return the matrix X
    get <- function() x
    # save the inverse of X
    setinv <- function(inverse) inv <<- inverse
    # return the inverse of X
    getinv <- function() inv
    # return the list with the functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## function cacheSolve
#       - returns the inverse of the matrix
#       - if the inverse has been already computed, it returns the cached matrix
#       - otherwise it computes the inverse, saves it into cache and returns it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # first, load the saved inv value
    inv <- x$getinv()
    # if it is not NULL, it is the inverse of the matrix X and can be returned
    if (!is.null(inv)) {
        # turn on the messaging by uncommenting the following line:
        # message("getting cached data")
        return(inv)
    }
    # otherwise, load the stored matrix, compute its inverse and save it
    A <- x$get()
    inv <- solve(A, ...)
    x$setinv(inv)
    # return the inverse
    inv
}

