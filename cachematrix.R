## This code provides functions that are able to cache a potentially time-consuming
## computation: calculating the inverse of a matrix.


## The following function creates a special "matrix" which is really a list
## containing a function to:
##  - set the data of the matrix
##  - get the data of the matrix
##  - set the inverse of the matrix
##  - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setInverse function. It is
## assumed that the matrix is always inversible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)%*%data
    x$setInverse(i)
    i
}
