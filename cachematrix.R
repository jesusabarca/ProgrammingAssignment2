## This script consists in two functions to get the inverse of a matrix and cache the result, so if the function
## is executed again for the same matrix, it will returned the cached result instead of computing it again.

## This function creates a list with four functions to store a version of the original input, retrieve the original
## input, put in cache the result of the previous computation and to retrieve the previous result.

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


## Front end for the makeCacheMatrix function, it calls the functions of the list returned by the makeCacheMatrix
## function to compare the previous computation (input and result) with the new input. If the new and previous inputs
## are the same, then it returns the previous result stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
