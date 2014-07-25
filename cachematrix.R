## The following two functions use cache to speed up the calculation of the inverse
## of a matrix.

## This function creates a list containing a function to set the value of a matrix,
## get the value of the stored matrix, set the inverse of the matrix and get the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with the above function.
## If the inverse is residing in the cache, then it brings forth the inverse.
## Else, it calculates the inverse of the matrix and sets the value using the 
## setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
