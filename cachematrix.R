## Check cache for the inverse matrix value.  If the matrix supplied is exactly
## the same as the previous call, the cache will be available and time will be saved
## by pulling from cache instead of computing the value.  

## Create a special matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInverseMatrix <- function(i) inv <<- i
    getInverseMatrix <- function() inv
    list( setMatrix = setMatrix, getMatrix = getMatrix, 
          setInverseMatrix = setInverseMatrix, 
          getInverseMatrix = getInverseMatrix)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverseMatrix()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$getMatrix()
    invMatrix <- solve(data)
    x$setInverseMatrix(invMatrix)
    invMatrix
}
