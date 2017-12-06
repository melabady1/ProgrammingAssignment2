## Cahing the the inverse of a matrix,
## Rather than calculating the same inverted matrix 
## everytime for the same object


## Takes a matrix and creates a list of four functions:
## get - get the inputed matrix (Argument x).
## set - change the value of the matrix.
## getinverse - get the cached inverted matrix of x.
## setinverse - cache the inverted matrix of x.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## return the cached iverted matrix if exists,
## otherwise calculate the iverse and cashe the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if(!is.null(inv)) {
            message("getting cached inversed matrix")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
