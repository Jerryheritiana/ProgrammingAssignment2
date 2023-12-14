## the function makeCacheMatrix creates a special vector which is actually
## a list containing a function to set the value of a matrix, to get the value
## of a matrix, to set the value of the inversion and to get the value
## of the inversion

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the cacheSolve function calculates the inverse of the matrix of the special
## vector from the makeCacheMatric function. However, it first checks whether
## the inverse has already been calculated. If this is the case, it retrieves
## the inverse from the cache and skips the calculation. Otherwise, the inverse
## of the data is calculated and the value of the inverse is set in the cache 
## using the setinverse function.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
