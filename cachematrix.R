# Takes matrix as a parameter and creates 
# matrix like object with cached inverse matrix
# Returns a list with four options:
# set (Sets a matrix)
# get (Gets a matrix)
# setinverse (Sets an inverse matrix to cache)
# getinverse (Gets an inverse matrix)

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


# Returns a matrix that is inverse of parameter matrix
# Returned value is returned from cache if possible
# or calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        orig <- x$get()
        inv <- solve(orig, ...)
        x$setinverse(inv)
        inv
}
