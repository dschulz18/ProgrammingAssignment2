## Together, these functions allow a matrix to be defined and its inverse
## calculated and cached. (It is assumed that matrices will be a square
## invertible matrix)

## This function contains methods to set a matrix, get the matrix, set the
## inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inv = NULL
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


## This function takes in a makeCacheMatrix list and returns the cached inverse
## matrix. If no inverse is cached, it calculates the inverse, caches it in
## the list and returns the value

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
