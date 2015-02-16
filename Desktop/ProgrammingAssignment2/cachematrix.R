## These functions take a matrix, compute its inverse and cache
## the inverse value of the matrix that was input

## This function creates a special "matrix" object that can cache ## its inverse

makeCacheMatrix <- function(x = matrix()) {
        value <- NULL
        set <- function(y) {
                x <<- y
                value <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) value <<- inverse
        getinverse <- function() value
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functions computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function above. It the inverse
## has already been calculated, then the cachesolve should 
## retrieve the inverse from the cache and tell the user it has
## done so

cacheSolve <- function(x, ...) {
        value <- x$getinverse()
        if(!is.null(value)) {
                message("getting cached data")
                return(value)
        }
        data <- x$get()
        value <- solve(data, ...)
        x$setinverse(value)
        value
}
