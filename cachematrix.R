## Create matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        k  <- NULL
        set  <- function(y){
                x <<- y
                k <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) k  <<- inverse
        getinverse  <- function() k
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## Computes inverse of matrix.
## If inverse has already been calculated then the cachesolve function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        k  <- x$getinverse()
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        data  <- x$get()
        k  <- solve(data, ...)
        x$setinverse(k)
        k
}
