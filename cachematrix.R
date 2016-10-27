## Functions for Programming Assessment 2
## To allow caching of the inverse of a matrix

## Creates an object from a matrix with functions to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Returns the inverse of a matrix from cache if available otherwise it caches the answer

cacheSolve <- function(x, ...) {
        ## set i to the the value from x$getinverse()
        i <- x$getinverse()
        if(!is.null(i)){
                ## if it's not null print out an advisory message
                message("getting cached data")
        }
        else{
                ## otherwise generate the inverse using solve and cache it
                ## using the  setinverse function of x
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
        }
        i
}
