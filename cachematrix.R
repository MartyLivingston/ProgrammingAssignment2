## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #creates a matrix of functions to use for caching the inverse
        #x is the matrix that we will want the inverse of
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        matrix(c(set, get, setinverse, getinverse)
                , nrow = 2, ncol = 2
                , dimnames = list(c("set","get"),c("base","inverse")))
}


## Write a short comment describing this function

cacheSolve <- function(x,y, ...) {
        #creates and caches the inverse of a matrix for future use
        #x is the matrix of functions for finding the inverse
        #y is the matrix that we will want the inverse of
        m <- x[["get","inverse"]]()
        data <- x[["get","base"]]()
        if(!is.null(m) && identical(data,y)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(y, ...)
        x[["set","inverse"]](m)
        m
}
