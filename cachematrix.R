## Coursera R Programming Week 3, Assignment 2
##
## G Essex 20Jun15
##
## The 'makeCacheMatrix function stores the input from a matrix into a cache so
## that the values an be used by another function calling this one.
## To better understand the logic behind this function, read the following:
## https://github.com/DanieleP/PA2-clarifying_instructions

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        ## the 'set' option from example hashed out as 
        ## not required for submission 
        ## set <- function(y) {
        ##         x <<- y
        ##         s <<- NULL
        ## }
        #get used to show the matrix values entered
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(get = get,
             ## set=set ##not required for submission
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function takes the input matrix from the 'makeCacheMatrix'
## function. If the cache already contains a previous value, the result
## is output direct from the cache. 
## If no previous result exists, the function calculates a new set of results.
## The results are the 'solve' output of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the result 's' is already cached in 'getsolve, return from cache
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## otherwise calculate new value for 's' into 'data', and store
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
