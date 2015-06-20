## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
