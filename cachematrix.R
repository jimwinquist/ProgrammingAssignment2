## The following pair of functions work together to cache the inverse of a 
## matrix for future use. By caching the inverse of the matrix we can
## avoid the overhead of recomputing this value every time it is needed.


## This function takes a matrix as input and returns a special list object 
## containing function calls for getting and setting attributes of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a special list object created by the makeCacheMatrix  
## function as input and uses the function definitions provided by the 
## list to test whether the inverse matrix has been previously cached.
## If it has been cached the cached inverse matrix is returned,
## and if it hasn't been cached the inverse of the matrix is calculated and 
## returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
