## The 2 functions below compute the inverse of a matrix and cache its value
## For recomputing the value of same matrix, value is first looked up in cache
## If value exists, it is returned from cache else inverse is computed and stored in cache

## makeCacheMatrix creates a special matrix object for caching matrix inverse
## Returns a list of fns to set and get values of matrix and its inverse respectively
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns inverse of matrix. If inverse is already solved, 
## value is returned from cache. Else inverse is computed and stored in cache

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
