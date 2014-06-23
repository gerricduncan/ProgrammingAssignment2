## These two functions return cached matrix inversions

## This function saves the matrix class functions and data
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverted <- function(solve) m <<- solve
    getinverted <- function() m
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}

## This function retrieves the matrix inverse from the cached object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}

