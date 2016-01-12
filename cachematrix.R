## These two functions will allow caching of a matrix inverse, to allow the 
## user to call that information quickly multiple times
## On a 1000 x 1000 matrix, it speeds up retrieval from about 2.4 seconds 
## to < 0.01

## This function creates the special object that will hold the base data 
## and the cached matrix inverse
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

## This function allows efficient retrieval of the matrix inverse
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
