## A function that implements 4 functions (set, get, setMatrix and getMatrix)
## that can be used to store the result of a inversed matrix that has been set

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## This function checks if there is a cached version of the inversed matrix and
## shows it without re-inversing, otherwise inverses the matrix, and store in
## the makeCacheMatrix$setMatrix function, so the next time, it it's the same
## matrix, it will return from the cached version.

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
