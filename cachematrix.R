## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) {
    get <- function() X
    setinverse <- function(inv) {
        I <<- inv
    }
    getinverse <- function() I
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve is a function that calculates (or re-calculates) the inverse of an invertible matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
            message("getting cached data")
            print(I)
            return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
        print(I)
}