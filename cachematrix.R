## makeCacheMatrix creates object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
    I <- NULL
    # set is a function that changes the matrix X stored in the main function
    set <- function (Y) {
        X <<- Y
        I <<- NULL
    }
    # get is a function that returns the matrix X in the main function
    get <- function() X
    # setinverse stores the value of the input from line 29 into the variable I
    setinverse <- function(inv) I <<- inv
    # getinverse returns the value I
    getinverse <- function() I
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve(x) is a function that calculates the inverse of an invertible matrix, but first checks to see if the inverse has already been calculated
cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        # checks to see if there is a matrix stored with getinverse (not NULL), if it exits a message and value I is returned
        if(!is.null(I)) {
            message("getting cached data")
            return(I)
        }
        data <- x$get()
        ## solve returns a matrix that is the inverse of 'x'
        I <- solve(data, ...)
        # stores the matrix I it the object assigned with makeCacheMatrix
        x$setinverse(I)
        I
}