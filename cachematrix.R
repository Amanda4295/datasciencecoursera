## The purpose of these functions is for the user to be able to cache the inverse of a matrix 

## The function 'makeCacheMatrix' below generates a matrix object that allows the user to cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function 'cacheSolve' calculates/retrieves the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}

## Using the functions:
## test <- matrix(rnorm(4), nrow=2, ncol=2)
## print(test)
## test_C <- makeCacheMatrix(test)
## cacheSolve(test_C)
## solve(test)

