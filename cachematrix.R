## Put comments here that give an overall description of what your
## functions do

## function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) minv <<- inv
        getinverse <- function() minv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data.")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv
}
