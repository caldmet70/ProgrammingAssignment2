getwd()
setwd("~/Desktop/Books/LearningProgCode/DataScience/DataScienceSpecializationJHU/R_Programming/week3/ProgrammingAssignment2")

## Matrix inversion is usually a costly computation, there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. In this assignment we need to write
## a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse. Steps involved
## are (1) set the value of the matrix; (2) get the value of the matrix; (3) set the value of
## inverse of the matrix; (4) get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix is same/unchanged), then the
## cacheSolve should retrive the inverse from the cache. Otherwise, cacheSolve computes the
## inverse, sets the value in the cache using setinverse function.
## Here we assume that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
