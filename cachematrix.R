## ***************************************************** SCRIPT AUTHOR ****************************************************
## Title - Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Author - Naman Gandhi
## Last Modified Date - 10/30/2016
## ************************************************************************************************************************


## **************************************************** SCRIPT PURPOSE ****************************************************
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## Assumption: The matrix supplied is always invertible.
## ************************************************************************************************************************


## ************************************************************************************************************************
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## ************************************************************************************************************************

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(data=NA,nrow=100,ncol=100)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## ************************************************************************************************************************
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## ************************************************************************************************************************

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!all(is.na(m))) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}

## ******************************************************** END ***********************************************************
