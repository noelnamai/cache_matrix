## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) { 
    
    inverse <- NULL 
    
    setMatrix <- function(y) {
        matrix <<- y
        inverse <- NULL
    }
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getMatrix <- function() {
        matrix
    }    
    
    getInverse <- function() {
        inverse
    } 
    
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(matrix, ...) { 
    
    theInverse <- matrix$getInverse() 
    
    if(!is.null(theInverse)) {
        message("Getting cached data")
        return(theInverse)
    }   
    
    theMatrix <- matrix$getMatrix()
    
    theInverse <- solve(theMatrix, ...)
    
    matrix$setInverse(theInverse)
    
    theInverse ## Return a matrix that is the inverse of 'x'
}