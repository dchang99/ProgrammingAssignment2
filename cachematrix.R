## Class: CacheMatrix
## ------------------
## Matrix inversion is usually a costly computation. This class
## allows caching of the inverse of a matrix rather than computing
## it repeatedly.

## Function: makeCacheMatrix
## -------------------------
## Creates an object that can store a matrix 'x' and cache its inverse.
##      set: Set stored matrix
##      get: Retrieve stored matrix
##      setInverse: Set cached inverse of stored matrix
##      getInverse: Get cached inverse of stored matrix

makeCacheMatrix <- function(x = matrix()) {
         x_inverse <- NULL
         
         set <- function(y) {
                 x <<- y
                 x_inverse <<- NULL
         }

         get <- function() x
         
         setInverse <- function(inverse) x_inverse <<- inverse
         
         getInverse <- function() x_inverse
         
         list(set = set, get = get, 
              setInverse = setInverse, getInverse = getInverse)
}


## Function: cacheSolve
## --------------------
## Return a matrix that is the inverse of makeCacheMatrix 'x'.
## The inverse is cached after the first time cacheSolve is
## called; subsequent calls return the cached value.

cacheSolve <- function(x, ...) {
        if(is.null(x$get())) return(NULL)
        
        x_inverse <- x$getInverse()
        
        if(is.null(x_inverse)) {
                x_inverse <- solve(x$get())
                x$setInverse(x_inverse)
        }
        
        x_inverse
}
