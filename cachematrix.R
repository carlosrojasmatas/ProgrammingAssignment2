## Put comments here that give an overall description of what your
## functions do

## This function it's a wrapper for a matrix to add caching capabilities regarding the 
## inverse computation. The declared accessors provide access to the underlying object
## and the inverse value. I also added a small function to check if the matrix is invertible.
## I know the assumption of the excercise is that the matrix is always invertible. I just did
## it as a practice for error handling and nested functions.

makeCacheMatrix <- function(x = matrix()) {
  
    
    checkIfInvertible <- function(z) {
      if(!all(is.na(z))){
        thres <- 1e-10
        if(ncol(z) != nrow(z)){
          stop("Matrix non invertible: Non square")
        }
      
        if(abs(det(z)) <= thres || is.na(det(z))){
          stop("Matrix non invertible: Singular, very near to singular or not initialized.")
        }     
      }
    }
    
    ## check if it's invertible at creation time ONLY IF THE MATRIX IS NOT EMPTY.
    checkIfInvertible(x)
    
    inv <- NULL
    
    setValue <- function(y){
      checkIfInvertible(y)
      x <<- y
      inv <<- NULL
    }
    
    getValue <- function() x
    
    setInverse <- function(i) inv <<- i
    
    getInverse <- function() inv
    
    list(setValue=setValue, getValue = getValue,
         setInverse= setInverse,
         getInverse = getInverse)
}


## This small function acts as a "cache manager" for objects returned by the "makeCacheMatrix"
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)){
      message("getting data from cache")
      return(inv)
    }
    x$setInverse(solve(x$getValue()))
    x$getInverse()
}
