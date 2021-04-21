#R Programming - Programming Assignment 2 
#writing 2 functions:
#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set matrix
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Get matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## To Set inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## To get inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
