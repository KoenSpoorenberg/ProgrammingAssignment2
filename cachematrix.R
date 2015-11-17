## Matrix inversion can be a costly computation so it might be useful 
## to cache the result of the computation. 
## The following two functions combined can be used to cache/return 
## the inverse of a matrix.

## makeCacheMatrix creates a list that contains 4 member functions: 
##  set         create matrix
##  get         get matrix
##  setInverse  invert matrix and cache
##  getInverse  get matrix from cache

## cacheSolve uses these functions to get/set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL 
  
  set <- function(y) {
    x <<- y
    xinverse <<- NULL 
  }
  get <- function() x 
  setInverse <- function(inverse) xinverse <<- inverse
  getInverse <- function() xinverse 
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns inverse of matrix. if possible from cache. 
cacheSolve <- function(x, ...) {
  m <- x$getInverse() 
  
  if(!is.null(m)) { 
    message("return inverse from cache!")
    return(m) 
  }
  else  {
    message("return calculated inverse!")
    data <- x$get() 
    m <- solve(data) 
    x$setInverse(m) 
    m 
  }


}

## Test scripts:

## source("cachematrix.R")                    load program
## testCached <- makeCacheMatrix(test)        create functions
## testCached$set(matrix(runif(4,1,10),2,2))  create matrix 

## cacheSolve(testCached)                     returns calculated inverted matrix
## cacheSolve(testCached)                     return inverted matrix from cache

