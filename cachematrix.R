## Matrix inversion can be a costly computation so it might be useful 
## to cache the inverse of a matrix. The following two functions are 
## used to cache the inverse of a matrix.

## makeCacheMatrix creates a list that contains:
## 4 member functions: set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv 
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve returns inverse of matrix. If possible from cache. 
cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message("inverse from cache!")
    return(m) 
  }
  else  {message("compute inverse!")}

  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}


## Test scripts:

## test <- matrix(runif(4,1,10),2,2)
## testCached <- makeCacheMatrix(test)
## cacheSolve(testCached)
## cacheSolve(testCached)


