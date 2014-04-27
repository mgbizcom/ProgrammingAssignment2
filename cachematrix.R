## The following two functions (makeCacheMatrix, cacheSolve)
## work together to cache the results of on otherwise expensive
## operation, namely getting the inverse of a matrix.
##
## The first function, makeCacheMatrix, serves as a helper 
## function to the second function, cacheSolve. More detailed
## descriptions are given below for each function.

## -------------------------------------------------------
## FUNCTION: makeCacheMatrix
## -------------------------------------------------------
## Thisfunction creates a special "vector", 
## which is really a list containing functions to
##  - set the value of the vector
##  - get the value of the vector
##  - set the value of the inverse matrix
##  - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## -------------------------------------------------------
## FUNCTION: cacheSolve
## -------------------------------------------------------
## This function will return the inverse of the matrix
## passed as a parameter. It will first check to see if
## the inverse has been cached. If so, it will return the
## cached matrix. Otherwise, it will compute the inverse 
## of the matrix, cache the result and return the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
