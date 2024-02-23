## Set the value of the matrix
## Get the value of the matrix
## Determine the inverse of the matrix
## cache the inverse of the matrix

## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special matrix. It first
## checks if the inverse is already calculated. If so, it gets the inverse
## from the cache and skips the computation. Else, it calculates
## the inverse and sets the value of the inverse in the cache via the 
## setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m 
}
