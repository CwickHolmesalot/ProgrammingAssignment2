## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## This pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
## precondition: assumes matrix provided is invertible
## return value: returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ii <- x$getinverse()
  if(!is.null(ii)) {
    message("getting cached data for inverse")
    return(ii)
  }
  
  # no cached inverse, need to calculate instead
  data <- x$get()
  ii <- solve(data, ...)
  x$setinverse(ii)
  ii
}