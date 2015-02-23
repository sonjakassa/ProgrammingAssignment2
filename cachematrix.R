## The makeCacheMatrix function caches the inverse of a special matrix and 
##the cacheSolve function computes the inverse of the special matrix.


## The makeCacheMatrix function creates a special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.
## Instead of recomputing the inverse it is cached and called.

cacheSolve <- function(x, ...) {
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