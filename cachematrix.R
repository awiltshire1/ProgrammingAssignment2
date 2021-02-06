## These functions work in collaboration to cache the results of a 
## matrix inversion computation. If the inversion has already been 
## computed, the function returns the cached result rather than repeating
## the computation again.

## makeCacheMatrix: this function creates a special matrix object from 
## matrix x, which also contains a list with functions to be used
## in the caching procedure

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: this function computes the inverse of a matrix x (created by
## the makeCacheMatrix function above). If the inverse has previously
## been calculated, it returns the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
