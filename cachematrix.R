#############################################################################################
## Title : Caching the inverse of Matrix                                                  ##
##    Name        Date              Version         Description                           ##
##    KS          24/072015           0.1             Initial                             ##
#############################################################################################
## Description:
## Matrix inversion could be very resource intensive and hence caching of result could
## improve performace. As part of the assignment two functions makeCacheMatrix and cacheSolve
## are created. cacheSolve call function makeCacheMatrix and should return result from
## cache if result is already stored.


## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
        
}
