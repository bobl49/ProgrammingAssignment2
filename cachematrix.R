## These functions make an object which is a cacheable form of 
## a matrix and its inverse. To use it, first call
## invX<-makeCacheMatrix(x)
## invX$set(x)
## Then to retrieve the inverse, call
## cacheSolve(xInv)
## to see x call
## inv$get()

## makeCacheMatrix creates a cacheable form of the matrix x and
## its inverse m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes as arguments an object x 
##and other ... arguments for the solve() functioncreated by the makeCacheMatrix
## function and returns its inverse, either from cache or computed
## directly. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
