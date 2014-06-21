## This function creates a special "matrix", which is really a list 
## containing a function to 
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of its inverse
##  4.get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##  set the value of the matrix
  set <- function(y) {
    x <<- y
  }
  
  ##  get the value of the matrix
  get <- function() {
    x
  }
  
  ##  set the value of the inverse of the matrix
  setinv <- function(y) {
    inv <<- y
  }
  
  ##  get the value of the inverse of the matrix
  getinv <- function() {
    inv
  }
  
  ## return a list of the functions created
  return(list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## NOTE: Computing the inverse of a square matrix can be done with the 
  ## solve function in R. For example, if X is a square invertible matrix, 
  ## then solve(X) returns its inverse.
  ## For this assignment we assume that the matrix supplied is always invertible.
  
  ## try to get the inverse of the matrix stored in x
  inv <- x$getinv()
  
  ## if not null, it means the inverse is already cached, so return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if not cached, calculate the inverse (using solve())
  data <- x$get()
  inv <- solve(data, ...)
  
  ## cache the calculated value and return it
  x$setinv(inv)
  return(inv)
}
