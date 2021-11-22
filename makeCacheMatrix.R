## make cache matrix
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# The function "makeCacheMatrix" generates a special "matrix," which is actually
# a list containing functions to set the matrix's value, get the matrix's value,
# set the inverse's value, and get the inverse's value. The inverse of the
# special "matrix" returned by makeCacheMatrix is computed


## cache solve
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
# Return a matrix that is the inverse of 'x'
# If the inverse has been calculated before, cacheSolve should get it from the
# cache


# These two functions are to construct a custom object that holds a matrix and
# caches its inverse
