## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL;
  set<-function(newM){
    x <<- newM
    inv <<- NULL  
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
