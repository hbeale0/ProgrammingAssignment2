## cachematrix.R
##
## Contains two functions that can be used to calculate the inverse of a matrix. 
## These functions use the lexical scoping features of R to cache the result of an 
## inverse calculation so that the inverse of a matrix does not have to be calculated
## repeatedly

## makeCacheMatrix
## given a matrix value x, stores the value of x in its environment and returns a list 
## object containing functions for getting and setting the value of x and the inverse of
## x, inv which is also stored in the environment

makeCacheMatrix <- function(x = matrix()) {
  ## validate the input
  if(!is.matrix(x)){
    x<-NULL;
    warning("x is not a matrix, so setting x to null")
  }
  
  ## inv holds the cached inverse value, initially NULL
  inv <- NULL;
  
  ## set a new value for the stored matrix m
  set<-function(xn){
    ## check type of xn
    if(!is.matrix(xn)){
      warning("new value is not a matrix, so won't apply it")
      return;
    }
    
    ## check to see it the new value, xn is the same as the current value of x
    ## if not there is no point reseting the cached inverse
    iden <- is.matrix(x) && is.matrix(xn) && dim(x) == dim(xn) && all(x == xn)
    
    if(!iden){
        ## set the value of x in the parent environment
        ## using the <<- operator
        x <<- xn
        ## reset the value of inv in the parent environment 
        inv <<- NULL   
    } else {
      message("new matrix value is identical, so not reseting")
    }
  }
  
  ## return the value of x from the parent environment
  get <- function() x
  
  ## set the cached inverse value, inv in the parent environment
  setInverse <- function(inverse) inv <<- inverse
  
  ## return the cached inverse value from the parent environment 
  getInverse <- function() inv
  
  ## return the list of getter and setter functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve 
## takes the output of makeCacheMatrix, x and returns the inverse of the 
## matrix stored in x. If the inverse has already been calculated and cached on x, 
## it returns the cached result. Otherwise, it calculates the inverse, sets/caches it 
## on x and returns the result

cacheSolve <- function(x, ...) {
  ## get the inverse from x
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    ## if the inverse is not null, 
    ## it has already been cached on x, 
    ## so return the inverse
    message("getting cached data")
    return(inv)
  }
  
  ## othersise calculate the inverse
  ## and cache it on x useing x's setInverse function
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  
  ## return the result
  inv
}
