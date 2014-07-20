## Functions for creating a matrix storage object
## and for solving and caching the inverse.
## Simple variation on the "makeVector" example in 
## the assignment description.

## This function creates an object that stores a matrix
## and can also cache the inverse.
## The cached inverse is set to NULL if the matrix
## is reset.

makeCacheMatrix <- function(x = matrix()) {
  
  i = NULL
  set = function(y){
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinverse = function(inverse) i <<- inverse
  getinverse = function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix
## passed as a parameter.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Cache the inverse within the object 'x'
  
  i = x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data = x$get()
  i = solve(data, ...)
  x$setinverse(i)
  i
}
