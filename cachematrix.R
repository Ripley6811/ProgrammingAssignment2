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
  ## Function for setting the stored matrix.
  set = function(y){
    x <<- y
    ## Reset the cached inverse.
    i <<- NULL
  }
  ## Get the matrix 'x'
  get = function() x
  ## Store the inverse in this scope.
  setinverse = function(inverse) i <<- inverse
  ## Retrieve the cached inverse from this scope.
  getinverse = function() i
  ## Return a list of functions for accessing this scope.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix
## passed as a parameter.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Cache the inverse within the object 'x'
  
  ## Get cached inverse if it exists.
  i = x$getinverse()
  ## If inverse already exists then return it and exit.
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## Compute inverse of 'x'
  data = x$get()
  i = solve(data, ...)
  ## Cache the inverse in the 'x' object.
  x$setinverse(i)
  ## Return the inverse.
  i
}
