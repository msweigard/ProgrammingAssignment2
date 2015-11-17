
## makeCacheMatrix creates a list containing a set of functions to set and get 
## a matrix or its inverse.  cacheSolve will check if a variable has a value,
## retrieving it if it does, and sets it if the variable is null.

## makeCacheMatrix accepts a matrix as an argument, and creates a list containing
## get and set functions for its intial or inverse states.  When set or setInverse is
## called, a variable accessable in the global environment (an instance of 
## makeCacheMatrix) is defined.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }


## cacheSolve accepts the list of functions created in makeCacheMatrix, sets a
## variable to the value of the getInverse function of this list.  If the value
## is not null, it is returned.  If the value is null, the matrix is retrieved,
## and its inverse is set to a variable.  The inverse value is then passed back
## to the list object for caching.  The inverse is then printed.

cacheSolve <- function(x, ...) {
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
