## The functions allow caching of a matrix's inverse matrix, 
## so that it only needs to be calculated once.

## Creates an object that holds a matrix x and its inverse matrix m.
## If the inverse matrix has not been calculated, the value is NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Get/Set functions for matrix x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Get/Set functions for the inverse of matrix x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  ## Create list that holds the get/set functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates and returns the inverse of 'x'.  
## If it is already cached, the cached version is used instead of re-calculating.

cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  
  ## Checks if the inverse is already calculated
  ## Uses cached version if that is the case
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Calculates the inverse since it is not cached
  data <- x$get()
  m <- solve(data, ...)
  
  #Save inverse for future use
  x$setinverse(m)
  m
}
