## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## functions do

## Write a short comment describing this function


## Create new Function name makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {

  ## Set the m value to null
  m <- NULL

  ## Reassign the cache value to null, 
  ## it's will be done every time the value change
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x

  ## Inverse the given matrix
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function

## Create new Function name CacheSolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Verify either the matrix already in cache
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Retrive the matrix to inverse
  data <- x$get()
  
  ## Inverse matrix
  m <- solve(data, ...)
  x$setsolve(m)
  
  ## Return m value
  m
}
