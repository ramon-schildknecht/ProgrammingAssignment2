## by Ramon Schildknecht
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## assumption: Input is a 2 x 2 Matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ## define set function
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## define get function
  get <- function() x
  
  ## define setsolve function for inversion
  setsolve <- function(solve) s <<- solve
  
  ## define getsolve functtion for inversion
  getsolve <- function() s
  
  ## create list for using functions with $
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## solve special matrix to inverse matrix
  ## Return a matrix that is the inverse of 'x'
  
  ## inverse matrix
  s <- x$getsolve()
  
  ## check if result is already computed
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  ## set new inversed matrix
  x$setsolve(s)
  s
  
  
}
