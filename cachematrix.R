## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix x as an input, and specifices a list of functions  
##to be used for inverting the matrix and the name of the inverted matrix to be chached t

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinv <- function(solve) t <<- solve
  getinv <- function() t
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## This function takes the list output of makeCacheMatrix as an input and produces a matrix that is 
## the inverse of x either by retreiving the cached inverse matrix t or, if it was empty, computing
## it anew and caching it in t for the next calculation

cacheSolve <- function(x, ...) {
  t <- x$getinv()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinv(t)
  t
}
