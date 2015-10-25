## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
## The first function creates a vector which is list of four functions. It creates the following four functions: set(), get(), setinverse(), getinverse().
## set() sets the value of the matrix
## get() gets the value of the matrix
## setinverse() sets the value of the matrix's inverse
## getinverse() gets the value of the matrix's inverse

makeCacheMatrix <- function(targetedmatrix = matrix()) {
  m <- NULL
  set <- function(newtargetedmatrix) {
    targetedmatrix <<- newtargetedmatrix
    m <<- NULL
  }
  get <- function() targetedmatrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the matrix created by the mackeCacheMatrix. 
## However, cacheSolve first checks to see if the inverse has already been calculated (by using the setinverse function from the makeCacheMatrix function). 
## If the inverse has already been calculated, cacheSolve gets the inverse from the cache and skips the computation. 
## If the inverse has not been calculated, cacheSolve calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse() function.

cacheSolve <- function(targetedmatrix, ...) {
  m <- targetedmatrix$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- targetedmatrix$get()
  m <- solve(data, ...)
  targetedmatrix$setinverse(m)
  m
}