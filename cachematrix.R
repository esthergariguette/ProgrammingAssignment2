## The first function makeCacheMatrix creates a matrix object that can cash its inverse.
## The second function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix; if the inverse has already been computed its retrieves it from cache.


makeCacheMatrix <- function(x = numeric()) {    # input x will be a matrix
  y <- NULL                                     #  y is the inverse; reset to Null every time makeCacheMatrix is called
  
  get <- function() { x }                       # returns the value of the original matrix
  
  setsolve <- function(solve)  { y <<- solve }  # called by cacheSolve during first access; stores the inverse
  
  getsolve <- function() { y }                  # returns the cached inverse to cacheSolve() when accessed later
  
  list(get = get,             
       setsolve = setsolve,  
       getsolve = getsolve)  
  
}

cacheSolve <- function(x, ...) {      # input x is an object created by makeCacheMatrix
  y <- x$getsolve()                   # accesses the object 'x' and gets the inverse
  if(!is.null(y)) {                   # if inverse was already cached...
    
    message("getting cached data")  
    return(y)                       # it returns the cached inverse
    
  }
  data <- x$get()         # if inverse is not cached...
  y <- solve(data, ...)   # the inverse is computed
  x$setsolve(y)           # and the computed inverse is stored in x
  y                       # return the inverse 
}