## Write a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
	  ## set local matrix cache mxcache to NULL
    mxcache <- NULL
    
    ## set matrix x in parent environment to matrix y (setmx param.)
    ## set matrix cache mxcache in parent environment to NULL
    setmx	<- function(y) {
      x <<- y
      mxcache <<- NULL}
    
    ## get matrix from matrix x (makeCacheMatrix param.)
    getmx <- function(){
      x}
    
    ## set 'mxcache' in parent environment to matrix inverse 'mxinvers'
    setinvers <- function(mxinvers){
      mxcache <<- mxinvers}
    
    ## get matrix inverse from matrix cache mxcache
    getinvers <- function(){
      mxcache}
    
    list(setmx = setmx, getmx = getmx, setinvers = setinvers, getinvers = getinvers)
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mxcache  <- x$getinvers()
    if(!is.null(mxcache))
    {
      message("Getting cached data!!")
      return(mxcache)
    }
    x$setinvers(solve(x$getmx(), ...))
    message("Calculate the inverse matrix!!")
    ##mxcache
    x$getinvers()
  }
  
  
  
