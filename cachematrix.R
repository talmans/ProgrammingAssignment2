
# Stoner CacheMatrix script: 09-17
##
## functions do
##    makeCacheMatrix()
##      This returns a matrix object with 4 methods
##      Set: Saves the matrix in the global  x and sets its inverse im, global free variable, to null
##      Get: Returns x, the saved matrix
##      setinverse: Sets the gloabl free variable im to the inverse matrix.
##           called by CacheSolve after creating the inverse matrix
##      getinverse: Returns the inverse matrix from the global im
##
##    CacheSolve()
##      Inputs:
##        Takes a cachematrix object:  
##      Purpose: 
##        Returns its inverse from the global im cache
##        If the im cache is empty then the inverse is computed and saved in the cache 
##      
## 

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(mtrx) im <<- mtrx
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
