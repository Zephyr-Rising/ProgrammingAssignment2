## These functions first cache the matrix for inverting, then solve the inverse of the matrix 
## If the inverse has already been found, the function will just get the cached value and return it instead.

## This function caches the matrix.  
##In addition, if the cacheSolve function has already been run, this function will save it for later use.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function () inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
  
}


## This function solves the inverse for the cached matrix from
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Finding cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
