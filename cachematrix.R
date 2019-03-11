

makeCacheMatrix <- function(x = matrix())
  ## This function creates a special "matrix" object that can cache its inverse.
  ## it supports setting matrix, getting matrix, setting inverse and getting its inverse
{
   inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }



## This function will compute the inverse of the special "matrix" 
## returned by CacheMatrix above. If the inverse has already
## been calculated, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
