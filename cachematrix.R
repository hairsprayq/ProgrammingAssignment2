## To save time we're caching the inverse of a matrix and if the contents of the matrix
## do not change when we need it again we look up in the cache rather than recomputing it.

## makeCacheMatrix creates a special "matrix", which is containing a function to
## set the value of the matrix, get the value of the matrix, set the value of inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the above function
## If cached skips computation otherwise calculates the inverse and stores in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}