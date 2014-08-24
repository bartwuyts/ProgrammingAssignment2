## Functions to cache the inverse of a matrix

## Prepares a cacheable matrix object
## Expected parameter: x = a matrix for which you want to cache the inverse
## Returns : CacheMatrix variable that can be passed to cacheSolve

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


## Inverses a matrix and caches the result. If called multiple times, the cached result is returned.
## Expected parameter: x = a CacheMatrix as created by the makeCacheMatrix function
## Returns : inversed matrix of x

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
