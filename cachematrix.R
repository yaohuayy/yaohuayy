## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ii <- NULL
  set <- function(y) {
    x <<- y
    ii <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ii <<- inverse
  getinverse <- function() ii
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ii <- x$getinverse()
  if(!is.null(ii)) {
    message("getting cached data")
    return (ii)
  }
  data <- x$get()
  ii <- solve(data)
  x$setinverse(ii)
  ii
}