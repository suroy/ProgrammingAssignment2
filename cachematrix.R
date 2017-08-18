## These two functions create a special object that store a matrix and caches its inverse
## to be retrieved if there is matrix does not change 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Solve) inv <<- Solve
  getInverse <- function () inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## and retrieve the inverse from the cache if the matrix has not changed

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse (inv)
  inv
}