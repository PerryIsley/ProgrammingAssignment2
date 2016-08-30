## https://github.com/PerryIsley/ProgrammingAssignment2
## Commit 13: a4474b62f4760b2939407b96d521faa59c07db20
## This funition creates a certain matrix that has the ability to set and get the matrix,
## and then set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function actually finds the inverse of the matrix made by makeCacheMatrix,
## only after it checks for an inverse previously calculated and cached. If this function
## must still calculate it, the inverse is then set as the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
