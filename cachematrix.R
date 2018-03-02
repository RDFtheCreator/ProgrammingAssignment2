## The functions below are used to create a matrix, cache it and its inverse into memory

## makeCacheMatrix() creates a cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function checks whether or not a Cached Matrix has a cached inverse.
## If not, the inverse of the matrix is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
