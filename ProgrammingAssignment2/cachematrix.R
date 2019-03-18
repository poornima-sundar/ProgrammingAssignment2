## The assignment is to write a pair of functions that cache the inverse of a matrix.

#Matrix to catch Inverse
makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)
  {
    inv <<- inverse
  }
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Matrix to compute Inverse
cacheSolve <- function(x,...)
{
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting catched data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
