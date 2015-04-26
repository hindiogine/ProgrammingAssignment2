## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# It seems that the code below does what it is supposed to do.
# I am not sure what it does.   It has to do with environments,
# closure and lexical scoping, but that is all that I
# understand about this assignment.

makeCacheMatrix <- function(x = matrix())
{
  # This function creates a special "matrix" object
  # that can cache its inverse.
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
