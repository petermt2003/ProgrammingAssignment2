## The result of the two functions is to return the inverse
## of a square matrix and then cache the result for future use.


## Creates a matrix object to act as cache for the inverse,
## also creates list of parts of function which: set the matrix,
## get the matrix; set the value of the inverse matrix; get the
## value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Checks whether an inverse matrix has already been calculated by
## makeCacheMatrix, and if it has returns this; otherwise it
## calculates the value of the inverse matrix and returns this
## instead.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  }
