## Programming Assignment 2
## There are two functions in this program
## makeCacheMatrix - This function stores a copy of a matrix in a different environment
##                   It further allows for the user to set or get the value of the matrix and
##                      set or get the value of the inverse via the list function

## cacheSolve - This function solves the inverse of a function, but first checks to see if the matrix inverse
##                is already stored in cache.  If it is already in cache, then it skips the calculation and 
##                returns the cached inverse matrix, skipping the computation.  Otherwize, it makes the calculation.

## makeCacheMatrix - a function to set or get the value of the matrix, or set or get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <-function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix, but first checks to see if the inverse is already stored in cache or alternate
##            environment.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}